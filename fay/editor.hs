{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Main (main) where

import           ACEditor
import           Data.Text   (Text, fromString, null, putStrLn, (<>))
import           DOM         (Element, Event, Timer, addClass, clearTimeout,
                              getElementById, hasClass, removeClass, setTimeout)
import           DOMUtils
import           FFI         (ffi)
import           FilePath    (FilePath, dropFileName, (</>))
import           FPromise    (catch, then_, toReject, toResolve)
import           Prelude     hiding (concat, lines, null, putStrLn, unlines)
import           ProcAPI     (ProcAPI, loadFileTree, newProcAPI, signFilePath)
import qualified ProcAPI     as API (readFile, removeFile, runFile,
                                     uploadArchive, uploadFile, writeFile)
import           TermManager (TermManager, closeTerm, newTermManager, openTerm)
import           Utils       (canProc, getMode, isImage, isTextFile)


data SaveState = Saved | Saving | Unsave

data ScreenMode = EditorMode | TermMode

setSaveState :: SaveState -> Fay ()
setSaveState = ffi "(function (state) { window['saveState'] = state['instance']})(%1)"

getSaveState :: Fay SaveState
getSaveState = ffi "{ instance: window['saveState'] }"

setScreenMode :: ScreenMode -> Fay ()
setScreenMode = ffi "(function (state) { window['screenMode'] = state['instance']})(%1)"

getScreenMode :: Fay ScreenMode
getScreenMode = ffi "{ instance: window['screenMode'] }"

setTimer :: Timer -> Fay ()
setTimer = ffi "(function (t) { window['saveTimeout'] = t; })(%1)"

getTimer :: Fay Timer
getTimer = ffi "window['saveTimeout']"

setAutoSave :: Bool -> Fay ()
setAutoSave = ffi "(function(autosave) { window['autosave'] = autosave; }) (%1)"

getAutoSave :: Fay Bool
getAutoSave = ffi "window['autosave']"

saveBtn :: Fay Element
saveBtn = getElementById "save"

saved :: Fay ()
saved = do
  st <- getSaveState
  case st of
    Saving -> do
      void $ saveBtn >>= setHtml "已保存"
      setSaveState Saved
    _ -> return ()

saving :: Fay ()
saving = do
  void $ saveBtn >>= setProp "disabled" "disabled" >>= setHtml "保存..."
  setSaveState Saving

unsaved :: ProcAPI -> Fay ()
unsaved api = do
  setSaveState Unsave
  void $ saveBtn >>= removeProp "disabled" >>= setHtml "保存"

  autosave <- getAutoSave
  when autosave $ do
    t <- getTimer
    clearTimeout t
    void $ setTimeout 1000 save
  where save :: Timer -> Fay ()
        save t = do
          setTimer t
          saveCurrent api

getCurrentPath :: Fay FilePath
getCurrentPath = ffi "window['currentPath']"

setCurrentPath_ :: FilePath -> Fay ()
setCurrentPath_ = ffi "(function(p){window['currentPath'] = p })(%1)"

setCurrentPath :: Bool -> FilePath -> Fay ()
setCurrentPath isdir path = do
  void $ getElementById "currentPath" >>= setHtml path
  setCurrentPath_ path
  setCurrentDirectory dir

  where dir = if isdir
                then path
                else dropFileName path

getCurrentDirectory :: Fay FilePath
getCurrentDirectory = ffi "window['currentDirectory']"

setCurrentDirectory :: FilePath -> Fay ()
setCurrentDirectory = ffi "(function(p){window['currentDirectory'] = p })(%1)"

isUnsave :: SaveState -> Bool
isUnsave Unsave = True
isUnsave _      = False

saveCurrent :: ProcAPI -> Fay ()
saveCurrent api = do
  currentPath <- getCurrentPath
  saveState <- getSaveState

  when (Prelude.not (null currentPath) && isUnsave saveState && isTextFile currentPath) $ do
    saving
    editor <- getEditor
    dat <- getValue editor
    void $ API.writeFile api currentPath dat
              >>= then_ (toResolve $ const saved)
              >>= catch (toReject $ const $ unsaved api)


newDoc :: ProcAPI -> Event -> Fay ()
newDoc api _ = do
  saveCurrent api
  prompt "输入文件名" "" $ \fn -> do
    currentDirectory <- getCurrentDirectory

    let path = currentDirectory </> fn

    void $ API.writeFile api (fixed path (isTextFile fn)) "\n"
                >>= then_ (toResolve $ const $ updateTree api)
                >>= catch (toReject putStrLn)

  where fixed fn True  = fn
        fixed fn False = fn <> ".md"

deleteDoc :: ProcAPI -> Event -> Fay ()
deleteDoc api _ = do
  currentPath <- getCurrentPath
  unless (null currentPath) $ do
    confirm ("删除 " <> currentPath <> " ?") $
      void $ API.removeFile api currentPath
                >>= then_ (toResolve $ const (updateTree api >> showCurrentPath False "" >> cleanScreen "Deleted"))
                >>= catch (toReject putStrLn)

data TreeNode

isDir :: TreeNode -> Bool
isDir = ffi "%1['isDir']"

serverPath :: TreeNode -> Text
serverPath = ffi "%1['serverPath']"

fileSize :: TreeNode -> Int
fileSize = ffi "%1['size']"

initTree :: Text -> (TreeNode -> Fay ()) -> Fay ()
initTree = ffi "initTree(%1, %2)"

clearTree :: Fay()
clearTree = ffi "clearTree()"

updateTree :: ProcAPI -> Fay ()
updateTree api = do
  clearTree
  loadTree api

loadTree :: ProcAPI -> Fay ()
loadTree api =
  void $ loadFileTree api
    >>= then_ (toResolve (`initTree` treeNodeAction api))


getEditor :: Fay Editor
getEditor = ffi "window['editor']"

setEditor :: Editor -> Fay ()
setEditor = ffi "window['editor'] = %1"

isEditorInitialized :: Fay Bool
isEditorInitialized = ffi "window['editorInitialized']"

setIsEditorInitialized :: Fay ()
setIsEditorInitialized = ffi "window['editorInitialized'] = true"

termElem :: Fay Element
termElem = getElementById "term"

readOnlyElem :: Fay Element
readOnlyElem = getElementById "read-only"

switchScreenBtn :: Fay Element
switchScreenBtn = getElementById "openTerm"

editorElem :: Fay Element
editorElem = getElementById "editor"

sidebarElem :: Fay Element
sidebarElem = getElementById "sidebar"

mainElem :: Fay Element
mainElem = getElementById "main"

menuElem :: Fay Element
menuElem = getElementById "menu"

setShow :: Bool -> Element -> Fay ()
setShow True  = flip removeClass "hide"
setShow False = flip addClass "hide"

showImage :: ProcAPI -> Fay ()
showImage api = do
  setEditorData "text" ""
  signCurrentPath api $ \_ url ->
    readOnlyElem
      >>= setHtml ("<img src='" <> url <> "'>")
      >>= setShow True

showFile :: ProcAPI -> Fay ()
showFile api = do
  currentPath <- getCurrentPath
  readOnlyElem >>= setShow False
  void $ API.readFile api currentPath
            >>= then_ (toResolve $ doResolve currentPath)
            >>= catch (toReject print)
  where doResolve path txt = do
          setEditorData (getMode path) txt
          when (isTextFile path) $ addChangeEvent api

initEditor :: Fay Editor
initEditor = do
  isInitialized <- isEditorInitialized
  if isInitialized then getEditor
  else do
    newEditor "editor" >>= setTheme "chrome" >>= setEditor
    editorElem >>= flip removeClass "uninitialized"
    setIsEditorInitialized
    getEditor

enableElem :: Text -> Bool -> Fay ()
enableElem el True = void $ getElementById el >>= removeProp "disabled"
enableElem el False = void $ getElementById el >>= setProp "disabled" "disabled"

setEditorData :: Text -> Text -> Fay ()
setEditorData mode body = do
  void $ initEditor
           >>= removeAllEvent "change"
           >>= setValue body
           >>= setMode mode

addChangeEvent :: ProcAPI -> Fay ()
addChangeEvent api =
  void $ getEditor >>= addEvent "change" (const $ unsaved api)

showCurrentPath :: Bool -> FilePath -> Fay ()
showCurrentPath isdir path = do
  setCurrentPath isdir path
  enableElem "download" $ not isdir
  enableElem "downloadLink" $ not isdir
  enableElem "delete" $ not $ null path
  enableElem "run" $ canProc path

cleanScreen :: Text -> Fay ()
cleanScreen e = do
  setEditorData "text" e
  readOnlyElem >>= setShow False

treeNodeAction :: ProcAPI -> TreeNode -> Fay ()
treeNodeAction api tn = do
  showCurrentPath (isDir tn) currentPath
  if isDir tn
    then cleanScreen "Directory"
    else
      if isImage currentPath
        then showImage api
        else
         if fileSize tn < 1048576 then showFile api
                                  else cleanScreen "File to large."

  where currentPath = serverPath tn


selectFile :: (Text -> Text -> Fay ()) -> Fay ()
selectFile = ffi "selectFile(%1)"

uploadFile :: ProcAPI -> Bool -> Event -> Fay ()
uploadFile api isArc _ = selectFile action
  where action :: Text -> Text -> Fay ()
        action name dat = do
          currentDirectory <- getCurrentDirectory
          void $ doUpload api (currentDirectory </> name) dat
                     >>= then_ (toResolve $ const $ updateTree api)
                     >>= catch (toReject print)
        doUpload = if isArc then API.uploadArchive else API.uploadFile

runProcAndShow :: ProcAPI -> FilePath -> [Text] -> Fay ()
runProcAndShow api fn args =
  void $ API.runFile api fn args
    >>= then_ (toResolve showResult)
    >>= catch (toReject showResult)

  where showResult :: Text -> Fay ()
        showResult txt = do
          updateTree api
          void $ getElementById "proc-result-message" >>= setHtml txt
          getModal "#proc-result" >>= showModal

runCurrentFile :: ProcAPI -> Fay ()
runCurrentFile api = do
  currentPath <- getCurrentPath
  runProcAndShow api currentPath []

showTerm :: TermManager -> Fay ()
showTerm tm = do
  termElem >>= setShow True
  setScreenMode TermMode
  void $ switchScreenBtn >>= setHtml "编辑器"
  openTerm tm

hideTerm :: ProcAPI -> Fay ()
hideTerm api = do
  termElem >>= setShow False
  setScreenMode EditorMode
  void $ switchScreenBtn >>= setHtml "终端"
  updateTree api

switchScreen :: TermManager -> ProcAPI -> Event -> Fay ()
switchScreen tm api _ = do
  mode <- getScreenMode
  case mode of
    TermMode   -> hideTerm api
    EditorMode -> showTerm tm

signCurrentPath :: ProcAPI -> (Text -> Text -> Fay ()) -> Fay ()
signCurrentPath api next = do
  currentPath <- getCurrentPath
  void $ signFilePath api currentPath
    >>= then_ (toResolve (next currentPath))
    >>= catch (toReject print)

download :: ProcAPI -> Event -> Fay ()
download api _ = do
  signCurrentPath api saveAs

downloadLink :: ProcAPI -> Event -> Fay ()
downloadLink api _ =
  signCurrentPath api $ \_ url ->
    prompt "下载地址" url $ const $ return ()

getKeyFromLocation :: Fay Text
getKeyFromLocation = ffi "(function() {var m = /key=([^&]+)/.exec(location.search); return m && m [1] || '';})()"

getSecret_ :: Maybe Text -> (Text -> Maybe Text) -> Text -> Fay (Maybe Text)
getSecret_ = ffi "(function(nothing, just, key) { var v = localStorage.getItem(key); if (v) {return just(v)} else {return nothing} })(%1, %2, %3)"

getSecret :: Text -> Fay (Maybe Text)
getSecret = getSecret_ Nothing Just

setSecret :: Text -> Text -> Fay ()
setSecret = ffi "localStorage.setItem(%1, %2)"

resetSecret :: Text -> Text -> Fay ()
resetSecret sec = prompt "请输入新密钥" sec . setSecret

prepareSecrect :: Text -> (Text -> Fay ()) -> Fay ()
prepareSecrect "" next  = next ""
prepareSecrect key next = do
  msec <- getSecret key
  case msec of
    Nothing ->
      prompt "请输入密钥" "" $ \sec -> do
        setSecret key sec
        next sec
    Just sec -> next sec

switchSidebar :: TermManager -> Event -> Fay ()
switchSidebar tm _ = do
  el <- sidebarElem
  has <- hasClass el "hide"
  setShow has el

  mel <- mainElem
  if has then removeClass mel "fullscreen"
         else addClass mel "fullscreen"


  mode <- getScreenMode
  case mode of
    TermMode   -> openTerm tm
    EditorMode -> return ()

program :: Text -> Text -> Fay ()
program key sec = do
  api <- newProcAPI key sec
  tm <- newTermManager "#terminal-container" api (hideTerm api)
  setAutoSave True
  setScreenMode EditorMode

  windowAddEventListener "beforeunload" $ const (closeTerm tm)

  void $ getElementById "new"
      >>= addEventListener "click" (newDoc api)
  void $ getElementById "delete"
      >>= addEventListener "click" (deleteDoc api)
  void $ saveBtn
      >>= addEventListener "click" (const $ saveCurrent api)
  void $ getElementById "upload"
      >>= addEventListener "click" (uploadFile api False)
  void $ getElementById "uploadArchive"
      >>= addEventListener "click" (uploadFile api True)
  void $ switchScreenBtn
      >>= addEventListener "click" (switchScreen tm api)

  void $ getElementById "run"
      >>= addEventListener "click" (const $ runCurrentFile api)

  void $ getElementById "download"
      >>= addEventListener "click" (download api)

  void $ getElementById "downloadLink"
      >>= addEventListener "click" (downloadLink api)

  void $ getElementById "resetSecret"
      >>= addEventListener "click" (const $ resetSecret sec key)

  void $ menuElem >>= addEventListener "click" (switchSidebar tm)

  loadTree api

main :: Fay ()
main = do
  key <- getKeyFromLocation
  prepareSecrect key $ program key
