{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Main (main) where

import           ACEditor
import           Data.Text (Text, fromString, null, putStrLn, (<>))
import           DOM       (Element, Event, Timer, clearTimeout, getElementById,
                            removeClass, setTimeout)
import           DOMUtils
import           FFI       (ffi)
import           FilePath  (FilePath, dropFileName, (</>))
import           FPromise  (catch, then_, toReject, toResolve)
import           HTTP      (get, put, resolveText)
import           Prelude   hiding (concat, lines, null, putStrLn, unlines)
import           Proc      (runProc)
import           RFile     (deleteFile, readFile, saveFile)
import           Term      (TermManager, closeTerm, newTermManager, openTerm)
import           Utils     (canProc, getMode, isTextFile)


data SaveState = Saved | Saving | Unsave

setSaveState :: SaveState -> Fay ()
setSaveState = ffi "(function (state) { window['saveState'] = state['instance']})(%1)"

getSaveState :: Fay SaveState
getSaveState = ffi "{ instance: window['saveState'] }"

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

unsaved :: Fay ()
unsaved = do
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
          saveCurrent

getCurrentPath :: Fay FilePath
getCurrentPath = ffi "window['currentPath']"

setCurrentPath :: FilePath -> Fay ()
setCurrentPath = ffi "(function(p){window['currentPath'] = p })(%1)"

getCurrentDirectory :: Fay FilePath
getCurrentDirectory = ffi "window['currentDirectory']"

setCurrentDirectory :: FilePath -> Fay ()
setCurrentDirectory = ffi "(function(p){window['currentDirectory'] = p })(%1)"

isUnsave :: SaveState -> Bool
isUnsave Unsave = True
isUnsave _      = False

saveCurrent :: Fay ()
saveCurrent = do
  currentPath <- getCurrentPath
  saveState <- getSaveState

  when (Prelude.not (null currentPath) && isUnsave saveState && isTextFile currentPath) $ do
    saving
    editor <- getEditor
    dat <- getValue editor
    void $ saveFile currentPath dat
              >>= then_ (toResolve $ const saved)
              >>= catch (toReject $ const unsaved)


newDoc :: Event -> Fay ()
newDoc _ = do
  saveCurrent
  prompt "输入文件名" $ \fn -> do
    currentDirectory <- getCurrentDirectory

    let path = currentDirectory </> fn

    void $ saveFile (fixed path (isTextFile fn)) "\n"
                >>= then_ (toResolve $ const updateTree)
                >>= catch (toReject putStrLn)

  where fixed fn True  = fn
        fixed fn False = fn <> ".md"

deleteDoc :: Event -> Fay ()
deleteDoc _ = do
  currentPath <- getCurrentPath
  unless (null currentPath) $ do
    confirm ("删除 " <> currentPath <> " ?") $
      void $ deleteFile currentPath
                >>= then_ (toResolve $ const (updateTree >> doResolveReadFile "" "" >> showCurrentPath ""))
                >>= catch (toReject putStrLn)

data TreeNode = TreeNode { isDir :: Bool, serverPath :: Text }

initTree :: Text -> (TreeNode -> Fay ()) -> Fay ()
initTree = ffi "initTree(%1, %2)"

clearTree :: Fay()
clearTree = ffi "clearTree()"

updateTree :: Fay ()
updateTree = do
  clearTree
  loadTree treeNodeAction

loadTree :: (TreeNode -> Fay ()) -> Fay ()
loadTree act = void $ get "/api/file"
                          >>= then_ resolveText
                          >>= then_ (toResolve (`initTree` act))

getEditor :: Fay Editor
getEditor = ffi "window['editor']"

setEditor :: Editor -> Fay ()
setEditor = ffi "window['editor'] = %1"

isEditorInitialized :: Fay Bool
isEditorInitialized = ffi "window['editorInitialized']"

setIsEditorInitialized :: Fay ()
setIsEditorInitialized = ffi "window['editorInitialized'] = true"

initEditor :: Fay Editor
initEditor = do
  isInitialized <- isEditorInitialized
  if isInitialized then getEditor
  else do
    newEditor "editor" >>= setTheme "chrome" >>= setEditor
    void $ getElementById "editor" >>= flip removeClass "uninitialized"
    setIsEditorInitialized
    getEditor

doResolveReadFile :: FilePath -> Text -> Fay ()
doResolveReadFile fn body = do
  void $ initEditor
           >>= removeAllEvent "change"
           >>= setValue body
           >>= setMode (getMode fn)
  if canProc fn then void $ getElementById "run" >>= removeProp "disabled"
  else void $ getElementById "run" >>= setProp "disabled" "disabled"

  when (isTextFile fn) $ void $ getEditor >>= addEvent "change" (const unsaved)

showCurrentPath :: FilePath -> Fay ()
showCurrentPath path =
  void $ getElementById "currentPath" >>= setHtml path

treeNodeAction :: TreeNode -> Fay ()
treeNodeAction tn = do
  showCurrentPath currentPath
  setCurrentPath currentPath
  setCurrentDirectory currentDirectory
  doResolveReadFile currentPath ""
  if isDir tn then void $ getElementById "download" >>= setProp "disabled" "disabled"
  else void $ getElementById "download" >>= removeProp "disabled"
  when (not (isDir tn) && isTextFile currentPath) $
    void $ readFile currentPath
              >>= then_ (toResolve $ doResolveReadFile currentPath)
              >>= catch (toReject print)

  where currentPath = serverPath tn
        currentDirectory = if isDir tn then currentPath
                           else dropFileName currentPath

selectFile :: (Text -> Text -> Fay ()) -> Fay ()
selectFile = ffi "selectFile(%1)"

uploadFile :: Bool -> Event -> Fay ()
uploadFile isArc _ = selectFile action
  where action :: Text -> Text -> Fay ()
        action name dat = do
          currentDirectory <- getCurrentDirectory
          void $ put (uri </> currentDirectory </> name) (Just dat)
                     >>= then_ (toResolve $ const updateTree)
        uri = if isArc then "/api/archive" else "/api/file"

runProcAndShow :: FilePath -> [Text] -> Fay ()
runProcAndShow fn args = void  $ runProc fn args
                                    >>= then_ (toResolve showResult)
                                    >>= catch (toReject showResult)
  where showResult :: Text -> Fay ()
        showResult txt = do
          updateTree
          void $ getElementById "proc-result-message" >>= setHtml txt
          getModal "#proc-result" >>= showModal

runCurrentFile :: Fay ()
runCurrentFile = do
  currentPath <- getCurrentPath
  runProcAndShow currentPath []

showTerm :: TermManager -> Event -> Fay ()
showTerm tm _ = do
  getModal "#term" >>= showModal
  openTerm tm

download :: Event -> Fay ()
download _ = do
  currentPath <- getCurrentPath
  saveAs currentPath

program ::  Fay ()
program = do
  setAutoSave True
  tm <- newTermManager =<< getModal "#term"

  modalEvent "#term" (return ()) updateTree

  windowAddEventListener "beforeunload" $ const (closeTerm tm)

  void $ getElementById "new"
      >>= addEventListener "click" newDoc
  void $ getElementById "delete"
      >>= addEventListener "click" deleteDoc
  void $ saveBtn
      >>= addEventListener "click" (const saveCurrent)
  void $ getElementById "upload"
      >>= addEventListener "click" (uploadFile False)
  void $ getElementById "uploadArchive"
      >>= addEventListener "click" (uploadFile True)
  void $ getElementById "openTerm"
      >>= addEventListener "click" (showTerm tm)

  void $ getElementById "run"
      >>= addEventListener "click" (const runCurrentFile)

  void $ getElementById "download"
      >>= addEventListener "click" download

  loadTree treeNodeAction

main :: Fay ()
main = program
