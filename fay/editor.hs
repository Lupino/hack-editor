{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Prelude hiding (null, concat, putStrLn, lines, unlines)
import qualified Prelude (null)
import FFI (ffi)
import DOM (getElementById, Event, Element, Timer, addClass, setTimeout,
            clearTimeout, removeClass)
import Data.Text (fromString, Text, null, putStrLn, (<>), splitOn, concat)
import FilePath ((</>), dropFileName, FilePath)
import HTTP (get, put, resolveText)

import FPromise (then_, catch, toResolve, toReject)
import RFile (readFile, saveFile, deleteFile)

import Data.Maybe (fromJust, isJust)

import qualified Control.Exception (catch)

import Config
import Proc
import DOMUtils
import Utils
import ACEditor


data SaveState = Saved | Saving | Unsave

setSaveState :: SaveState -> Fay ()
setSaveState = ffi "(function (state) { window.saveState = state['instance']})(%1)"

getSaveState :: Fay SaveState
getSaveState = ffi "{ instance: window.saveState }"

setTimer :: Timer -> Fay ()
setTimer = ffi "(function (t) { window.saveTimeout = t; })(%1)"

getTimer :: Fay Timer
getTimer = ffi "window.saveTimeout"

setAutoSave :: Bool -> Fay ()
setAutoSave = ffi "(function(autosave) { window.autosave = autosave; }) (%1)"

getAutoSave :: Fay Bool
getAutoSave = ffi "window.autosave"

saveBtn :: Fay Element
saveBtn = getElementById "save"

saved :: Fay ()
saved = do
  st <- getSaveState
  case st of
    Saving -> do
      saveBtn >>= setHtml "已保存"
      setSaveState Saved
    _ -> return ()

saving :: Fay ()
saving = do
  saveBtn >>= setProp "disabled" "disabled" >>= setHtml "保存..."
  setSaveState Saving

unsaved :: Fay ()
unsaved = do
  setSaveState Unsave
  saveBtn >>= removeProp "disabled" >>= setHtml "保存"

  autosave <- getAutoSave
  when autosave $ do
    t <- getTimer
    clearTimeout t
    setTimeout 1000 save
    return ()
  where save :: Timer -> Fay ()
        save t = do
          setTimer t
          saveCurrent

getCurrentPath :: Fay FilePath
getCurrentPath = ffi "window.currentPath"

setCurrentPath :: FilePath -> Fay ()
setCurrentPath = ffi "(function(p){window.currentPath = p })(%1)"

getCurrentDirectory :: Fay FilePath
getCurrentDirectory = ffi "window.currentDirectory"

setCurrentDirectory :: FilePath -> Fay ()
setCurrentDirectory = ffi "(function(p){window.currentDirectory = p })(%1)"

saveErrorElem :: Fay Element
saveErrorElem = getElementById "save-error"

isUnsave :: SaveState -> Bool
isUnsave Unsave = True
isUnsave _ = False

saveCurrent :: Fay ()
saveCurrent = do
  saveErrorElem >>= setHtml ""
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
                >>= then_ (toResolve $ const updateTree)
                >>= catch (toReject putStrLn)

data TreeNode = TreeNode { isDir :: Bool, serverPath :: Text, text :: Text }

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
                          >>= then_ (toResolve (flip initTree act))

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
    getElementById "editor" >>= flip removeClass "uninitialized"
    setIsEditorInitialized
    getEditor

doResolveReadFile :: FilePath -> Text -> Fay ()
doResolveReadFile fn body = do
  initEditor
           >>= removeAllEvent "change"
           >>= setValue body
           >>= setMode (getMode fn)
  if (isPythonFile fn || isNodeFile fn) then getElementById "run" >>= removeProp "disabled"
  else getElementById "run" >>= setProp "disabled" "disabled"

  when (isTextFile fn) $ void $ getEditor >>= addEvent "change" (const unsaved)

showCurrentPath :: FilePath -> Fay ()
showCurrentPath path = do
  getElementById "currentPath" >>= setHtml path
  return ()

treeNodeAction :: TreeNode -> Fay ()
treeNodeAction tn = do
  showCurrentPath currentPath
  setCurrentPath currentPath
  setCurrentDirectory currentDirectory
  unless (isDir tn) $
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

startProc_ :: [Text] -> Fay ()
startProc_ cmds = void $ startProc (map toConfFile cmds)

toConfFile :: Text -> FilePath
toConfFile cmd = "/proc" </> cmd <> ".conf"

toPidFile :: Text -> FilePath
toPidFile cmd = "/run" </> cmd <> ".pid"

stopProc_ :: [Text] -> Fay ()
stopProc_ cmds = void $ stopProc (map toConfFile cmds)

runProcAndShow :: FilePath -> [Text] -> Fay ()
runProcAndShow fn args = void  $ runProc fn args
                                    >>= then_ (toResolve showResult)
                                    >>= catch (toReject showResult)
  where showResult :: Text -> Fay ()
        showResult txt = do
          getElementById "proc-result-message" >>= setHtml txt
          getModal "#proc-result" >>= showModal

runCurrentFile :: Fay ()
runCurrentFile = do
  currentPath <- getCurrentPath
  runProcAndShow currentPath []

showTools :: Fay ()
showTools = getModal "#tools" >>= showModal

showProc :: Fay ()
showProc = getModal "#proc" >>= showModal

getProcTarget :: Event -> Fay [Text]
getProcTarget evt = do
  procs <- getEventTargetAttr "data-proc" evt
  return $ splitOn ',' procs

getProcPrompt :: Event -> Fay Text
getProcPrompt = getEventTargetAttr "data-prompt"

getModalTool :: [Tool] -> Event -> Fay (Maybe Tool)
getModalTool tools ev = do
  toolId <- getEventTargetAttr "data-tool-id" ev
  return $ getToolByID toolId tools

getProcFile :: Event -> Fay Text
getProcFile = getEventTargetAttr "data-proc"

bindStartProc :: Event -> Fay ()
bindStartProc ev = do
  cmds <- getProcTarget ev
  when (not $ Prelude.null cmds) $ do
    prom <- getProcPrompt ev
    confirm ("确定启动 " <> prom <> " ?") $ startProc_ cmds

bindStopProc :: Event -> Fay ()
bindStopProc ev = do
  cmds <- getProcTarget ev
  when (not $ Prelude.null cmds) $ do
    prom <- getProcPrompt ev
    confirm ("确定关闭 " <> prom <> " ?") $ stopProc_ cmds

bindRestartProc :: Event -> Fay ()
bindRestartProc ev = do
  cmds <- getProcTarget ev
  when (not $ Prelude.null cmds) $ do
    prom <- getProcPrompt ev
    confirm ("确定重启 " <> prom <> " ?") $ void $ killProc (map toPidFile cmds)

bindShowToolModal :: [Tool] -> Event -> Fay ()
bindShowToolModal tools ev = do

  tool <- getModalTool tools ev
  when (isJust tool) $ processTool (fromJust tool)

  where processTool :: Tool -> Fay ()
        processTool tool | not . null $ getToolProcFile tool =
                            runProcAndShow (getToolProcFile tool) (getToolProcArgv tool)
                         | not . null $ getToolModalFile tool =
                            void $ readFile (getToolModalFile tool)
                                       >>= then_ (toResolve doResolve)
                                       >>= catch (toReject putStrLn)
                         | otherwise = return ()

        doResolve :: Text -> Fay ()
        doResolve txt = do
          querySelector "#tool-modal"
              >>= setHtml txt
              >>= clearEventListeners

          querySelector ("#tool-modal .proc")
              >>= addEventListener "click" bindProcModal

          getModal "#tool-modal .uk-modal" >>= showModal

bindProcModal :: Event -> Fay ()
bindProcModal ev = do
  procFile <- getProcFile ev
  when (not $ null procFile) $ do
    elems <- querySelectorAll ("#tool-modal input")
    args <- mapM (getProp "value") elems
    runProcAndShow procFile args

readConfig :: Text -> Config
readConfig = ffi "JSON.parse(%1)"

loadConfig :: (Config -> Fay ()) -> Fay ()
loadConfig done = void $ readFile "/conf/config.json"
                             >>= then_ (toResolve doResolve)
                             >>= catch (toReject (const $ done emptyConfig))
  where doResolve :: Text -> Fay ()
        doResolve txt = Control.Exception.catch (return $ readConfig txt) (const $ return emptyConfig)
                            >>= done

program :: Config -> Fay ()
program config = do

  setAutoSave $ getIsAutoSave config

  when (not . null $ getProcModalStyle config) $
    querySelector "#proc .uk-modal-dialog"
        >>= flip addClass (getProcModalStyle config)
  when (not . null $ getToolModalStyle config) $
    querySelector "#tools .uk-modal-dialog"
        >>= flip addClass (getToolModalStyle config)

  querySelector "#proc .start-group"
      >>= setHtml (concat (map renderProcBtn $ getStartProcList config))
  querySelector "#proc .stop-group"
      >>= setHtml (concat (map renderProcBtn $ getStopProcList config))
  querySelector "#proc .restart-group"
      >>= setHtml (concat (map renderProcBtn $ getRestartProcList config))
  querySelector "#tools .tool-group"
      >>= setHtml (concat (map renderToolBtn $ getToolList config))

  getElementById "new"
      >>= addEventListener "click" newDoc
  getElementById "delete"
      >>= addEventListener "click" deleteDoc
  saveBtn
      >>= addEventListener "click" (const saveCurrent)
  getElementById "upload"
      >>= addEventListener "click" (uploadFile False)
  getElementById "uploadArchive"
      >>= addEventListener "click" (uploadFile True)
  getElementById "showProc"
      >>= addEventListener "click" (const showProc)

  querySelector "#proc .start-group"
      >>= addEventListener "click" bindStartProc
  querySelector "#proc .stop-group"
      >>= addEventListener "click" bindStopProc
  querySelector "#proc .restart-group"
      >>= addEventListener "click" bindRestartProc

  getElementById "tools"
      >>= addEventListener "click" (bindShowToolModal $ getToolList config)

  getElementById "run"
      >>= addEventListener "click" (const runCurrentFile)

  getElementById "showTools"
      >>= addEventListener "click" (const showTools)

  loadTree treeNodeAction

main :: Fay ()
main = do
  loadConfig program
