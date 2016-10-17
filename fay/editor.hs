{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Main (main) where

import Prelude hiding (null, concat, putStrLn, lines, unlines)
import qualified Prelude (null)
import FFI (ffi)
import DOM (getElementById, Event, Element, Timer, addClass,
            XMLHttpRequest, setTimeout, clearTimeout, responseText)
import Data.Text (fromString, Text, null, putStrLn, unpack, (<>), splitOn, concat)
import FilePath ((</>), dropFileName, FilePath)
import HTTP (get, put)
import File (readFile, saveFile, deleteFile)

import Data.Maybe (fromJust, isJust)

import Control.Exception (catch)

import Config

import Proc

import DOMUtils


data SaveState = Saved | Saving | Unsave

setSaveState :: SaveState -> Fay ()
setSaveState = ffi "(function (state) { window.saveState = state['instance']})(%1)"

getSaveState :: Fay SaveState
getSaveState = ffi "{ instance: window.saveState }"

setTimer :: Timer -> Fay ()
setTimer = ffi "(function (t) { window.saveTimeout = t; })(%1)"

getTimer :: Fay Timer
getTimer = ffi "window.saveTimeout"

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

isTextFile :: FilePath -> Bool
isTextFile = ffi "isTextFile(%1)"

saveCurrent :: Fay ()
saveCurrent = do
  saveErrorElem >>= setHtml ""
  currentPath <- getCurrentPath
  saveState <- getSaveState

  when (Prelude.not (null currentPath) && isUnsave saveState && isTextFile currentPath) $ do
    saving
    editor <- getEditor
    dat <- getEditorValue editor
    saveFile currentPath dat act

  where act :: Either Text Text -> Fay ()
        act (Left _) = unsaved
        act (Right _) = saved

newDoc :: Event -> Fay ()
newDoc _ = do
  saveCurrent
  prompt "输入文件名" $ \fn -> do
    currentDirectory <- getCurrentDirectory

    let path = currentDirectory </> fn

    put ("/api/file" </> fixed path (isTextFile fn)) "\n" (const updateTree)

  where fixed fn True  = fn
        fixed fn False = fn <> ".md"

deleteDoc :: Event -> Fay ()
deleteDoc _ = do
  currentPath <- getCurrentPath
  unless (null currentPath) $ do
    confirm ("删除 " <> currentPath <> " ?") $ deleteFile currentPath act
  where act :: Either Text Text -> Fay ()
        act (Left err) = putStrLn err
        act (Right _) = updateTree

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
loadTree act = get "/api/file" resolve
  where resolve :: XMLHttpRequest -> Fay ()
        resolve xhr = do
          t <- responseText xhr
          initTree t act

data Editor

getEditor :: Fay Editor
getEditor = initEditor

initEditor :: Fay Editor
initEditor = ffi "initEditor()"

setEditorValue :: Text -> Editor -> Fay Editor
setEditorValue = ffi "(function(value, editor) { editor.setValue(value, -1); return editor })(%1, %2)"

getEditorValue :: Editor -> Fay Text
getEditorValue = ffi "(function(editor) { return editor.getValue() })(%1)"

setEditorMode :: FilePath -> Editor -> Fay Editor
setEditorMode = ffi "setEditorMode(%2, %1)"

setEditorEvent :: Text -> (Event -> Fay ()) -> Editor -> Fay Editor
setEditorEvent = ffi "(function (evt, func, editor) { editor.on(evt, func); return editor; })(%1, %2, %3)"

removeAllEditorEvent :: Text -> Editor -> Fay Editor
removeAllEditorEvent = ffi "(function(evt, editor) { editor.removeAllListeners(evt); return editor; })(%1, %2)"

readFileAction :: FilePath -> Either Text Text -> Fay ()
readFileAction _ (Left err) = error $ unpack err
readFileAction fn (Right body) = do
  getEditor
           >>= removeAllEditorEvent "change"
           >>= setEditorValue body
           >>= setEditorMode fn
  if (isPythonFile fn || isNodeFile fn) then getElementById "run" >>= removeProp "disabled"
  else getElementById "run" >>= setProp "disabled" "disabled"

  when (isTextFile fn) $ void $ getEditor >>= setEditorEvent "change" (const unsaved)

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
    readFile currentPath (readFileAction currentPath)

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
          put (uri </> currentDirectory </> name) dat (const updateTree)
        uri = if isArc then "/api/archive" else "/api/file"

startProc_ :: [Text] -> Fay ()
startProc_ cmds = startProc (map toConfFile cmds) (const $ return ())

toConfFile :: Text -> FilePath
toConfFile cmd = "/proc" </> cmd <> ".conf"

toPidFile :: Text -> FilePath
toPidFile cmd = "/run" </> cmd <> ".pid"

stopProc_ :: [Text] -> Fay ()
stopProc_ cmds = stopProc (map toConfFile cmds) (const $ return ())

runProcAndShow :: FilePath -> [Text] -> Fay ()
runProcAndShow fn args = runProc fn args act
  where act :: Either Text Text -> Fay ()
        act (Left err)  = showResult err
        act (Right txt) = showResult txt

        showResult :: Text -> Fay ()
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
    confirm ("确定重启 " <> prom <> " ?") $ killProc (map toPidFile cmds) (const $ return ())

bindShowToolModal :: [Tool] -> Event -> Fay ()
bindShowToolModal tools ev = do

  tool <- getModalTool tools ev
  when (isJust tool) $ processTool (fromJust tool)

  where processTool :: Tool -> Fay ()
        processTool tool | not . null $ getToolProcFile tool =
                            runProcAndShow (getToolProcFile tool) (getToolProcArgv tool)
                         | not . null $ getToolModalFile tool =
                            readFile (getToolModalFile tool) act
                         | otherwise = return ()

        act :: Either Text Text -> Fay ()
        act (Left err)  = error $ unpack err
        act (Right txt) = do
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
loadConfig done = readFile "/conf/config.json" act
  where act :: Either Text Text -> Fay ()
        act (Left _) = done emptyConfig
        act (Right txt) = catch (return $ readConfig txt) (const $ return emptyConfig)
                              >>= done

program :: Config -> Fay ()
program config = do

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
