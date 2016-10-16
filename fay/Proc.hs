{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Proc
  (
    runProc,
    startProc,
    stopProc,
    killProc,
    exec,
    isPythonFile,
    isNodeFile
  ) where

import Prelude
import FFI (ffi)
import Data.Text (fromString, Text, pack)
import HTTP (post, toHandler)
import File (readFile, saveFile)
import FilePath ((</>), FilePath)

isPythonFile :: FilePath -> Bool
isPythonFile = ffi "isPythonFile(%1)"

isNodeFile :: FilePath -> Bool
isNodeFile = ffi "isNodeFile(%1)"

runProc :: FilePath -> [Text] -> (Either Text Text -> Fay ()) -> Fay ()
runProc fn args act = post uri (pack $ show args) (toHandler act)
  where uri = if isPythonFile fn then "/api/python" </> fn
              else "/api/node" </> fn

concatFile :: [FilePath] -> FilePath -> (Either Text Text -> Fay ()) -> Fay ()
concatFile args target act = runProc "/system/concat.py" (target:args) act

reloadAngel :: (Either Text Text -> Fay ()) -> Fay ()
reloadAngel act = runProc "/system/reload_angel.py" [] act

killProc :: [Text] -> (Either Text Text -> Fay ()) -> Fay ()
killProc cmds act = runProc "/system/kill_proc.py" cmds act

exec :: Text -> [Text] -> (Either Text Text -> Fay ()) -> Fay ()
exec cmd args act = runProc "/system/exec.py" (cmd:args) act

startProc :: [Text] -> (Either Text Text -> Fay ()) -> Fay ()
startProc cmds act = angelProc (union cmds) act


stopProc :: [Text] -> (Either Text Text -> Fay ()) -> Fay ()
stopProc cmds act = angelProc (difference cmds) act

angelProc :: ([Text] -> [Text]) -> (Either Text Text -> Fay ()) -> Fay ()
angelProc dealCmd act = readFile "/conf/deploy.json" readFileAction
  where readFileAction :: Either Text Text -> Fay ()
        readFileAction err@(Left _) = act err
        readFileAction (Right txt)  =  concatFileAndSave txt toReloadAngelAction

        concatFileAndSave :: Text -> (Either Text Text -> Fay ()) -> Fay ()
        concatFileAndSave txt act' = concatFile cmds "/conf/deploy.conf" toSaveFile
          where cmds = dealCmd $ readList txt
                toSaveFile :: Either Text Text -> Fay ()
                toSaveFile err@(Left _) = act' err
                toSaveFile (Right _)    = saveFile "/conf/deploy.json" (showList cmds) act'

        toReloadAngelAction :: Either Text Text -> Fay ()
        toReloadAngelAction err@(Left _) = act err
        toReloadAngelAction (Right _)    = reloadAngel act

readList :: Text -> [Text]
readList = ffi "JSON.parse(%1)"

showList :: [Text] -> Text
showList = ffi "JSON.stringify(%1)"

union :: [Text] -> [Text] -> [Text]
union (x:xs) cmds | x `elem` cmds = union xs cmds
                  | otherwise     = x:(union xs cmds)

union [] cmds                     = cmds

difference :: [Text] -> [Text] -> [Text]
difference removed cmds = filter keep cmds
  where keep :: Text -> Bool
        keep txt = txt `notElem` removed
