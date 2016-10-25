{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Proc
  (
    runProc,
    startProc,
    stopProc,
    killProc,
    exec
  ) where

import Prelude
import FFI (ffi)
import Data.Text (fromString, Text, pack)
import HTTP (post, resolveText)
import RFile (readFile, saveFile)
import FilePath ((</>), FilePath)
import FPromise (Promise, newPromise, then_, Resolve, Reject, catch, fromResolve,
                 toResolve, toReject, resolve)
import Utils (isPythonFile)


runProc :: FilePath -> [Text] -> Fay Promise
runProc fn args = post uri (Just . pack $ show args) >>= then_ resolveText
  where uri = if isPythonFile fn then "/api/python" </> fn
              else "/api/node" </> fn

concatFile :: [FilePath] -> FilePath -> Fay Promise
concatFile args target = runProc "/system/concat.py" (target:args)

reloadAngel :: Fay Promise
reloadAngel = runProc "/system/reload_angel.py" []

killProc :: [Text] -> Fay Promise
killProc cmds = runProc "/system/kill_proc.py" cmds

exec :: Text -> [Text] -> Fay Promise
exec cmd args = runProc "/system/exec.py" (cmd:args)

startProc :: [Text] -> Fay Promise
startProc cmds = angelProc (union cmds)


stopProc :: [Text] -> Fay Promise
stopProc cmds = angelProc (difference cmds)

angelProc :: ([Text] -> [Text]) -> Fay Promise
angelProc dealCmd = newPromise doReadConf
                        >>= then_ (toResolve doConcatFile)
                        >>= then_ (toResolve doSaveConf)
                        >>= then_ (toResolve (const $ reloadAngel))
  where doReadConf :: Resolve Text () -> Reject -> Fay ()
        doReadConf f _ = void $ readFile "/conf/deploy.json"
                                    >>= then_ f
                                    >>= catch (toReject (const $ f_ "[]"))
          where f_ = fromResolve f

        doConcatFile :: Text -> Fay Promise
        doConcatFile txt = concatFile cmds "/conf/deploy.conf"
                               >>= then_ (toResolve (const $ resolve cmds))
          where cmds = dealCmd $ readList txt

        doSaveConf :: [Text] -> Fay Promise
        doSaveConf cmds = saveFile "/conf/deploy.json" (showList cmds)


readList :: Text -> [Text]
readList = ffi "(function(txt) { try { return JSON.parse(%1); } catch (e) { return []; }  })(%1)"

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
