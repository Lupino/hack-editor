{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Proc
  ( runProc
  ) where

import           Data.Text (Text, fromString, pack)
import           FFI       (ffi)
import           FilePath  (FilePath, (</>))
import           FPromise  (Promise, Reject, Resolve, catch, fromResolve,
                            newPromise, resolve, then_, toReject, toResolve)
import           HTTP      (post, resolveText)
import           Prelude
import           RFile     (readFile, saveFile)
import           Utils     (isNodeFile, isPythonFile)


runProc :: FilePath -> [Text] -> Fay Promise
runProc fn args = post uri (Just . pack $ show args) >>= then_ resolveText
  where uri = if isPythonFile fn then "/api/python" </> fn
              else if isNodeFile fn then "/api/node" </> fn
              else "/api/bash" </> fn
