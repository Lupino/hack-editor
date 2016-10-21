{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module RFile
  (
    saveFile,
    readFile,
    deleteFile
  ) where

import Prelude
import Data.Text (fromString, Text)
import FilePath ((</>), FilePath)
import HTTP (get, put, delete, resolveText)
import FPromise (then_, Promise)

saveFile :: FilePath -> Text -> Fay Promise
saveFile fn body = put url (Just body) >>= then_ resolveText
  where url = "/api/file" </> fn

readFile :: FilePath -> Fay Promise
readFile fn = get url >>= then_ resolveText
  where url = "/api/file" </> fn

deleteFile :: FilePath -> Fay Promise
deleteFile fn = delete ur >>= then_ resolveText
  where ur = "/api/file" </> fn
