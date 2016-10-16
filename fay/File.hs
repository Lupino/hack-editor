{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module File
  (
    saveFile,
    readFile,
    deleteFile
  ) where

import Prelude
import Data.Text (fromString, Text)
import FilePath ((</>), FilePath)
import HTTP (get, put, delete, toHandler)

saveFile :: FilePath -> Text -> (Either Text Text -> Fay ()) -> Fay ()
saveFile fn body act = put url body handler
  where handler = toHandler act
        url = "/api/file" </> fn

readFile :: FilePath -> (Either Text Text -> Fay ()) -> Fay ()
readFile fn act = get url handler
  where handler = toHandler act
        url = "/api/file" </> fn

deleteFile :: FilePath -> (Either Text Text -> Fay ()) -> Fay ()
deleteFile fn act = delete ur handler
  where handler = toHandler act
        ur = "/api/file" </> fn
