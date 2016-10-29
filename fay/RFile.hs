{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module RFile
  (
    saveFile,
    readFile,
    deleteFile
  ) where

import           Data.Text (Text, fromString, (<>))
import           FFI       (ffi)
import           FilePath  (FilePath, (</>))
import           FPromise  (Promise, then_)
import           HTTP      (delete, get, put, resolveText)
import           Prelude

timeSuffix :: Fay Text
timeSuffix = ffi "(function() {return '?t=' + Math.floor(new Date() / 1000 )})()"

saveFile :: FilePath -> Text -> Fay Promise
saveFile fn body = put url (Just body) >>= then_ resolveText
  where url = "/api/file" </> fn

readFile :: FilePath -> Fay Promise
readFile fn = do
  t <- timeSuffix
  get (url <> t) >>= then_ resolveText
  where url = "/api/file" </> fn

deleteFile :: FilePath -> Fay Promise
deleteFile fn = delete ur >>= then_ resolveText
  where ur = "/api/file" </> fn
