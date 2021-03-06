module ProcAPI
  ( ProcAPI
  , newProcAPI
  , readFile
  , writeFile
  , removeFile
  , File
  , uploadFile
  , loadFileTree
  , signWSPath
  , signFilePath
  ) where

import           Data.Text (Text)
import           FFI       (ffi)
import           FilePath  (FilePath)
import           FPromise  (Promise)
import           Prelude

data ProcAPI
data File

newProcAPI :: Text -> Text -> Fay ProcAPI
newProcAPI = ffi "new ProcJSApi({key: %1, secret: %2})"

readFile :: ProcAPI -> FilePath -> Fay Promise
readFile = ffi "%1['readFile'](%2)"

writeFile :: ProcAPI -> FilePath -> Text -> Fay Promise
writeFile = ffi "%1['writeFile'](%2, %3)"

removeFile :: ProcAPI -> FilePath -> Fay Promise
removeFile = ffi "%1['removeFile'](%2)"

uploadFile :: ProcAPI -> Text -> File -> Fay Promise
uploadFile = ffi "%1['uploadFile'](%2, %3)"

loadFileTree :: ProcAPI -> Fay Promise
loadFileTree = ffi "%1['loadFileTree']()"

signWSPath :: ProcAPI -> FilePath -> Fay Promise
signWSPath = ffi "%1['signWSPath'](%2)"

signFilePath :: ProcAPI -> FilePath -> Fay Promise
signFilePath = ffi "%1['signFilePath'](%2)"
