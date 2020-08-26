module ProcAPI
  ( ProcAPI
  , newProcAPI
  , readFile
  , writeFile
  , removeFile
  , uploadFile
  , uploadArchive
  , runFile
  , createTerm
  , resizeTerm
  , closeTerm
  , loadFileTree
  , signWSPath
  , signFilePath
  ) where

import           Data.Text (Text, pack)
import           FFI       (ffi)
import           FilePath  (FilePath)
import           FPromise  (Promise)
import           Prelude

data ProcAPI

newProcAPI :: Text -> Text -> Fay ProcAPI
newProcAPI = ffi "new ProcJSApi({key: %1, secret: %2})"

readFile :: ProcAPI -> FilePath -> Fay Promise
readFile = ffi "%1['readFile'](%2)"

writeFile :: ProcAPI -> FilePath -> Text -> Fay Promise
writeFile = ffi "%1['writeFile'](%2, %3)"

removeFile :: ProcAPI -> FilePath -> Fay Promise
removeFile = ffi "%1['removeFile'](%2)"

uploadFile :: ProcAPI -> FilePath -> Text -> Fay Promise
uploadFile = ffi "%1['uploadFile'](%2, %3)"

uploadArchive :: ProcAPI -> FilePath -> Text -> Fay Promise
uploadArchive = ffi "%1['uploadArchive'](%2, %3)"

runFile_ :: ProcAPI -> FilePath -> Text -> Fay Promise
runFile_ = ffi "%1['runFile'](%2, %3)"

runFile :: ProcAPI -> FilePath -> [Text] -> Fay Promise
runFile api fn = runFile_ api fn . pack . show

createTerm :: ProcAPI -> Int -> Int -> Fay Promise
createTerm = ffi "%1['createTerm'](%2, %3)"

resizeTerm :: ProcAPI -> Int -> Int -> Fay Promise
resizeTerm = ffi "%1['resizeTerm'](%2, %3)"

closeTerm :: ProcAPI -> Fay Promise
closeTerm = ffi "%1['closeTerm']()"

loadFileTree :: ProcAPI -> Fay Promise
loadFileTree = ffi "%1['loadFileTree']()"

signWSPath :: ProcAPI -> FilePath -> Fay Promise
signWSPath = ffi "%1['signWSPath'](%2)"

signFilePath :: ProcAPI -> FilePath -> Fay Promise
signFilePath = ffi "%1['signFilePath'](%2)"
