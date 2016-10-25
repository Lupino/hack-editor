module Proc
  (
    encodeTreeList,
    treeListToJSON,
    getFileTreeList,
    saveFile,
    deleteFile,
    ProcName (..),
    Proc (..),
    runProc
  ) where

import Control.Monad (forM, when, unless)
import System.Directory (doesDirectoryExist, getDirectoryContents,
                         createDirectoryIfMissing, doesFileExist,
                         removeFile, removeDirectoryRecursive)
import System.FilePath ((</>), dropFileName)
import Data.Maybe (fromMaybe)

import System.IO (withFile, IOMode( ReadMode ), hFileSize)

import Data.Aeson (ToJSON(..), object, (.=), Value(..), encode)
import Data.HashMap.Strict (union)
import qualified Data.ByteString.Lazy as LB (ByteString, writeFile, hGetContents, concat)
import qualified Data.Text as T (pack)
import System.Process (createProcess, StdStream(..), proc, std_out, std_err)

data FileTree = Directory String [FileTree] | FileName String Int | Empty
  deriving (Show)

instance ToJSON FileTree where
  toJSON (Directory dir trees) = object [ T.pack dir .= treeListToJSON trees ]
  toJSON (FileName name size)  = object [T.pack name .= size ]
  toJSON Empty                 = Null

unionValue :: Value -> Value -> Value
unionValue (Object a) (Object b) = Object $ union a b
unionValue (Object a) _ = Object a
unionValue _ (Object b) = Object b
unionValue _ _ = Null

treeListToJSON :: [FileTree] -> Value
treeListToJSON = foldr (unionValue . toJSON) Null

encodeTreeList :: [FileTree] -> LB.ByteString
encodeTreeList = encode . treeListToJSON

getFileTreeList :: FilePath -> IO [FileTree]
getFileTreeList topdir = do
    isDirectory <- doesDirectoryExist topdir
    unless isDirectory $ createDirectoryIfMissing True topdir
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    forM properNames $ \name -> do
      let path = topdir </> name
      isDirectory <- doesDirectoryExist path
      if isDirectory then do
        subTree <- getFileTreeList path
        return $ Directory name subTree
      else do
        isFile <- doesFileExist path
        if isFile then do
          size <- fromInteger <$> getFileSize path
          return $ FileName name size
        else return Empty

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize

saveFile :: FilePath -> LB.ByteString -> IO ()
saveFile fn fc = do
  createDirectoryIfMissing True dir
  LB.writeFile fn fc

  where dir = dropFileName fn

deleteFile :: FilePath -> IO ()
deleteFile fn = do
  isDirectory <- doesDirectoryExist fn
  when isDirectory $ removeDirectoryRecursive fn
  fileExists <- doesFileExist fn
  when fileExists $ removeFile fn

data ProcName = Python | Node
data Proc = Proc ProcName [String]

getProcName :: ProcName -> FilePath
getProcName Python = "python3"
getProcName Node   = "node"

runProc :: Proc -> IO LB.ByteString
runProc (Proc name args) = do
  (_,Just hout,Just herr,_) <- createProcess (proc (getProcName name) args){ std_out = CreatePipe,
                                                                     std_err = CreatePipe }
  out <- LB.hGetContents hout
  err <- LB.hGetContents herr

  return $ LB.concat [err, out]
