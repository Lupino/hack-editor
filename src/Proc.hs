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

import           Control.Monad                  (forM, unless, when)
import           Data.Maybe                     (fromMaybe)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesDirectoryExist,
                                                 doesFileExist,
                                                 getDirectoryContents,
                                                 removeDirectoryRecursive,
                                                 removeFile)
import           System.FilePath                (dropFileName, (</>))

import           System.IO                      (IOMode (ReadMode), hFileSize,
                                                 withFile)

import           Data.Aeson                     (ToJSON (..), Value (..),
                                                 encode, object, (.=))
import qualified Data.ByteString.Lazy           as LB (ByteString, concat,
                                                       empty, hGetContents,
                                                       writeFile)
import           Data.HashMap.Strict            (union)
import qualified Data.Text                      as T (pack)
import           System.Exit                    (ExitCode (..))
import           System.Process.ByteString.Lazy (readProcessWithExitCode)

data FileTree = Directory String [FileTree] | FileName String Int | Empty
  deriving (Show)

instance ToJSON FileTree where
  toJSON (Directory dir trees) = object [ T.pack dir .= treeListToJSON trees ]
  toJSON (FileName name size)  = object [T.pack name .= size ]
  toJSON Empty                 = Null

unionValue :: Value -> Value -> Value
unionValue (Object a) (Object b) = Object $ union a b
unionValue (Object a) _          = Object a
unionValue _ (Object b)          = Object b
unionValue _ _                   = Null

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

data ProcName = Python | Node | Bash
data Proc = Proc ProcName [String]

getProcName :: ProcName -> FilePath
getProcName Python = "python3"
getProcName Node   = "node"
getProcName Bash   = "bash"

runProc :: Proc -> IO (Either LB.ByteString LB.ByteString)
runProc (Proc name args) = do
  (code, out, err) <- readProcessWithExitCode (getProcName name) args LB.empty
  case code of
    ExitSuccess   -> return (Right out)
    ExitFailure _ -> return (Left err)
