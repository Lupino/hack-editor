{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proc
  ( encodeTreeList
  , treeListToJSON
  , getFileTreeList
  , saveFile
  , deleteFile
  , ProcName (..)
  , Proc (..)
  , runProc

  , newTermHandle
  , TermHandle
  , closeTerm
  , createTerm
  , readTerm
  , writeTerm
  , resizeTerm
  , termServerApp
  ) where


import           Control.Concurrent             (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM         (TVar, newTVarIO, readTVar,
                                                 readTVarIO, writeTVar)
import           Control.Exception              (SomeException, try)
import           Control.Monad                  (forM, forever, unless, void,
                                                 when)
import           Control.Monad.STM              (atomically)
import           Data.Aeson                     (ToJSON (..), Value (..),
                                                 encode, object, (.=))
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Lazy           as LB (ByteString, empty,
                                                       toStrict, writeFile)
import qualified Data.ByteString.Lazy.UTF8      as LBU (fromString)
import qualified Data.ByteString.UTF8           as BU (toString)
import           Data.HashMap.Strict            (union)
import           Data.Maybe                     (isNothing)
import qualified Data.Text                      as T (pack)
import qualified Network.WebSockets             as WS (DataMessage (..),
                                                       ServerApp, acceptRequest,
                                                       receiveDataMessage,
                                                       sendDataMessage)
import           Network.WebSockets.Connection  as WS (pingThread)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesDirectoryExist,
                                                 doesFileExist,
                                                 getDirectoryContents,
                                                 removeDirectoryRecursive,
                                                 removeFile)
import           System.Exit                    (ExitCode (..))
import           System.FilePath                (dropFileName, (</>))
import           System.IO                      (IOMode (ReadMode), hFileSize,
                                                 withFile)
import           System.Posix.Pty               (Pty, PtyControlCode, closePty,
                                                 resizePty, spawnWithPty,
                                                 tryReadPty, writePty)
import           System.Process                 (ProcessHandle,
                                                 terminateProcess)
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
      isDir <- doesDirectoryExist path
      if isDir then do
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

newtype TermHandle = TermHandle (TVar (Maybe (Pty, ProcessHandle)))

newTermHandle :: IO TermHandle
newTermHandle = TermHandle <$> newTVarIO Nothing

createTerm :: TermHandle -> (Int, Int) -> IO ()
createTerm (TermHandle h) size = do
  o <- readTVarIO h
  when (isNothing o) $ do
    r <- spawnWithPty Nothing True "bash" [] size
    atomically $ writeTVar h $ Just r

callTerm ::(Pty -> IO a) -> TermHandle -> IO a
callTerm f (TermHandle h) = do
  o <- readTVarIO h
  case o of
    Nothing       -> error "term is terminate"
    Just (pty, _) -> f pty

closeTerm :: TermHandle -> IO ()
closeTerm (TermHandle h) = do
  o <- atomically $ do
    o <- readTVar h
    writeTVar h Nothing
    return o

  case o of
    Nothing -> return ()
    Just (pty, ph) -> do
      terminateProcess ph
      closePty pty

readTerm :: TermHandle -> IO (Either [PtyControlCode] ByteString)
readTerm = callTerm tryReadPty

writeTerm :: TermHandle -> ByteString -> IO ()
writeTerm h bs = callTerm (flip writePty bs) h

resizeTerm :: TermHandle -> (Int, Int) -> IO ()
resizeTerm h size = callTerm (flip resizePty size) h

termServerApp :: TermHandle -> WS.ServerApp
termServerApp th pendingConn = do
  conn <- WS.acceptRequest pendingConn
  threads <- newTVarIO []
  let addThread t = atomically $ do
        xs <- readTVar threads
        writeTVar threads (t:xs)
      killThreads = do
        xs <- readTVarIO threads
        void . forkIO $ mapM_ killThread xs
  thread1 <- forkIO $ WS.pingThread conn 30 (return ())
  addThread thread1

  thread2 <- forkIO $ forever $ do
    bs0 <- try $ WS.receiveDataMessage conn
    case bs0 of
      Left (_ :: SomeException) -> killThreads
      Right (WS.Text bs _)      -> writeTerm th $ LB.toStrict bs
      Right (WS.Binary bs)      -> writeTerm th $ LB.toStrict bs
  addThread thread2

  thread3 <- myThreadId
  addThread thread3

  forever $ do
    bs0 <- try $ readTerm th
    case bs0 of
      Left (_ :: SomeException) -> killThreads
      Right (Left c) -> print c
      Right (Right bs1) -> WS.sendDataMessage conn (WS.Text (LBU.fromString $ BU.toString bs1) Nothing)
