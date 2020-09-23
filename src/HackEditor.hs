{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HackEditor
  ( encodeTreeList
  , treeListToJSON
  , getFileTreeList
  , saveFile
  , deleteFile
  , ProcName (..)
  , Proc (..)
  , runProc

  , TermId (..)
  , TermGen
  , newTermGen
  , TermManager
  , newTermManager
  , closeTerm
  , createTerm
  , resizeTerm
  , serverApp
  ) where


import           Control.Concurrent             (forkIO, killThread, myThreadId)
import           Control.Concurrent.STM         (TVar, newTVarIO, readTVar,
                                                 readTVarIO, writeTVar)
import           Control.Exception              (SomeException, try)
import           Control.Monad                  (forM, forever, mzero, unless,
                                                 void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.STM              (atomically)
import           Control.Monad.Trans.Maybe      (runMaybeT)
import           Data.Aeson                     (ToJSON (..), Value (..),
                                                 encode, object, (.=))
import qualified Data.ByteString                as B (readFile)
import qualified Data.ByteString.Char8          as B (drop, take, takeWhile)
import qualified Data.ByteString.Lazy           as LB (ByteString, empty, hPut,
                                                       toStrict, writeFile)
import qualified Data.ByteString.Lazy.UTF8      as LBU (fromString)
import qualified Data.ByteString.UTF8           as BU (lines, toString)
import           Data.Hashable                  (Hashable, hashWithSalt)
import           Data.HashMap.Strict            (HashMap, union)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T (pack)
import           Network.HTTP.Types             (urlDecode)
import qualified Network.WebSockets             as WS (DataMessage (..),
                                                       ServerApp, acceptRequest,
                                                       pendingRequest,
                                                       receiveDataMessage,
                                                       rejectRequest,
                                                       requestPath,
                                                       sendDataMessage)
import           Network.WebSockets.Connection  as WS (pingThread)
import           System.Directory               (createDirectoryIfMissing,
                                                 doesDirectoryExist,
                                                 doesFileExist,
                                                 getDirectoryContents,
                                                 removeDirectoryRecursive,
                                                 removeFile)
import           System.Exit                    (ExitCode (..))
import           System.FilePath                (dropFileName, takeFileName,
                                                 (</>))
import           System.FilePath.Glob           (Pattern, compile, match,
                                                 simplify)
import           System.IO                      (IOMode (ReadMode, WriteMode),
                                                 hFileSize, withFile)
import           System.Posix.Pty               (Pty, closePty, resizePty,
                                                 spawnWithPty, tryReadPty,
                                                 writePty)
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

loadPattern :: FilePath -> [Pattern] -> IO [Pattern]
loadPattern dir pats = do
  isFile <- doesFileExist path
  if isFile then do
    xs <- map BU.toString . BU.lines <$> B.readFile path
    return $ pats ++ go xs
  else return pats
  where path = dir </> ".editorignore"

        go :: [String] -> [Pattern]
        go []           = []
        go (('#':_):xs) = go xs
        go ("":xs)      = go xs
        go (x:xs)       = simplify (compile x) : go xs

isIgnorePath :: [Pattern] -> FilePath -> Bool
isIgnorePath [] _ = False
isIgnorePath (x:xs) p | match x p = True
                      | match x (takeFileName p) = True
                      | otherwise = isIgnorePath xs p

getFileTreeList :: [Pattern] -> FilePath -> IO [FileTree]
getFileTreeList fpats topdir = do
    isDirectory <- doesDirectoryExist topdir
    unless isDirectory $ createDirectoryIfMissing True topdir
    names <- getDirectoryContents topdir
    pats <- loadPattern topdir fpats
    let properNames = filter (`notElem` [".", ".."]) names
    forM properNames $ \name -> do
      let path = topdir </> name
      if isIgnorePath pats path then return Empty
                           else do
        isDir <- doesDirectoryExist path
        if isDir then do
          subTree <- getFileTreeList pats path
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

newtype TermId = TermId Int
  deriving (Show, Eq)

instance Hashable TermId where
  hashWithSalt s (TermId tid) = hashWithSalt s tid

newtype TermGen = TermGen (IO TermId)
newtype TermManager = TermManager (TVar (HashMap TermId (Pty, ProcessHandle)))

newTermGen :: IO TermGen
newTermGen = do
  h <- newTVarIO 1
  let nextId = atomically $ do
        r <- readTVar h
        writeTVar h $ r + 1
        return $ TermId r
  return $ TermGen nextId


nextTermId :: TermGen -> IO TermId
nextTermId (TermGen gen) = gen

newTermManager :: IO TermManager
newTermManager = TermManager <$> newTVarIO HM.empty

createTerm :: TermManager -> TermGen -> (Int, Int) -> IO TermId
createTerm (TermManager tm) gen size = do
  r <- spawnWithPty Nothing True "bash" [] size
  termId <- nextTermId gen
  atomically $ do
    hm <- readTVar tm
    writeTVar tm $ HM.insert termId r hm

  return termId


getTerm :: TermManager -> TermId -> IO (Maybe (Pty, ProcessHandle))
getTerm (TermManager tm) tid = HM.lookup tid <$> readTVarIO tm

getTermPty :: TermManager -> TermId -> IO (Maybe Pty)
getTermPty tm tid = fmap fst <$> getTerm tm tid

closeTerm :: TermManager -> TermId -> IO ()
closeTerm (TermManager tm) tid = do
  mterm <- atomically $ do
    hm <- readTVar tm
    let mterm = HM.lookup tid hm
    writeTVar tm $ HM.delete tid hm

    return mterm

  case mterm of
    Nothing -> return ()
    Just (pty, ph) -> do
      terminateProcess ph
      closePty pty

resizeTerm :: TermManager -> TermId -> (Int, Int) -> IO ()
resizeTerm tm tid size = do
  mpty <- getTermPty tm tid
  case mpty of
    Nothing  -> error "Term not found."
    Just pty -> resizePty pty size

-- /api/term/:tid
termServerApp :: TermId -> TermManager -> WS.ServerApp
termServerApp tid tm pendingConn = do
  mpty <- getTermPty tm tid
  case mpty of
    Nothing -> WS.rejectRequest pendingConn "{\"err\": \"Term not found.\"}"
    Just pty -> do
      conn <- WS.acceptRequest pendingConn
      threads <- newTVarIO []
      let addThread t = atomically $ do
            xs <- readTVar threads
            writeTVar threads (t:xs)
          killThreads = do
            xs <- readTVarIO threads
            void . forkIO $ mapM_ killThread xs
            closeTerm tm tid

      thread1 <- forkIO $ WS.pingThread conn 30 (return ())
      addThread thread1

      thread2 <- forkIO $ forever $ do
        bs0 <- try $ WS.receiveDataMessage conn
        case bs0 of
          Left (_ :: SomeException) -> killThreads
          Right (WS.Text bs _)      -> writePty pty $ LB.toStrict bs
          Right (WS.Binary bs)      -> writePty pty $ LB.toStrict bs
      addThread thread2

      thread3 <- myThreadId
      addThread thread3

      forever $ do
        bs0 <- try $ tryReadPty pty
        case bs0 of
          Left (_ :: SomeException) -> killThreads
          Right (Left c) -> print c
          Right (Right bs1) -> WS.sendDataMessage conn (WS.Text (LBU.fromString $ BU.toString bs1) Nothing)

-- /api/upload/:filePath
uploadServerApp :: FilePath -> WS.ServerApp
uploadServerApp path pendingConn = do
  conn <- WS.acceptRequest pendingConn
  WS.sendDataMessage conn (WS.Text "{\"result\": \"OK\"}" Nothing)
  withFile path WriteMode $ \h -> do
    void . runMaybeT . forever $ do
      bs0 <- liftIO $ try $ WS.receiveDataMessage conn
      case bs0 of
        Left (_ :: SomeException) -> mzero
        Right (WS.Text "EOF" _)   -> mzero
        Right (WS.Binary "EOF")   -> mzero
        Right (WS.Text bs _)      -> liftIO $ do
          LB.hPut h bs
          WS.sendDataMessage conn (WS.Text "{\"result\": \"OK\"}" Nothing)
        Right (WS.Binary bs)      -> liftIO $ do
          LB.hPut h bs
          WS.sendDataMessage conn (WS.Text "{\"result\": \"OK\"}" Nothing)

-- /api/term/:tid
-- /api/upload/:filePath
serverApp :: FilePath -> TermManager -> WS.ServerApp
serverApp workRoot tm pendingConn =
  case B.take 10 pathname of
    "/api/term/" -> termServerApp tid tm pendingConn
    "/api/uploa" -> uploadServerApp path pendingConn
    _            -> WS.rejectRequest pendingConn "{\"err\": \"no such path.\"}"
  where requestHead = WS.pendingRequest pendingConn
        rawuri = WS.requestPath requestHead
        pathname = B.takeWhile (/='?') rawuri
        path = workRoot </> BU.toString (urlDecode True $ B.drop 12 pathname)
        tid = TermId $ read $ BU.toString $ B.drop 10 pathname
