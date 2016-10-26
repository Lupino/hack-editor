{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Archive.Zip                    (ZipOption (..),
                                                       extractFilesFromArchive,
                                                       toArchive)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (object, (.=))
import qualified Data.ByteString.Char8                as BC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8           as BL (readFile, unpack)
import           Data.Default.Class                   (def)
import           Data.List                            (isPrefixOf)
import           Data.Maybe                           (fromJust)
import           Data.SecureMem                       (SecureMem,
                                                       secureMemFromByteString)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import qualified Data.Text                            as T (Text, pack, unpack)
import qualified Data.Text.Lazy                       as TL (pack, unpack)
import           Network                              (PortID (PortNumber))
import           Network.HTTP.Types                   (status404)
import           Network.Mime                         (MimeType,
                                                       defaultMimeLookup)
import           Network.Wai                          (Request (..))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.HttpAuth      (basicAuth)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       staticPolicy, (>->))
import           Proc
import           System.Directory                     (doesFileExist)
import           System.FilePath                      (dropDrive, dropFileName,
                                                       (</>))
import           Web.Scotty                           (ActionM, RoutePattern,
                                                       body, delete, function,
                                                       get, header, json,
                                                       middleware, param, post,
                                                       put, raw, redirect,
                                                       scottyOpts, setHeader,
                                                       settings, status, text)

import           Options.Applicative                  (Parser (..), auto,
                                                       execParser, fullDesc,
                                                       help, helper, info, long,
                                                       metavar, option,
                                                       progDesc, short,
                                                       strOption, value, (<*>),
                                                       (<>))

data Options = Options { getHost   :: String,
                         getPort   :: Int,
                         getRoot   :: String,
                         getAdmin  :: String,
                         getPasswd :: String }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "The sshakyll server host."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "The sshakyll server port."
                                  <> value 8000 )
                 <*> strOption (long "directory"
                                <> short 'd'
                                <> metavar "DIR"
                                <> help "Site root dirctory."
                                <> value "" )
                 <*> strOption (long "admin"
                                <> metavar "ADMIN"
                                <> help "Site admin."
                                <> value "admin" )
                 <*> strOption (long "passwd"
                                <> metavar "PASSWD"
                                <> help "Admin password."
                                <> value "passwd" )

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      (fullDesc
       <> progDesc "Proc Server" )

program :: Options -> IO ()
program opts =
  scottyOpts serverOpts $ do
    middleware logStdout
    middleware staticMid
    middleware staticMid'
    middleware $ basicAuth (\u p -> return $ u == admin && secureMemFromByteString p == passwd)
      "My Realm"

    get "/" $ do
      hasEditor <- liftIO $ doesFileExist editor
      if hasEditor then redirect "/editor/index.html"
      else redirect "/index.html"

    get "/api/file" $ do
      trees <- liftIO $ getFileTreeList $ root </> "source"
      json $ treeListToJSON trees

    get (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      setHeader "Content-Type" $ TL.pack $ BC.unpack $ getMimeType path
      fileExists <- liftIO $ doesFileExist path
      if fileExists then do
        fc <- liftIO $ BL.readFile path
        raw fc
      else do
        status status404
        text "404 Not Found"

    put (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      wb <- body
      liftIO $ saveFile path wb

      text "OK"

    put (textRoute [ "api", "archive" ]) $ do
      path <- filePath root
      wb <- body
      liftIO $ extractFilesFromArchive [OptDestination (dropFileName path)] $ toArchive wb

      text "OK"

    delete (textRoute [ "api", "file" ]) $ do
      path <- filePath root
      liftIO $ deleteFile path

      text "OK"

    post (textRoute [ "api", "python" ]) $ do
      path <- filePath root
      wb <- body
      setHeader "Content-Type" $ TL.pack "plain/text"

      let args = read $ BL.unpack wb
      fc <- liftIO $ runProc $ Proc Python (path:args)
      raw fc

    post (textRoute [ "api", "node" ]) $ do
      path <- filePath root
      wb <- body
      setHeader "Content-Type" $ TL.pack "plain/text"

      let args = read $ BL.unpack wb
      fc <- liftIO $ runProc $ Proc Node (path:args)
      raw fc

    post (textRoute [ "api", "bash" ]) $ do
      path <- filePath root
      wb <- body
      setHeader "Content-Type" $ TL.pack "plain/text"

      let args = read $ BL.unpack wb
      fc <- liftIO $ runProc $ Proc Bash (path:args)
      raw fc

  where staticMid = staticPolicy (addBase "public")
        staticMid' = staticPolicy (addBase $ root </> "source")
        port = getPort opts
        host = getHost opts
        root = getRoot opts
        editor = root </> "source" </> "editor" </> "index.html"
        serverOpts = def { settings = setPort port $ setHost (Host host) (settings def) }
        admin = BC.pack $ getAdmin opts
        passwd = secureMemFromByteString $ BC.pack $ getPasswd opts

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- param "path"
  return $ root </> "source" </> path

textRoute :: [T.Text] -> RoutePattern
textRoute strs = function $ \req ->
  if isPrefixOf strs (pathInfo req) then
    Just [("path", TL.pack $ foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req))]
  else Nothing


-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
