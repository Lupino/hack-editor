{-# LANGUAGE OverloadedStrings #-}
module Main where

import Proc
import Network (PortID(PortNumber))
import Web.Scotty (get, post, delete, put, raw, settings, json, param, header,
                   ActionM, redirect, setHeader, scottyOpts, body, middleware,
                   text, RoutePattern, function)
import Network.Wai (Request(..))
import Network.Wai.Handler.Warp (setPort, setHost)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.HttpAuth (basicAuth)
import Data.SecureMem (SecureMem, secureMemFromByteString)
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.Wai.Middleware.Static (staticPolicy, noDots, (>->), addBase)
import Data.Default.Class (def)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>), dropDrive, dropFileName)
import Data.List (isPrefixOf)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (pack, unpack)
import qualified Data.ByteString.Char8 as BC (unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as BL (readFile, unpack)
import Data.Aeson (object, (.=))
import Network.Mime (MimeType, defaultMimeLookup)
import Data.Maybe (fromJust)
import Codec.Archive.Zip (toArchive, ZipOption (..), extractFilesFromArchive)
import System.Directory (doesFileExist)

import Options.Applicative (Parser(..), execParser, strOption, option, auto,
                            long, short, help, value, (<*>), (<>), helper,
                            fullDesc, info, progDesc, metavar)

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
      fc <- liftIO $ BL.readFile path
      raw fc

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
