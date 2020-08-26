{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Archive.Zip                    (ZipOption (..),
                                                       extractFilesFromArchive,
                                                       toArchive)
import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (object, (.=))
import           Data.ByteString.Base64.Lazy          (decodeLenient)
import qualified Data.ByteString.Char8                as BC (unpack)
import qualified Data.ByteString.Lazy.Char8           as BL (readFile, unpack)
import           Data.List                            (isPrefixOf)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import qualified Data.Text                            as T (Text, pack, unpack)
import qualified Data.Text.Lazy                       as TL (pack)
import           Network.HTTP.Types                   (status404, status500)
import           Network.Mime                         (MimeType,
                                                       defaultMimeLookup)
import           Network.Wai                          (Request (..))
import qualified Network.Wai.Handler.Warp             as W (defaultSettings,
                                                            runSettings,
                                                            setHost, setPort)
import           Network.Wai.Handler.WebSockets       (websocketsOr)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
import qualified Network.WebSockets                   as WS (defaultConnectionOptions)
import           Proc
import           System.Directory                     (doesFileExist)
import           System.FilePath                      (dropFileName, (</>))
import           System.Posix.Directory               (changeWorkingDirectory,
                                                       getWorkingDirectory)
import           Web.Scotty                           (ActionM, RoutePattern,
                                                       ScottyM, body, delete,
                                                       function, get, json,
                                                       middleware, param, post,
                                                       put, raw, redirect,
                                                       scottyApp, setHeader,
                                                       status, text)

import           Options.Applicative                  (Parser (..), auto,
                                                       execParser, fullDesc,
                                                       help, helper, info, long,
                                                       metavar, option,
                                                       progDesc, short,
                                                       strOption, value)

data Options = Options
  { getHost :: String
  , getPort :: Int
  , getRoot :: String
  }

parser :: Parser Options
parser = Options <$> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Proc server host."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "Proc server port."
                                  <> value 8000 )
                 <*> strOption (long "source"
                                <> short 's'
                                <> metavar "DIR"
                                <> help "Source directory."
                                <> value "source" )

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      (fullDesc
       <> progDesc "Proc Server" )

program :: Options -> IO ()
program opts = do
  th <- newTermHandle
  procRoot <- getWorkingDirectory
  changeWorkingDirectory root
  workRoot <- getWorkingDirectory
  sapp <- scottyApp $ application th procRoot workRoot
  W.runSettings settings
    $ websocketsOr WS.defaultConnectionOptions (termServerApp th) sapp

  where port = getPort opts
        host = getHost opts
        root = getRoot opts
        settings = W.setPort port . W.setHost (Host host) $ W.defaultSettings

application :: TermHandle -> FilePath -> FilePath -> ScottyM ()
application th procRoot workRoot = do
  middleware logStdout
  middleware staticMid
  middleware staticMid'

  get "/" $ redirect "/index.html"

  get "/api/file" $ do
    trees <- liftIO $ getFileTreeList workRoot
    json $ treeListToJSON trees

  get (textRoute [ "api", "file" ]) $ do
    path <- filePath workRoot
    setHeader "Content-Type" $ TL.pack $ BC.unpack $ getMimeType path
    fileExists <- liftIO $ doesFileExist path
    if fileExists then do
      fc <- liftIO $ BL.readFile path
      raw fc
    else do
      status status404
      text "404 Not Found"

  put (textRoute [ "api", "file" ]) $ do
    path <- filePath workRoot
    wb <- body
    liftIO $ saveFile path wb
    resultOK


  put (textRoute [ "api", "upload" ]) $ do
    path <- filePath workRoot
    wb <- decodeLenient <$> body
    liftIO $ saveFile path wb
    resultOK


  put (textRoute [ "api", "uploadArchive" ]) $ do
    path <- filePath workRoot
    wb <- decodeLenient <$> body
    liftIO $ extractFilesFromArchive [OptDestination (dropFileName path)] $ toArchive wb
    resultOK

  delete (textRoute [ "api", "file" ]) $ do
    path <- filePath workRoot
    liftIO $ deleteFile path
    resultOK

  post (textRoute [ "api", "python" ]) $ runProc_ workRoot Python
  post (textRoute [ "api", "node" ])   $ runProc_ workRoot Node
  post (textRoute [ "api", "bash" ])   $ runProc_ workRoot Bash

  post "/api/term/create" $ do
    cols <- param "cols"
    rows <- param "rows"
    liftIO $ closeTerm th
    liftIO $ createTerm th (cols, rows)
    resultOK
  post "/api/term/resize" $ do
    cols <- param "cols"
    rows <- param "rows"
    liftIO $ resizeTerm th (cols, rows)
    resultOK
  post "/api/term/close" $ do
    liftIO $ closeTerm th
    resultOK

  where staticMid = staticPolicy (addBase $ procRoot </> "public")
        staticMid' = staticPolicy (addBase workRoot)
        resultOK = json $ object [ "result" .= ("OK" :: String) ]

runProc_ :: FilePath -> ProcName -> ActionM ()
runProc_ root name = do
  path <- filePath root
  wb <- body
  setHeader "Content-Type" $ TL.pack "plain/text"

  let args = read $ BL.unpack wb
  liftIO $ changeWorkingDirectory root
  fc <- liftIO $ runProc $ Proc name (path:args)
  case fc of
    Left err  -> status status500 >> raw err
    Right out -> raw out

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- param "path"
  return $ root </> path

textRoute :: [T.Text] -> RoutePattern
textRoute strs = function $ \req ->
  if isPrefixOf strs (pathInfo req) then
    Just [("path", TL.pack $ foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req))]
  else Nothing


-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
