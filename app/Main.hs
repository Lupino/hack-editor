{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class               (liftIO)
import           Data.Aeson                           (object, (.=))
import qualified Data.ByteString.Char8                as BC (unpack)
import qualified Data.ByteString.Lazy.Char8           as BL (readFile)
import qualified Data.ByteString.UTF8                 as BU (toString)
import           Data.List                            (isPrefixOf)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import qualified Data.Text                            as T (Text, pack, unpack)
import qualified Data.Text.Lazy                       as TL (pack)
import           HackEditor
import           Network.HTTP.Types                   (status404, urlDecode)
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
import           System.Directory                     (doesFileExist)
import           System.FilePath                      ((</>))
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
                                <> help "HackEditor server host."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                  <> short 'p'
                                  <> metavar "PORT"
                                  <> help "HackEditor server port."
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
       <> progDesc "HackEditor Server" )

program :: Options -> IO ()
program opts = do
  tm <- newTermManager
  gen <- newTermGen

  procRoot <- getWorkingDirectory
  changeWorkingDirectory root
  workRoot <- getWorkingDirectory
  sapp <- scottyApp $ application tm gen procRoot workRoot
  W.runSettings settings
    $ websocketsOr WS.defaultConnectionOptions (serverApp workRoot tm) sapp

  where port = getPort opts
        host = getHost opts
        root = getRoot opts
        settings = W.setPort port . W.setHost (Host host) $ W.defaultSettings

application :: TermManager -> TermGen -> FilePath -> FilePath -> ScottyM ()
application tm gen procRoot workRoot = do
  middleware logStdout
  middleware staticMid
  middleware staticMid'

  get "/" $ redirect "/index.html"

  get "/api/file" $ do
    trees <- liftIO $ getFileTreeList [] workRoot
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

  delete (textRoute [ "api", "file" ]) $ do
    path <- filePath workRoot
    liftIO $ deleteFile path
    resultOK

  post "/api/term/create" $ do
    cols <- param "cols"
    rows <- param "rows"
    TermId tid <- liftIO $ createTerm tm gen (cols, rows)
    json $ object [ "id" .= tid ]
  post "/api/term/:tid/resize" $ do
    cols <- param "cols"
    rows <- param "rows"
    tid <- TermId <$> param "tid"
    liftIO $ resizeTerm tm tid (cols, rows)
    resultOK
  post "/api/term/:tid/close" $ do
    tid <- TermId <$> param "tid"
    liftIO $ closeTerm tm tid
    resultOK

  where staticMid = staticPolicy (addBase $ procRoot </> "public")
        staticMid' = staticPolicy (addBase workRoot)
        resultOK = json $ object [ "result" .= ("OK" :: String) ]

filePath :: FilePath -> ActionM FilePath
filePath root = do
  path <- BU.toString . urlDecode True <$> param "path"
  return $ root </> path

textRoute :: [T.Text] -> RoutePattern
textRoute strs = function $ \req ->
  if isPrefixOf strs (pathInfo req) then
    Just [("path", TL.pack $ foldr ((</>) . T.unpack) "" (drop 2 $ pathInfo req))]
  else Nothing


-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
