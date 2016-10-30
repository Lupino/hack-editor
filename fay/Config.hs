{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Config
  (
    ProcessButton (..),
    Tool (..),
    getToolByID,
    Config (..),
    emptyConfig,
    parseConfig,
    renderProcBtn,
    renderToolBtn
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Text  (Text, concat, fromString, null, (<>))
import           Prelude    hiding (concat, null)
import           SimpleJSON

data ProcessButton = ProcessButton { getProcBtnStyle  :: Maybe Text,
                                     getProcBtnPrompt :: Text,
                                     getProcList      :: Text,
                                     getProcBtnTitle  :: Text }

procBtnDecoder :: Parser
procBtnDecoder = withDecoder "ProcessButton" [ rule (maybeParser rawParser) "getProcBtnStyle"  "style",
                                               rawRule "getProcBtnPrompt" "prompt",
                                               rawRule "getProcList"      "proc",
                                               rawRule "getProcBtnTitle"  "title"
                                               ]

data Tool = Tool { getToolID        :: Text,
                   getToolName      :: Text,
                   getToolStyle     :: Maybe Text,
                   getToolModalFile :: Maybe Text,
                   getToolProcFile  :: Maybe Text,
                   getToolProcArgv  :: Maybe [Text] }

toolDecoder :: Parser
toolDecoder = withDecoder "Tool" [ rawRule            "getToolID"        "id",
                                   rawRule            "getToolName"      "name",
                                   rule (maybeParser rawParser) "getToolStyle"     "style",
                                   rule (maybeParser rawParser) "getToolModalFile" "modal",
                                   rule (maybeParser rawParser) "getToolProcFile"  "proc",
                                   rule (maybeParser $ listParser rawParser) "getToolProcArgv"  "argv"
                                   ]

getToolByID :: Text -> [Tool] -> Maybe Tool
getToolByID toolId (x:xs) | getToolID x == toolId = Just x
                          | otherwise             = getToolByID toolId xs

getToolByID _ [] = Nothing


data Config = Config { getStartProcList   :: [ProcessButton],
                       getStopProcList    :: [ProcessButton],
                       getRestartProcList :: [ProcessButton],
                       getToolList        :: [Tool],
                       getProcModalStyle  :: Maybe Text,
                       getToolModalStyle  :: Maybe Text,
                       getIsAutoSave      :: Bool }

configDecoder :: Parser
configDecoder = withDecoder "Config" [ listRule procBtnDecoder "getStartProcList"   "start_list",
                                       listRule procBtnDecoder "getStopProcList"    "stop_list",
                                       listRule procBtnDecoder "getRestartProcList" "restart_list",
                                       listRule toolDecoder    "getToolList"        "tools",
                                       rule     (maybeParser rawParser)  "getProcModalStyle"  "proc_modal_style",
                                       rule     (maybeParser rawParser)  "getToolModalStyle"  "tool_modal_style",
                                       rawRule                 "getIsAutoSave"      "auto_save"
                                       ]

emptyConfig :: Config
emptyConfig = Config { getStartProcList   = [],
                       getStopProcList    = [],
                       getRestartProcList = [],
                       getToolList        = [],
                       getProcModalStyle  = Nothing,
                       getToolModalStyle  = Nothing,
                       getIsAutoSave      = True }

parseConfig :: Text -> Config
parseConfig txt = decode txt configDecoder :: Config

renderProcBtn :: ProcessButton -> Text
renderProcBtn btn = renderButton [("class",       "uk-button " <> sty),
                                  ("data-prompt", prompt),
                                  ("data-proc",   procList)] title
  where sty      = fromMaybe "" $ getProcBtnStyle  btn
        prompt   = getProcBtnPrompt btn
        procList = getProcList      btn
        title    = getProcBtnTitle  btn

renderToolBtn :: Tool -> Text
renderToolBtn btn = renderButton [("class",        "uk-button " <> sty),
                                  ("data-tool-id", toolId)] name

  where sty    = fromMaybe "" $ getToolStyle btn
        name   = getToolName  btn
        toolId = getToolID    btn

renderButton :: [(Text, Text)] -> Text -> Text
renderButton props title = concat ["<button", renderProps props, ">", title, "</button>"]
  where renderProps :: [(Text, Text)] -> Text
        renderProps ((k,v):xs) | not $ null v  = concat [" ", k, "=", "\"", v, "\"", renderProps xs]
                               | otherwise     = renderProps xs
        renderProps []                         = ""
