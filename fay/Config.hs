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

import           Data.Text  (Text, concat, fromString, null, (<>))
import           Prelude    hiding (concat, null)
import           SimpleJSON

data ProcessButton = ProcessButton { getProcBtnStyle  :: Text,
                                     getProcBtnPrompt :: Text,
                                     getProcList      :: Text,
                                     getProcBtnTitle  :: Text }

procBtnDecoder :: Parser
procBtnDecoder = withDecoder "ProcessButton" [ customRule "getProcBtnStyle"  "style",
                                               customRule "getProcBtnPrompt" "prompt",
                                               customRule "getProcList"      "proc",
                                               customRule "getProcBtnTitle"  "title"
                                               ]

data Tool = Tool { getToolID        :: Text,
                   getToolName      :: Text,
                   getToolStyle     :: Text,
                   getToolModalFile :: Text,
                   getToolProcFile  :: Text,
                   getToolProcArgv  :: [Text] }

toolDecoder :: Parser
toolDecoder = withDecoder "Tool" [ customRule "getToolID"        "id",
                                   customRule "getToolName"      "name",
                                   customRule "getToolStyle"     "style",
                                   customRule "getToolModalFile" "modal",
                                   customRule "getToolProcFile"  "proc",
                                   customRule "getToolProcArgv"  "argv"
                                   ]

getToolByID :: Text -> [Tool] -> Maybe Tool
getToolByID toolId (x:xs) | getToolID x == toolId = Just x
                          | otherwise             = getToolByID toolId xs

getToolByID _ [] = Nothing


data Config = Config { getStartProcList   :: [ProcessButton],
                       getStopProcList    :: [ProcessButton],
                       getRestartProcList :: [ProcessButton],
                       getToolList        :: [Tool],
                       getProcModalStyle  :: Text,
                       getToolModalStyle  :: Text,
                       getIsAutoSave      :: Bool }

configDecoder :: Parser
configDecoder = withDecoder "Config" [ listRule   "getStartProcList"   "start_list"   procBtnDecoder,
                                       listRule   "getStopProcList"    "stop_list"    procBtnDecoder,
                                       listRule   "getRestartProcList" "restart_list" procBtnDecoder,
                                       listRule   "getToolList"        "tools"        toolDecoder,
                                       customRule "getProcModalStyle"  "proc_modal_style",
                                       customRule "getToolModalStyle"  "tool_modal_style",
                                       customRule "getIsAutoSave"      "auto_save"
                                       ]

emptyConfig :: Config
emptyConfig = Config { getStartProcList   = [],
                       getStopProcList    = [],
                       getRestartProcList = [],
                       getToolList        = [],
                       getProcModalStyle  = "",
                       getToolModalStyle  = "",
                       getIsAutoSave      = True }

parseConfig :: Text -> Config
parseConfig txt = decode txt configDecoder :: Config

renderProcBtn :: ProcessButton -> Text
renderProcBtn btn = renderButton [("class",       "uk-button " <> sty),
                                  ("data-prompt", prompt),
                                  ("data-proc",   procList)] title
  where sty      = getProcBtnStyle  btn
        prompt   = getProcBtnPrompt btn
        procList = getProcList      btn
        title    = getProcBtnTitle  btn

renderToolBtn :: Tool -> Text
renderToolBtn btn = renderButton [("class",        "uk-button " <> sty),
                                  ("data-tool-id", toolId)] name

  where sty    = getToolStyle btn
        name   = getToolName  btn
        toolId = getToolID    btn

renderButton :: [(Text, Text)] -> Text -> Text
renderButton props title = concat ["<button", renderProps props, ">", title, "</button>"]
  where renderProps :: [(Text, Text)] -> Text
        renderProps ((k,v):xs) | not $ null v  = concat [" ", k, "=", "\"", v, "\"", renderProps xs]
                               | otherwise     = renderProps xs
        renderProps []                         = ""
