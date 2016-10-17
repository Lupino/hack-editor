{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Config
  (
    ProcessButton (..),
    Tool (..),
    getToolByID,
    Config (..),
    emptyConfig,
    renderProcBtn,
    renderToolBtn
  ) where

import Prelude hiding (concat, null)
import Data.Text (fromString, Text, concat, (<>), null)

data ProcessButton = ProcessButton { getProcBtnStyle  :: Text,
                                     getProcBtnPrompt :: Text,
                                     getProcList      :: Text,
                                     getProcBtnTitle  :: Text }

data Tool = Tool { getToolID        :: Text,
                   getToolName      :: Text,
                   getToolStyle     :: Text,
                   getToolModalFile :: Text,
                   getToolProcFile  :: Text,
                   getToolProcArgv  :: [Text] }

getToolByID :: Text -> [Tool] -> Maybe Tool
getToolByID toolId (x:xs) | getToolID x == toolId = Just x
                          | otherwise             = getToolByID toolId xs

getToolByID _ [] = Nothing


data Config = Config { getStartProcList   :: [ProcessButton],
                       getStopProcList    :: [ProcessButton],
                       getRestartProcList :: [ProcessButton],
                       getToolList        :: [Tool],
                       getProcModalStyle  :: Text,
                       getToolModalStyle  :: Text }

emptyConfig :: Config
emptyConfig = Config { getStartProcList   = [],
                       getStopProcList    = [],
                       getRestartProcList = [],
                       getToolList        = [],
                       getProcModalStyle  = "",
                       getToolModalStyle  = "" }

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
