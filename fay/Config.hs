{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Config
  (
    ProcessButton (..),
    Tool (..),
    Config (..),
    emptyConfig,
    renderProcBtn,
    renderToolBtn
  ) where

import Prelude hiding (concat)
import Data.Text (fromString, Text, concat, (<>))


data ProcessButton = ProcessButton { getProcBtnStyle  :: Text,
                                     getProcBtnPrompt :: Text,
                                     getProcList      :: Text,
                                     getProcBtnTitle  :: Text }

data Tool = Tool { getToolName      :: Text,
                   getToolStyle     :: Text,
                   getToolModalFile :: Text }



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
renderProcBtn btn = concat ["<button class=\"uk-button " <> sty <> "\"",
                                   " data-prompt=\""<> prompt <> "\"",
                                   " data-proc=\"" <> procList <> "\">",
                                   title,
                                   "</button>"]
  where sty = getProcBtnStyle btn
        prompt = getProcBtnPrompt btn
        procList = getProcList btn
        title = getProcBtnTitle btn

renderToolBtn :: Tool -> Text
renderToolBtn btn = concat ["<button class=\"uk-button " <> sty <> "\"",
                                   " data-modal=\"" <> modalName <> "\">",
                                   name,
                                   "</button>"]
  where sty = getToolStyle btn
        name = getToolName btn
        modalName = getToolModalFile btn
