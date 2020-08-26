{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Utils
  ( isTextFile
  , getMode
  , canProc
  ) where

import           Data.Text (Text, fromString)
import           FilePath  (FilePath)
import           Prelude   (Bool, otherwise)
import           Regex     (Flag (I), Regex, newRegex_, test)

isTextFile :: FilePath -> Bool
isTextFile = test (newRegex_ "\\.(json|js|html|markdown|md|rst|css|htm|xml|txt|conf|py|csv|tex|aux|log|out|sh)$" [I])

canProc :: FilePath -> Bool
canProc = test (newRegex_ "\\.(js|sh|py)$" [I])

modeMap :: [(Text, Regex)]
modeMap = [ ("javascript", newRegex_ "\\.js$" [I]),
            ("markdown", newRegex_ "\\.(md|markdown|rst)$" [I]),
            ("html", newRegex_ "\\.(html|htm)$" [I]),
            ("css", newRegex_ "\\.css$" [I]),
            ("yaml", newRegex_ "\\.(yaml|yml)$" [I]),
            ("xml", newRegex_ "\\.(svg|xml)$" [I]),
            ("json", newRegex_ "\\.json$" [I]),
            ("python", newRegex_ "\\.py$" [I]),
            ("tex", newRegex_ "\\.(tex|aux)$" [I]),
            ("sh", newRegex_ "\\.sh$" [I])
            ]

getMode :: FilePath -> Text
getMode = go modeMap
  where go :: [(Text, Regex)] -> Text -> Text
        go [] _ = "text"
        go ((mode, reg):xs) f | test reg f = mode
                              | otherwise  = go xs f
