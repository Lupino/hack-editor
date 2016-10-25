{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Utils
  (
    isTextFile,
    isPythonFile,
    isNodeFile,
    getMode
  ) where

import Prelude (Bool, otherwise)
import Data.Text (fromString, Text)
import FilePath (FilePath)
import Regex (Regex, newRegex_, test, Flag(I))

isTextFile :: FilePath -> Bool
isTextFile = test (newRegex_ "\\.(json|js|html|markdown|md|rst|css|htm|xml|txt|conf|py|csv|tex|aux|log|out)$" [I])

isPythonFile :: FilePath -> Bool
isPythonFile = test (newRegex_ "\\.py$" [I])

isNodeFile :: FilePath -> Bool
isNodeFile = test (newRegex_ "\\.js$" [I])

modeMap :: [(Text, Regex)]
modeMap = [ ("javascript", newRegex_ "\\.js$" [I]),
            ("markdown", newRegex_ "\\.(md|markdown|rst)$" [I]),
            ("html", newRegex_ "\\.(html|htm)$" [I]),
            ("css", newRegex_ "\\.css$" [I]),
            ("yaml", newRegex_ "\\.(yaml|yml)$" [I]),
            ("xml", newRegex_ "\\.(svg|xml)$" [I]),
            ("json", newRegex_ "\\.json$" [I]),
            ("python", newRegex_ "\\.py$" [I]),
            ("txt", newRegex_ "\\.(tex|aux)$" [I])
            ]

getMode :: FilePath -> Text
getMode = go modeMap
  where go :: [(Text, Regex)] -> Text -> Text
        go [] _ = "text"
        go ((mode, reg):xs) f | test reg f = mode
                              | otherwise  = go xs f
