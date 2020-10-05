{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Utils
  ( isTextFile
  , getMode
  , isExecutable
  , isImage
  , getShell
  ) where

import           Data.Text (Text, fromString)
import           FFI       (ffi)
import           FilePath  (FilePath)
import           Prelude   (Bool, otherwise)
import           Regex     (Flag (I), Regex, newRegex_, test)

isTextFile :: FilePath -> Bool
isTextFile = ffi "isTextFile(%1)"

isExecutable :: FilePath -> Bool
isExecutable = ffi "isExecutable(%1)"

getShell :: FilePath -> FilePath
getShell = ffi "getShell(%1)"

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

isImage :: FilePath -> Bool
isImage = test (newRegex_ "\\.(jpg|png|gif|jpeg|svg)$" [I])
