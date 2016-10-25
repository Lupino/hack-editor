{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Utils
  (
    isTextFile,
    isPythonFile,
    isNodeFile
  ) where

import Prelude (Bool)
import Data.Text (fromString)
import FilePath (FilePath)
import Regex (newRegex_, test, Flag(I))

isTextFile :: FilePath -> Bool
isTextFile = test (newRegex_ "\\.(json|js|html|markdown|md|rst|css|htm|xml|txt|conf|py|csv|tex|aux|log|out)$" [I])

isPythonFile :: FilePath -> Bool
isPythonFile = test (newRegex_ "\\.py$" [I])

isNodeFile :: FilePath -> Bool
isNodeFile = test (newRegex_ "\\.js$" [I])
