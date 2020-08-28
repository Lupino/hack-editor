{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Regex
  ( Regex
  , Flag (..)
  , newRegex
  , newRegex_
  , compile
  , compile_
  , exec
  , test
  , flags
  , global
  , ignoreCase
  , multiline
  , unicode
  , sticky
  ) where

import           Data.Text (Text, fromString)
import           FFI       (ffi)
import           Prelude

data Regex

-- flags = global ignoreCase multiline unicode sticky
data Flag = G | I | M | U | Y

newRegex :: Text -> Regex
newRegex = flip newRegex_ []

newRegex_ :: Text -> [Flag] -> Regex
newRegex_ = ffi "(function(txt, flags){\
               \  flags = flags['map'](function(flag) {\
               \    return flag['instance']['toLowerCase']();\
               \  });\
               \  return new RegExp(txt, flags['join'](''));\
               \})(%1, %2)"

compile :: Regex -> Text -> Regex
compile reg txt = compile_ reg txt []

compile_ :: Regex -> Text -> [Flag] -> Regex
compile_ = ffi "(function(reg, txt, flags){\
               \  flags = flags['map'](function(flag) {\
               \    return flag['instance']['toLowerCase']();\
               \  });\
              \  reg['compile'](txt, flags['join'](''));\
              \  return reg;\
              \})(%1, %2, %3)"

exec :: Regex -> Text -> [Text]
exec = ffi "(function(reg, txt) { return reg['exec'](txt) || [];})(%1, %2)"

test :: Regex -> Text -> Bool
test = ffi "%1['test'](%2)"

flags :: Regex -> [Flag]
flags = ffi "(function(reg){\
            \  return reg['flags']['split']('')['map'](function(flag){\
            \    return {instance: flag['toUpperCase']()};\
            \  });\
            \})(%1)"

global     :: Regex -> Bool
ignoreCase :: Regex -> Bool
multiline  :: Regex -> Bool
unicode    :: Regex -> Bool
sticky     :: Regex -> Bool
global     = ffi "%1['global']"
ignoreCase = ffi "%1['ignoreCase']"
multiline  = ffi "%1['multiline']"
unicode    = ffi "%1['unicode']"
sticky     = ffi "%1['sticky']"
