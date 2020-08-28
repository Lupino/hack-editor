{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module ACEditor
  ( Editor
  , newEditor
  , setTheme
  , setMode
  , setValue
  , getValue
  , addEvent
  , removeAllEvent
  ) where

import           Data.Text (Text, fromString)
import           DOM       (Event)
import           FFI       (ffi)
import           Prelude

data Editor

newEditor :: Text -> Fay Editor
newEditor = ffi "ace['edit'](%1)"

setTheme :: Text -> Editor -> Fay Editor
setTheme = ffi "(function(theme, editor){editor['setTheme']('ace/theme/' + theme); return editor;})(%1, %2)"

setMode :: Text -> Editor -> Fay Editor
setMode = ffi "(function(mode, editor){editor['getSession']()['setMode']('ace/mode/' + mode); return editor;})(%1, %2)"

setValue :: Text -> Editor -> Fay Editor
setValue = ffi "(function(value, editor) { editor['setValue'](value, -1); return editor })(%1, %2)"

getValue :: Editor -> Fay Text
getValue = ffi "(function(editor) { return editor['getValue']() })(%1)"

addEvent :: Text -> (Event -> Fay ()) -> Editor -> Fay Editor
addEvent = ffi "(function (evt, func, editor) { editor['on'](evt, func); return editor; })(%1, %2, %3)"

removeAllEvent :: Text -> Editor -> Fay Editor
removeAllEvent = ffi "(function(evt, editor) { editor['removeAllListeners'](evt); return editor; })(%1, %2)"
