module TermManager
  ( TermManager
  , newTermManager
  , openTerm
  , closeTerm
  , termSend
  ) where

import           Data.Text (Text)
import           FFI       (ffi)
import           Prelude
import           ProcAPI   (ProcAPI)

data TermManager

newTermManager :: Text -> ProcAPI -> Fay () -> Fay TermManager
newTermManager =  ffi "new JSTermManager(%1, %2, %3)"

openTerm :: TermManager -> Fay ()
openTerm = ffi "%1['init']()"

closeTerm :: TermManager -> Fay ()
closeTerm = ffi "%1['close']()"

termSend :: TermManager -> Text -> Fay ()
termSend = ffi "%1['send'](%2)"
