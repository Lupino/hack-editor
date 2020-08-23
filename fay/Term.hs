module Term
  ( TermManager
  , newTermManager
  , openTerm
  , closeTerm
  ) where

import           DOMUtils (Modal)
import           FFI      (ffi)
import           Prelude

data TermManager

newTermManager :: Modal -> Fay TermManager
newTermManager =  ffi "new TermManager(%1)"

openTerm :: TermManager -> Fay ()
openTerm = ffi "(function(t) {t.init()})(%1)"

closeTerm :: TermManager -> Fay ()
closeTerm = ffi "(function(t) {t.close()})(%1)"
