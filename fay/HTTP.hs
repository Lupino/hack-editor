{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module HTTP
  (
    RequestMethod (..),
    fetch,
    get,
    post,
    put,
    delete,
    resolveText
  ) where

import Prelude hiding (concat)
import DOM (XMLHttpRequest, send, xmlHttpRequest, responseText, status)
import FFI (ffi)
import Data.Text (fromString, Text, pack, concat)

import FPromise (Promise, newPromise, fromResolve, fromReject, toResolve,
                 Resolve, Reject, resolve)

data RequestMethod = GET | POST | PUT | DELETE

open :: RequestMethod -> Text -> XMLHttpRequest -> Fay XMLHttpRequest
open = ffi "(function(method, url, xhr) { xhr['open'](method['instance'], url, true); return xhr; })(%1, %2, %3)"

send_ :: Text -> XMLHttpRequest -> Fay ()
send_ = ffi "%2['send'](%1)"

setOnLoadHandler :: (XMLHttpRequest -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
setOnLoadHandler = ffi "(function(handler, xhr) { xhr['onload'] = function() { handler(xhr); }; return xhr; })(%1, %2)"

setOnErrorHandler :: (Text -> Fay ()) -> XMLHttpRequest -> Fay XMLHttpRequest
setOnErrorHandler = ffi "(function(handler, xhr) { xhr['onerror'] = function(e) { handler(e.toString()); }; return xhr; })(%1, %2)"

fetch :: RequestMethod -> Text -> Maybe Text -> Fay Promise
fetch method url body = newPromise doFetch
  where doFetch :: Resolve XMLHttpRequest () -> Reject -> Fay ()
        doFetch f g = xmlHttpRequest
                                    >>= open method url
                                    >>= setOnLoadHandler doResolve
                                    >>= setOnErrorHandler reject_
                                    >>= sendData body

          where resolve_ = fromResolve f
                reject_ = fromReject g

                sendData :: Maybe Text -> XMLHttpRequest -> Fay ()
                sendData (Just dt) = send_ dt
                sendData Nothing = send

                doResolve :: XMLHttpRequest -> Fay ()
                doResolve xhr = do
                  st <- status xhr

                  if st >= 400 then do
                    rt <- responseText xhr
                    reject_ (concat ["XHR returned status ", pack (show st), ":\n", rt])
                  else resolve_ xhr


get :: Text -> Fay Promise
get ur = fetch GET ur Nothing

post :: Text -> Maybe Text -> Fay Promise
post = fetch POST

put :: Text -> Maybe Text -> Fay Promise
put = fetch PUT

delete :: Text -> Fay Promise
delete ur = fetch DELETE ur Nothing

resolveText :: Resolve XMLHttpRequest Promise
resolveText = toResolve doResolve
  where doResolve :: XMLHttpRequest -> Fay Promise
        doResolve xhr = responseText xhr >>= resolve
