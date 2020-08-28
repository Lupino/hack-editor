{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module FPromise
  ( Promise
  , Reject
  , Resolve
  , fromReject
  , fromResolve
  , toReject
  , toResolve
  , newPromise
  , then_
  , then__
  , catch
  , resolve
  , reject
  ) where

import           Data.Text (Text, fromString)
import           FFI       (ffi)
import           Prelude

data Promise

newtype Reject = Reject (Text -> Fay ())

newtype Resolve a b = Resolve (a -> Fay b)

fromReject :: Reject -> Text -> Fay ()
fromReject (Reject f) = f

fromResolve :: Resolve a b -> a -> Fay b
fromResolve (Resolve f) = f

toReject :: (Text -> Fay ()) -> Reject
toReject = Reject

toResolve :: (a -> Fay b) -> Resolve a b
toResolve = Resolve

newPromise :: (Resolve a b -> Reject -> Fay ()) -> Fay Promise
newPromise cb = newPromise_ (f cb)
  where f :: (Resolve a b -> Reject -> Fay ()) -> (a -> Fay b) -> (Text -> Fay ()) -> Fay ()
        f f0 f1 g1 = f0 (toResolve f1) (toReject g1)

newPromise_ :: ((a -> Fay b) -> (Text -> Fay ()) -> Fay ()) -> Fay Promise
newPromise_ = ffi "new Promise(%1)"

then_ :: Resolve a b -> Promise -> Fay Promise
then_ f = then___ (fromResolve f) (const $ return ())

then__ :: Resolve a b -> Reject -> Promise -> Fay Promise
then__ f g = then___ (fromResolve f) (fromReject g)


then___ :: (a -> Fay b) -> (Text -> Fay ()) -> Promise -> Fay Promise
then___ = ffi "(function(resolve, reject, promise) {\
                    \   return promise['then'](resolve, reject);\
                    \})(%1, %2, %3)"

catch :: Reject -> Promise -> Fay Promise
catch f = catch_ (fromReject f)

catch_ :: (Text -> Fay ()) -> Promise -> Fay Promise
catch_ = ffi "(function(reject, promise) {\
           \   return promise['catch'](reject);\
           \})(%1, %2)"

resolve :: a -> Fay Promise
resolve = ffi "Promise['resolve'](%1)"

reject :: Text -> Fay Promise
reject = ffi "Promise['reject'](%1)"
