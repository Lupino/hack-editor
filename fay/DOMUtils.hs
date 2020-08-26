{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module DOMUtils
  (
    setProp,
    removeProp,
    getProp,
    querySelector,
    querySelectorAll,
    setHtml,
    setDisplay,
    addEventListener,
    windowAddEventListener,
    clearEventListeners,
    getEventTargetAttr,
    Modal,
    getModal,
    showModal,
    hideModal,
    modalEvent,
    prompt,
    confirm_,
    confirm,
    notify,
    saveAs
  ) where

import           Data.Text (Text, fromString)
import           DOM       (Element, Event)
import           FFI       (ffi)
import           Prelude

setProp :: Text -> Text -> Element -> Fay Element
setProp = ffi "(function(prop, val, elem) { elem[prop] = val; return elem })(%1, %2, %3)"

removeProp :: Text -> Element -> Fay Element
removeProp = ffi "(function(prop, elem) { elem[prop] = null; return elem })(%1, %2)"

getProp :: Text -> Element -> Fay Text
getProp = ffi "%2[%1]"

querySelector :: Text -> Fay Element
querySelector = ffi "document['querySelector'](%1)"

querySelectorAll :: Text -> Fay [Element]
querySelectorAll = ffi "document['querySelectorAll'](%1)"

setHtml :: Text -> Element -> Fay Element
setHtml = ffi "(function(text, elem) { elem['innerHTML'] = text; return elem; })(%1, %2)"

setDisplay :: Text -> Element -> Fay Element
setDisplay = ffi "(function (val, elem) { elem['style']['display'] = val; return elem })(%1, %2)"

addEventListener :: Text -> (Event -> Fay a) -> Element ->  Fay Element
addEventListener = ffi "(function(evt, func, elem) {elem['addEventListener'](evt, func); return elem;})(%1, %2, %3)"

windowAddEventListener :: Text -> (Event -> Fay ()) -> Fay ()
windowAddEventListener = ffi "(function(evt, func) {window.addEventListener(evt, func);})(%1, %2)"

clearEventListeners :: Element -> Fay Element
clearEventListeners = ffi "(function(elem) {\
                          \  var elem_ = elem['cloneNode'](true);\
                          \  elem['parentNode']['replaceChild'](elem_, elem);\
                          \})(%1)"

getEventTargetAttr :: Text -> Event -> Fay Text
getEventTargetAttr = ffi "(function(txt, evt) {\
                    \  var elem = evt['target'];\
                    \  var attr = elem['getAttribute'](txt);\
                    \  if (attr) {\
                    \    return attr;\
                    \  } else {\
                    \    return '';\
                    \  }\
                    \})(%1, %2)"

data Modal

getModal :: Text -> Fay Modal
getModal = ffi "UIkit['modal'](%1)"

showModal :: Modal -> Fay ()
showModal = ffi "%1['show']()"

hideModal :: Modal -> Fay ()
hideModal = ffi "%1['hide']()"

modalEvent :: Text -> Fay () -> Fay () -> Fay ()
modalEvent = ffi "(function (elem, show, hide) {\
                 \  $(elem).on({\
                 \    'show.uk.modal': show,\
                 \    'hide.uk.modal': hide\
                 \  })\
                 \})(%1, %2, %3)"

prompt :: Text -> Text -> (Text -> Fay ()) -> Fay ()
prompt msg val doPrompt = do
  void $ querySelector "#modal-prompt" >>= clearEventListeners

  void $ querySelector "#modal-prompt .prompt" >>= setHtml msg
  void $ querySelector "#modal-prompt input" >>= setProp "value" val
  void $ querySelector "#modal-prompt .js-modal-ok"
      >>= addEventListener "click" (const doPrompt')
      >>= addEventListener "click" (const hide)

  getModal "#modal-prompt" >>= showModal

  where hide = getModal "#modal-prompt" >>= hideModal
        doPrompt' :: Fay ()
        doPrompt' =
          doPrompt =<< getProp "value" =<< querySelector "#modal-prompt input"

confirm_ :: Text -> Fay () -> Fay () -> Fay ()
confirm_ msg doConfirm doCancel = do
  void $ querySelector "#modal-confirm" >>= clearEventListeners

  void $ querySelector "#modal-confirm .uk-modal-content" >>= setHtml msg
  void $ querySelector "#modal-confirm .modal-confirm-cancel"
      >>= addEventListener "click" (const doCancel)
      >>= addEventListener "click" (const hide)

  void $ querySelector "#modal-confirm .modal-confirm"
      >>= addEventListener "click" (const doConfirm)
      >>= addEventListener "click" (const hide)

  getModal "#modal-confirm" >>= showModal

  where hide = getModal "#modal-confirm" >>= hideModal

confirm :: Text -> Fay () -> Fay ()
confirm msg doConfirm = confirm_ msg doConfirm (return ())

notify :: Text -> Fay ()
notify = ffi "UIkit['notify'](%1)"

saveAs :: Text -> Fay ()
saveAs = ffi "(function(fn){saveAs('/api/file' + fn, fn.substr(1))})(%1)"
