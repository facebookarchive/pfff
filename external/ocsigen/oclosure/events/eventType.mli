#ifndef EVENTS
open Js
#endif

type eventType =
  (* Mouse events*)
  | CLICK
  | DBLCLICK
  | MOUSEDOWN
  | MOUSEUP
  | MOUSEOVER
  | MOUSEOUT
  | MOUSEMOVE
  | SELECTSTART

  (* Key events*)
  | KEYPRESS
  | KEYDOWN
  | KEYUP

  (* Focus*)
  | BLUR
  | FOCUS
  | DEACTIVATE
  (* TODO(user): Test these. I experienced problems with DOMFocusIn, the event*)
  (* just wasn't firing.*)
  | FOCUSIN
  | FOCUSOUT

  (* Forms*)
  | CHANGE
  | SELECT
  | SUBMIT

  (* Drag and drop*)
  | DRAGSTART
  | DRAGENTER
  | DRAGOVER
  | DRAGLEAVE
  | DROP

  (* Misc*)
  | CONTEXTMENU
  | ERROR
  | HASHCHANGE
  | HELP
  | LOAD
  | LOSECAPTURE
  | READYSTATECHANGE
  | RESIZE
  | SCROLL
  | UNLOAD

type eventType_pre

val of_eventType : eventType -> eventType_pre
