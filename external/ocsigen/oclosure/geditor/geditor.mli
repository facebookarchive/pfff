#define EDITOR
open Js

#include "pluginField.mli"

module Command : sig
  val _UNDO : js_string t
  val _REDO : js_string t
  val _LINK : js_string t
  val _FORMAT_BLOCK : js_string t
  val _INDENT : js_string t
  val _OUTDENT : js_string t
  val _REMOVE_FORMAT : js_string t
  val _STRIKE_THROUGH : js_string t
  val _HORIZONTAL_RULE : js_string t
  val _SUBSCRIPT : js_string t
  val _SUPERSCRIPT : js_string t
  val _UNDERLINE : js_string t
  val _BOLD : js_string t
  val _ITALIC : js_string t
  val _FONT_SIZE : js_string t
  val _FONT_FACE : js_string t
  val _FONT_COLOR : js_string t
  val _EMOTICON : js_string t
  val _BACKGROUND_COLOR : js_string t
  val _ORDERED_LIST : js_string t
  val _UNORDERED_LIST : js_string t
  val _TABLE : js_string t
  val _JUSTIFY_CENTER : js_string t
  val _JUSTIFY_FULL : js_string t
  val _JUSTIFY_RIGHT : js_string t
  val _JUSTIFY_LEFT : js_string t
  val _BLOCKQUOTE : js_string t (* This is a nodename. Should be all caps. *)
  val _DIR_LTR : js_string t (* should be exactly 'ltr' as it becomes dir attribute value *)
  val _DIR_RTL : js_string t (* same here *)
  val _IMAGE : js_string t
  val _EDIT_HTML : js_string t

  (* queryCommandValue only: returns the default tag name used in the field.
     DIV should be considered the default if no plugin responds. *)
  val _DEFAULT_TAG : js_string t

  (* TODO(nicksantos): Try to give clients an API so that they don't need
     these execCommands. *)
  val _CLEAR_LOREM : js_string t
  val _UPDATE_LOREM : js_string t
  val _USING_LOREM : js_string t

  (* Modal editor commands (usually dialogs). *)
  val _MODAL_LINK_EDITOR : js_string t
end

module Plugins : sig 
#include "plugins/plugins.mli"
end
