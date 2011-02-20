(**
  goog/ui/editor/defaulttoolbar.js file

  @author Gabriel Cardoso
*)

#ifndef UI_EDITOR
open Js
#endif
#ifndef UI
open Tools
open Control
open Select
open Toolbar
open Button
#endif

module DefaultToolbar : sig
(**
 * Initializes the given font size menu button by adding default font sizes to
 * it.
 * @param button Font size menu button.
 *)
  val addDefaultFontSizes : #select t -> unit

(**
 * Initializes the given font menu button by adding default fonts to the menu.
 * If goog.ui.editor.DefaultToolbar.setLocale was called to specify a locale
 * for which locale-specific default fonts exist, those are added before
 * common fonts.
 * @param button Font menu button.
 *)
  val addDefaultFonts : #select t -> unit

(**
 * Initializes the given "Format block" menu button by adding default format
 * options to the menu.
 * @param button "Format block" menu button.
 *)
  val addDefaultFormatOptions : #select t -> unit

(**
 * Creates an instance of a subclass of [goog.ui.Button] for the given
 * [goog.editor.Command], or null if no built-in button exists for the
 * command.  Note that this function is only intended to create built-in
 * buttons; please don't try to hack it!
 * @param command Editor command ID.
 * @param opt_domHelper DOM helper, used for DOM
 *     creation; defaults to the current document if unspecified.
 * @return Toolbar button (null if no built-in button exists
 *     for the command).
 *)
  val makeBuiltInToolbarButton : js_string t -> Gdom.domHelper t opt 
    -> #button t

(**
 * Creates a [goog.ui.Toolbar] containing a default set of editor
 * toolbar buttons, and renders it into the given parent element.
 * @param elem Toolbar parent element.
 * @param opt_isRightToLeft Whether the editor chrome is
 *     right-to-left; defaults to the directionality of the toolbar parent
 *     element.
 * @return Default editor toolbar, rendered into the given
 *     parent element.
 *)
  val makeDefaultToolbar : #Dom_html.element t -> bool t opt -> toolbar t

(**
 * Creates a [goog.ui.Toolbar] containing the specified set of
 * toolbar buttons, and renders it into the given parent element.  Each
 * item in the [items] array must either be a
 * [goog.editor.Command] (to create a built-in button) or a subclass
 * of [goog.ui.Control] (to create a custom control).
 * @param items Toolbar items; each must
 *     be a [goog.editor.Command] or a [goog.ui.Control].
 * @param elem Toolbar parent element.
 * @param opt_isRightToLeft Whether the editor chrome is
 *     right-to-left; defaults to the directionality of the toolbar parent
 *     element.
 * @return Editor toolbar, rendered into the given parent
 *     element.
 *)
  val makeToolbar : (js_string t, control t) Union.t js_array t
    -> #Dom_html.element t -> bool t opt -> toolbar t 


(**
 * Sets the locale for the font names.  If not set, defaults to 'en-us'.
 * Used only for default creation of font names name.  Must be set
 * before font name menu is created.
 * @param locale Locale to use for the toolbar font names.
 *)
  val setLocale : js_string t -> unit
end
