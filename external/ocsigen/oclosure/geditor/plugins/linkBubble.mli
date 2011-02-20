(**
   class goog.editor.plugins.LinkBubble
   class goog.editor.plugins.LinkBubble.Action

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open AbstractBubblePlugin
#endif

module LinkBubble : sig
  class type action = object end

(**
 * Constructor for extra actions that can be added to the link bubble.
 * @param spanId The ID for the span showing the action.
 * @param linkId The ID for the link showing the action.
 * @param message The text for the link showing the action.
 * @param toShowFn Test function to determine whether
 *     to show the action for the given URL.
 * @param actionFn Action function to run when the
 *     action is clicked.  Takes the current target URL as a parameter.
 *)
  val action : js_string t -> js_string t -> js_string t 
    -> (js_string t -> bool t) callback -> (js_string t -> unit) callback 
      -> action t constr
end

class type linkBubble = object
  inherit abstractBubblePlugin

(**
 * Should be overriden by subclasses to add the type specific contents to the
 *     bubble.
 * @param bubbleContainer The container element of the bubble to
 *     which the contents should be added.
 *)
  method createBubbleContents : #Dom_html.element t -> unit meth

(**
 * Should be overriden by subclasses to return the bubble target element or
 * null if an element of their required type isn't found.
 * @param selectedElement The target of the selection change event or
 *     the parent container of the current entire selection.
 * @return The HTML bubble target element or null if no element of
 *     the required type is not found.
 *)
  method getBubbleTargetFromSelection : #Dom_html.element t 
    -> Dom_html.element t opt meth

(**
 * @return The title for bubble shown by this plugin.  Defaults to no
 *     title.  Should be overridden by subclasses.
 *)
  method getBubbleTitle : js_string t meth

(**
 * @return The type of bubble shown by this plugin.  Usually the tag
 *     name of the element this bubble targets.
 * @protected
 *)
  method getBubbleType : js_string t meth

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Returns whether the URL should be considered invalid.  This always returns
 * false in the base class, and should be overridden by subclasses that wish
 * to impose validity rules on URLs.
 * @param url The url to check.
 * @return Whether the URL should be considered invalid.
 *)
  method isInvalidUrl : bool t meth

(**
 * Set the optional function for getting the "test" link of a url.
 * @param func The function to use.
 *)
  method setTestLinkUrlFn : (js_string t -> js_string t) callback -> unit meth

(**
 * Tells the plugin to stop leaking the page's url via the referrer header when
 * the link text link is clicked. When the user clicks on a link, the
 * browser makes a request for the link url, passing the url of the current page
 * in the request headers. If the user wants the current url to be kept secret
 * (e.g. an unpublished document), the owner of the url that was clicked will
 * see the secret url in the request headers, and it will no longer be a secret.
 * Calling this method will not send a referrer header in the request, just as
 * if the user had opened a blank window and typed the url in themselves.
 *)
  method stopReferrerLeaks : unit meth
end

(**
 * Property bubble plugin for links.
 * @param var_args List of
 *     extra actions supported by the bubble.
 * @constructor
 *)
val linkBubble : (LinkBubble.action t js_array t -> linkBubble t) constr
