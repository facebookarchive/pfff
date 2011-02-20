(** 
   class goog.editor.plugins.LinkDialogPlugin

   @author Gabriel Cardoso
*)

#ifndef PLUGINS
open Js
open AbstractDialogPlugin
#endif

class type linkDialogPlugin = object
  inherit abstractDialogPlugin

  method disposeInternal : unit meth

(**
 * @return The ID unique to this plugin class. Note that different
 *     instances off the plugin share the same classId.
 *)
  method getTrogClassId : js_string t meth

(**
 * Sets the warning message to show to users about including email addresses on
 * public web pages.
 * @param emailWarning Warning message to show users about including
 *     email addresses on the web.
 *)
  method setEmailWarning : js_string t -> unit meth

(**
 * Tells the plugin to stop leaking the page's url via the referrer header when
 * the "test this link" link is clicked. When the user clicks on a link, the
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
 * A plugin that opens the link dialog.
 * @constructor
 * @extends abstractDialogPlugin
 *)
val linkDialogPlugin : linkDialogPlugin t constr
