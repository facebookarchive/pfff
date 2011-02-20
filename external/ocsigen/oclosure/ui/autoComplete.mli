(*
   OClosure Project - 2010

   Class goog.ui.AutoComplete
   
   @author : Oran Charles
   @version 0.2
*)
#ifndef UI
open Js
#endif
open Tools

(** This is the central manager class for an AutoComplete instance. *)
class type autoComplete = object
  inherit Events.eventTarget 
  
  (** Returns the renderer that renders/shows/highlights/hides the autocomplete menu.
      @return Renderer used by the this widget. *)
  method getRenderer : Events.eventTarget meth
    
  (** Generic event handler that handles any events this object is listening to. 
      @param e Event Object. *)
  method handleEvent : Events.event t -> unit meth
    
  (** Sets the max number of matches to fetch from the Matcher. 
      @param max Max number of matches. *)
  method setMaxMatches : int -> unit meth
    
  (** Sets whether or not the first row should be highlighted by default. 
     @param autoHilite true iff the first row should be highlighted by default. *)
  method setAutoHilite : bool t -> unit meth
    
  (** Sets whether or not the up arrow can unhilite all rows. 
      @param allowFreeSelect true iff the up arrow can unhilite all rows. *)
  method setAllowFreeSelect : bool t -> unit meth
 
  (** Sets whether or not to request new suggestions immediately after completion of a suggestion. 
      @param triggerSuggestionsOnUpdate true iff completion should fetch new suggestions. *)
  method setTriggerSuggestionsOnUpdate : bool t -> unit meth

  (** Sets the token to match against.  This triggers calls to the Matcher to fetch the matches (up to maxMatches), and then it triggers a call to <code>renderer.renderRows()</code>. 
      @param token The string for which to search in the Matcher.
      @param opt_fullString Optionally, the full string in the input field. *)
  method setToken : js_string t -> js_string t opt -> unit meth

  (** Gets the current target HTML node for displaying autocomplete UI. 
      @return The current target HTML node for displaying autocomplete UI. *)
  method getTarget : js_string t meth

  (** Sets the current target HTML node for displaying autocomplete UI. Can be an implementation specific definition of how to display UI in relation to the target node. 
      @param target The current target HTML node for displaying autocomplete UI. *)
  method setTarget : #Dom_html.element t -> unit meth
    
  (** Whether the autocomplete's renderer is open. 
      @return Whether the autocomplete's renderer is open. *)
  method isOpen : bool t meth
    
  (** Moves the hilite to the next row, or does nothing if we're already at the end of the current set of matches. Calls renderer.hiliteId() when there's something to do. 
      @return Returns true on a successful hilite. *)
  method hiliteNext : bool t meth

  (** Moves the hilite to the previous row, or does nothing if we're already at the beginning of the current set of matches. Calls renderer.hiliteId() when there's something to do.
      @return Returns true on a successful hilite. *)
  method hilitePrev : bool t meth

  (** Hilites the id if it's valid, otherwise does nothing. 
      @param id A row id (not index).
      @return  Whether the id was hilited. *)
  method hiliteId : int -> bool meth

  (** If there are any current matches, this passes the hilited row data to <code>selectionHandler.selectRow()</code> 
      @return Whether there are any current matches. *)
  method selectHilited : bool t meth
    
  (** Clears out the token, rows, and hilite, and calls <code>renderer.dismiss()</code> *)
  method dismiss : unit meth

  (** Call a dismiss after a delay, if there's already a dismiss active, ignore. *)
  method dismissOnDelay : unit meth
    
  (** Call a dismiss after a delay, if there's already a dismiss active, ignore. *)
  method cancelDelayedDismiss : unit meth
    
 (** Cleans up the autocomplete object. *)
  method disposeInternal : unit meth
    
  (** Renders the rows and adds highlighting. 
      @param rows Set of data that match the given token.
      @param opt_preserveHilited If true, keeps the currently hilited (by index) element hilited. *)
  method renderRows : js_string t array -> bool t opt -> unit meth

 (* attachInputs / detachInputs *)
 
end

(*let autocomplete : (obj -> eventTarget -> obj -> autocomplete t) constr = 
  Js.Unsafe.variable "goog.ui.AutoComplete"*)

  
module AutoComplete : sig

  (** Events associated with the autocomplete *)
  module EventType : sig 
	(** A row has been highlighted by the renderer. *)
    val _HILITE : js_string t
      
    (** A row has been selected by the renderer. *)
    val _SELECT : js_string t
      
    (**  A dismiss event has occurred. *)
    val _DISMISS : js_string t
      
    (** Event that cancels a dismiss event. *)
    val _CANCEL_DISMISS : js_string t
      
    (** Field value was updated.  A row field is included and is non-null when a row has been selected.  The value of the row typically includes fields: contactData and formattedValue as well as a toString function (though none of these fields are guaranteed to exist).  The row field may be used to return custom-type row data. *)
    val _UPDATE : js_string t
  end

  (**  Class goog.ui.AutoComplete.ArrayMatcher *)
  class type arrayMatcher = object 
    (** Function used to pass matches to the autocomplete. 
	@param token Token to match.
	@param maxMatches Max number of matches to return.
	@param matchHandler callback to execute after matching.
	@param opt_fullString The full string from the input box. *)
    method requestMatchingRows : js_string t -> int -> (unit -> unit) -> js_string opt -> unit meth
      
    (** Matches the token against the start of words in the row. 
	@param token Token to match.
	@param maxMatches Max number of matches to return.
	@return Rows that match. *)
    method getPrefixMatches : js_string t -> int -> js_string t array
      
    (** Matches the token against similar rows, by calculating "distance" between the terms. 
	@param token Token to match.
	@param  maxMatches Max number of matches to return.
	@return The best maxMatches rows. *)
    method getSimilarRows : js_string t -> int -> js_string t array
  end
	    
  (** Basic class for matching words in an array. 
      @param rows Dictionary of items to match.  Can be objects if they have a toString method that returns the value to match against.
      @param opt_noSimilar if true, do not do similarity matches for the input token against the dictionary. *)
  val arrayMatcher : (js_string t array -> bool t opt -> arrayMatcher t) constr

  (** Class goog.ui.AutoComplete.InputHandler *)
  class type inputHandler = object
    inherit Disposable.disposable 
      
    (** Attach an instance of an AutoComplete
	@param ac Autocomplete object. *)
    method attachAutoComplete : autoComplete t -> unit meth 
      
    (** Returns the associated autocomplete instance.
	@return The associated autocomplete instance. *)
    method getAutoComplete : autoComplete t meth
      
    (** Returns the current active element.
	@return The currently active element. *)
    method getActiveElement : Dom_html.element t meth
      
    (** Returns the value of the current active element.
	@return The value of the current active element. *)
    method getValue : js_string t meth
      
    (** Sets the value of the current active element.
	@param value The new value. *)
    method setValue : int -> unit meth
      
    (** Returns the current cursor position.
	@return The index of the cursor position. *)
    method getCursorPosition : int meth
      
    (** Sets the cursor at the given position.
	@param pos The index of the cursor position. *)
    method setCursorPosition : int -> unit meth
      
    (** Attaches the input handler to an element such as a textarea or input box.
	The element could basically be anything as long as it exposes the correct
	interface and events.
	@param el An element to attach the input handler too. *)
    method attachInput : #Dom_html.element t -> unit meth
      
    (** Detaches the input handler from the provided element.
	@param el An element to detach the input handler from. *)
    method detachInput : #Dom_html.element t -> unit meth
      
    (*(** Selects the given row.  Implements the SelectionHandler interface.
      @param row The row to select.
      @param opt_multi Should this be treated as a single or multi-token auto-complete?  Overrides previous setting of opt_multi on constructor.
      @return Whether to suppress the update event. *)
      method selectRow : ?????*)
      
    (** Disposes of the input handler. *)
    method disposeInternal : unit meth
      
    (** Sets the entry separator characters.
	@param separators The separator characters to set. *)
    method setSeparators : js_string t -> unit meth

    (**  Sets whether to flip the orientation of up & down for hiliting next and previous autocomplete entries.
	 @param upsideDown Whether the orientation is upside down. *)
    method setUpsideDown : bool t -> unit meth 

    (** Sets whether auto-completed tokens should be wrapped with whitespace.
	@param newValue boolean value indicating whether or not auto-completed tokens should be wrapped with whitespace. *)
    method setWhitespaceWrapEntries : bool t -> unit meth
      
    (** Sets whether new tokens should be generated from literals. That is, should hello'world be two tokens, assuming ' is a literal?
	@param newValue boolean value indicating whether or not new tokens should be generated from literals. *)
    method setGenerateNewTokenOnLiteral : bool t -> unit meth

    (** Sets the regular expression used to trim the tokens before passing them to the matcher:  every substring that matches the given regular expression will be removed. This can also be set to null to disable trimming.
	@param trimmer Regexp to use for trimming or null to disable it. *)
    method setTrimmingRegExp : regExp t -> unit meth
      
    (** Sets whether we will prevent the default input behavior (moving focus to the next focusable  element) on TAB.
	@param newValue Whether to preventDefault on TAB. *)
    method setPreventDefaultOnTab : bool t -> unit meth
      
    (** Sets whether separators perform autocomplete.
	@param newValue Whether to autocomplete on separators. *)
    method setSeparatorCompletes : bool t -> unit meth

    (** Sets whether separators perform autocomplete.
	@param newValue Whether to autocomplete on separators. *)
    method setSeparatorSelects : bool t -> unit meth

    (** Gets the time to wait before updating the results. If the update during typing flag is switched on, this delay counts from the last update, otherwise from the last keypress.
	@return Throttle time in milliseconds. *)
    method getThrottleTime : int meth

    (** Sets whether a row has just been selected.
	@param justSelected Whether or not the row has just been selected. *)
    method setRowJustSelected : bool t -> unit meth

    (** Sets the time to wait before updating the results.
	@param time New throttle time in milliseconds. *)
    method setThrottleTime : int -> unit meth

    (** Gets whether the result list is updated during typing.
	@return Value of the flag. *)
    method getUpdateDuringTyping : bool t meth

    (** Sets whether the result list should be updated during typing.
	@param value New value of the flag. *)
    method setUpdateDuringTyping : bool t -> unit meth

    (** Checks if an update has occurred and notified the autocomplete of the new token.
	@param opt_force If true the menu will be forced to update. *)
    method update : bool t opt -> unit meth
  end


  (** Class for managing the interaction between an auto-complete object and a text-input or textarea.
      
     @param opt_separators Separators to split multiple entries.
     @param opt_literals Characters used to delimit text literals.
     @param opt_multi Whether to allow multiple entries (Default: true).
     @param opt_throttleTime Number of milliseconds to throttle keyevents with 
     (Default: 150). Use -1 to disable updates on typing. Note that typing the 
     separator will update autocomplete suggestions. *)
  val inputHandler : (js_string t opt -> js_string t opt -> bool t opt -> int -> inputHandler t) constr

  module InputHandler : sig
    (** Standard list separators. *)
    val _STANDARD_LIST_SEPARATORS : js_string t

    (** Literals for quotes. *)  
    val _QUOTE_LITERALS : js_string t
  end 

  (** Class goog.ui.AutoComplete.Basic *)
  class type basic = object
    inherit autoComplete
  end
	    
  (** Factory class for building a basic autocomplete widget that autocompletes an inputbox or text area from a data array. 
      @param data Data array.
      @param input Input element or text area.
      @param opt_multi Whether to allow multiple entries separated with semi-colons or colons.
      @param opt_useSimilar use similar matches. e.g. "gost" => "ghost". *)
  val basic : (string js_array t -> #Dom_html.element t -> bool t opt -> bool t opt -> basic t) constr 

  (**  Class goog.ui.AutoComplete.Remote *)
  class type remote = object 
    inherit autoComplete
      (** Set whether or not standard highlighting should be used when rendering rows.
	  @param useStandardHighlighting true if standard highlighting used.*)
    method setUseStandardHighlighting : bool t -> unit meth
      
    (** Gets the attached InputHandler object.
	@return The input handler *)
    method getInputHandler : inputHandler t meth
      
    (** Set the send method ("GET", "POST") for the matcher. 
	@param method The send method; default: GET. *)
    method setMethod : js_string t -> unit meth 
      
    (** Set the post data for the matcher. 
	@param content Post data. *)
    method setContent : js_string t -> unit meth
      
    (** Set the HTTP headers for the matcher. 
	@param headers Map of headers to add to the request. *)
    method setHeaders : #Dom_html.element t -> unit meth
      
    (** Set the timeout interval for the matcher.
	@param interval Number of milliseconds after which an incomplete request will be aborted; 0 means no timeout is set. *)
    method setTimeoutInterval : float -> unit meth  
  end

  (** Factory class for building a remote autocomplete widget that autocompletes
     an inputbox or text area from a data array provided via ajax. 
      
     @param url The Uri which generates the auto complete matches.
     @param input Input element or text area.
     @param opt_multi Whether to allow multiple entries; defaults to false.
     @param opt_useSimilar Whether to use similar matches; e.g. 
     "gost" => "ghost". *)
  val remote : (js_string t -> #Dom_html.element t -> bool t opt -> bool t opt -> remote t) constr 

  (**  Class goog.ui.AutoComplete.Remote *)
  class type richRemote = object 
    inherit autoComplete
      (** Set the filter that is called before the array matches are returned.
	  @param rowFilter A function(rows) that returns an array of rows as a subset of the rows input array. *)
    method setRowFilter : (unit -> unit) callback -> unit meth
      
  end

  (** Factory class to create a rich autocomplete widget that autocompletes an 
     inputbox or textarea from data provided via ajax.  The server returns a 
     complex data structure that is used with client-side javascript functions 
     to render the results.
     @param url The Uri which generates the auto complete matches.
     @param input Input element or text area.
     @param opt_multi Whether to allow multiple entries; defaults to false.
     @param opt_useSimilar Whether to use similar matches; 
     e.g. "gost" => "ghost". *)
  val richRemote : (js_string t -> #Dom_html.element t -> bool t opt -> bool t opt -> richRemote t) constr
 
end
