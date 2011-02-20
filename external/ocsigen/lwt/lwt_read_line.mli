(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_read_line
 * Copyright (C) 2009 Jérémie Dimino
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

(** Interactive line input *)

(** {6 Definitions} *)

exception Interrupt
  (** Exception raised when the user press [Ctrl^D] *)

type edition_state = Text.t * Text.t
    (** An edition state, it is a pair of two UTF-8 encoded strings:

        - the input before the cursor
        - the input after the cursor *)

type prompt = Lwt_term.styled_text
    (** A prompt. It may contains colors. *)

type text_set = Set.Make(Text).t

(** {8 Completion} *)

(** Result of a completion function: *)
type completion_result = {
  comp_state : edition_state;
  (** The new edition state *)

  comp_words : text_set;
  (** A list of possibilities *)
}

type completion = edition_state -> completion_result Lwt.t
      (** Type of a completion function. It takes as argument the
          current edition state.

          Note: the thread launched by the completion function is
          cancelled using {!Lwt.cancel} if the user continue typing
          text. *)

val lookup : Text.t -> text_set -> (Text.t * text_set)
  (** [lookup word words] lookup for completion of [word] into
      [words]. It returns [(prefix, possibilities)] where
      [possibilities] are all words starting with [word] and [prefix]
      is the longest common prefix of [possibilities]. *)

val complete : ?suffix : Text.t -> Text.t -> Text.t -> Text.t -> text_set -> completion_result
  (** [complete ?suffix before word after words] basic completion
      functions. [words] is a list of possible completions for
      [word].

      If completion succeed [suffix] is append to the resulting
      text. It defaults to [" "]. *)

val print_words : Lwt_text.output_channel -> int -> string list -> unit Lwt.t
  (** [print_words oc columns strs] pretty-prints a list of words. *)

(** {8 History} *)

type history = Text.t list
    (** Type of an history *)

val add_entry : Text.t -> history -> history
  (** [add_entry line history] returns the history [history] plus
      [line] at the beginning. If [line] already appears at the
      beginning or contains only spaces, it is discarded. *)

val save_history : string -> history -> unit Lwt.t
  (** [save_history filename history] saves [history] to
      [filename]. History is saved by separating lines with a null
      character. *)

val load_history : string -> history Lwt.t
  (** [load_history filename] loads history from [filename]. Returns
      the empty history if the the file does not exit. *)

(** {8 Clipboards} *)

(** Type of a clipboard. *)
class clipboard : object
  method set : Text.t -> unit
  method contents : Text.t React.signal
end

val clipboard : clipboard
  (** The global clipboard. All read-line instances which do not use a
      specific clipboard use this one. *)

(** {6 High-level functions} *)

type completion_mode = [ `classic | `real_time | `none ]
    (** The completion mode.

        - [`classic] means that when the user hit [Tab] a list of
          possible completions is proposed,

        - [`real_time] means that possible completions are shown to
          the user as he types, and he can navigate in them with
          [Meta+left], [Meta+right]

        - [`none] means no completion at all *)

val read_line :
  ?history : history ->
  ?complete : completion ->
  ?clipboard : clipboard ->
  ?mode : completion_mode ->
  ?prompt : prompt -> unit -> Text.t Lwt.t
  (** [readline ?history ?complete ?mode ?prompt ()] inputs some text
      from the user. If input is not a terminal, it defaults to
      [Lwt_text.read_line Lwt_text.stdin].

      If @param mode contains the current completion mode. It defaults
      to [`real_time].

      @param prompt defaults to [Lwt_term.Text "# "] *)

type password_style = [ `empty | `clear | `text of Text.t ]
    (** Style which indicate how the password is echoed to the user:

        - with [`empty] nothing is printed
        - with [`clear] the password is displayed has it
        - with [`text ch] all characters are replaced by [ch] *)

val read_password :
  ?clipboard : clipboard ->
  ?style : password_style ->
  ?prompt : prompt -> unit -> Text.t Lwt.t
  (** [read_password ?clipboard ?clear ~prompt ()] inputs a password
      from the user. This function fails if input is not a terminal.

      @param style defaults to [`text "*"].
  *)

val read_keyword :
  ?history : history ->
  ?case_sensitive : bool ->
  ?mode : completion_mode ->
  ?prompt : prompt ->
  values :  (Text.t * 'value) list -> unit -> 'value Lwt.t
  (** [read_keyword ?history ?case_sensitive ?mode ~prompt ~keywords
      ()] reads one word which is a member of [words]. And returns
      which keyword the user choosed.

      [case_sensitive] default to [false]. *)

val read_yes_no : ?history : history -> ?mode : completion_mode -> ?prompt : prompt -> unit -> bool Lwt.t
  (** [read_yes_no ?history ?dynamic prompt ()] is the same as:

      {[
        read_keyword ?history ?dynamic prompt [("yes", true); ("no", false)] ()
      ]}
  *)

(** {6 Low-level interaction} *)

(** This part allow you to implements your own read-line function, or
    just to use the readline engine in another context (message box,
    ...). *)

(** Readline commands *)
module Command : sig

  (** Type of all read-line function: *)
  type t =
    | Nop
        (** Command which do nothing. Unknown keys maps to this commands. *)
    | Char of Text.t
        (** Any printable character. *)
    | Backward_delete_char
    | Forward_delete_char
    | Beginning_of_line
    | End_of_line
    | Complete
    | Meta_complete
    | Kill_line
    | Backward_kill_line
    | Accept_line
    | Backward_delete_word
    | Forward_delete_word
    | History_next
    | History_previous
    | Break
    | Clear_screen
    | Insert
    | Refresh
    | Backward_char
    | Forward_char
    | Set_mark
    | Paste
    | Copy
    | Cut
    | Uppercase
    | Lowercase
    | Capitalize
    | Backward_word
    | Forward_word
    | Backward_search
    | Complete_left
    | Complete_right
    | Complete_up
    | Complete_down
    | Complete_first
    | Complete_last
    | Undo
    | Delete_char_or_list

  val to_string : t -> string
    (** [to_string cmd] returns a string representation of a command *)

  val of_string : string -> t
    (** [of_string cld] tries to convert a command name to a
        command. @raise Failure if it fails. *)

  val names : (t * string) list
    (** [names] is the list of all commands (except [Char ch]) with
        their name. *)

  val of_key : Lwt_term.key -> t
    (** [of_key key] returns the command to which a key is mapped. *)
end

(** Engine *)
module Engine : sig

  (** Note: this part know nothing about rendering or completion. *)

  (** State when the user is doing selection: *)
  type selection_state = {
    sel_text : Text.t;
    (** The whole input text on which the selection is working *)
    sel_mark : Text.pointer;
    (** Pointer to the mark *)
    sel_cursor : Text.pointer;
    (** Pointer to the cursor *)
  }

  (** State when searching in the history *)
  type search_state = {
    search_word : Text.t;
    (** The word we are looking for *)
    search_history : history;
    (** Position in history. The first element is a sentence
        containing the searched word *)
    search_init_history : history;
    (** The initial history, before searching for a word *)
  }

  (** The engine mode: *)
  type mode =
    | Edition of edition_state
        (** The user is typing some text *)
    | Selection of selection_state
        (** The user is selecting some text *)
    | Search of search_state
        (** The user is searching the given word in the history *)

  (** An engine state: *)
  type state = {
    mode : mode;
    history : history * history;
    (** Cursor to the history position. *)
  }

  val init : history -> state
    (** [init history] return a initial state using the given
        history *)

  val reset : state -> state
    (** [reset state] reset the given state, if the user was doing a
        selection, it is canceled *)

  val update : engine_state : state -> ?clipboard : clipboard -> command : Command.t -> unit -> state
    (** [update ~state ?clipboard ~command ()] update an engine state by
        processing the given command. It returns the new state, and
        may have the side effect of changing the clipboard contents.

        [clipboard] defaults to the global clipboard.
    *)

  val edition_state : state -> edition_state
    (** Returns the edition state of a state, whatever its mode is. *)

  val all_input : state -> Text.t
    (** Returns the current complete user input. *)
end

(** Rendering to the terminal *)
module Terminal : sig

  type state
    (** State of rendering *)

  val init : state
    (** Initial state *)

  (** The following functions are the one used by read-line functions
      of this module. *)

  (** Box for the completion: *)
  type box =
    | Box_none
        (** No box at all *)
    | Box_empty
        (** An empty box *)
    | Box_words of text_set * int
        (** [BM_words(words, position)] is a box with the given list
            of words. [position] is the position of the selected word
            in the list.. *)
    | Box_message of string
        (** A box containing only the given message *)

  val draw :
    columns : int ->
    ?map_text : (Text.t -> Text.t) ->
    ?box : box ->
    render_state : state ->
    engine_state : Engine.state ->
    prompt : prompt -> unit -> Lwt_term.styled_text * state
    (** [draw ~column ?map_text ?bar ~render_state ~engine_state
        prompt ()] returns [(text, state)] where [state] is the new
        rendering state, and [text] is a text containing escape
        sequences. When printed, it will update the displayed state.

        @param map_text is a function used to map user input before
          printing it, for example to hide passwords.
        @param message is a message to display if completion is not
          yet available.
        @param box defaults to {!Box_none}. *)

  val last_draw :
    columns : int ->
    ?map_text : (Text.t -> Text.t) ->
    render_state : state ->
    engine_state : Engine.state ->
    prompt : prompt -> unit -> Lwt_term.styled_text
    (** Draw for the last time, i.e. the cursor is left after the text
        and not at current position. *)

  val erase : columns : int -> render_state : state -> unit -> Lwt_term.styled_text
    (** [erase ~columns ~render_state ()] returns a text which will
        erase everything (the prompt, user input, completion, ...).

        After an erase, the rendering state is [init]. *)
end

(** {6 Advanced use} *)

(** Controlling a running read-line instance *)
module Control : sig

  type 'a t
    (** Type of a running read-line instance, returning a value of
        type ['a] *)

  (** {6 Control} *)

  val result : 'a t -> 'a Lwt.t
    (** Threads waiting for the read-line instance to terminates *)

  val send_command : 'a t -> Command.t -> unit
    (** [send_command instance command] sends the given command to the
        read-line instance *)

  val accept : 'a t -> unit
    (** [accept instance = send_command instance Command.Accept_line] *)

  val interrupt : 'a t -> unit
    (** [accept instance = send_command instance Command.Break] *)

  val hide : 'a t -> unit Lwt.t
    (** Hides everything (prompt, user input, completion box) until
        {!show} is called. *)

  val show : 'a t -> unit Lwt.t
    (** Un-hide everything *)

  (** Note: in case the input is not a terminal, read-line instances
      are not controllable. i.e. {!accept}, {!refresh}, ... have no
      effect. *)

  (** {6 Creation of read-line instances} *)

  type prompt = Engine.state React.signal -> Lwt_term.styled_text React.signal
    (** The prompt a signal which may depends on the engine state *)

  type state
    (** State of an instance *)

  val engine_state : state -> Engine.state
    (** Return the engine state of the given state *)

  val render_state : state -> Terminal.state
    (** Return the rendering state of the given state *)

  val make :
    ?history : history ->
    ?complete : completion ->
    ?clipboard : clipboard ->
    ?mode : [ completion_mode | `none ] ->
    ?map_text : (Text.t -> Text.t) ->
    ?filter : (state -> Command.t -> Command.t Lwt.t) ->
    map_result : (Text.t -> 'a Lwt.t) ->
    ?prompt : prompt -> unit -> 'a t
    (** Creates a new read-line instance with the given
        parameters. [filter] is called to handle commands. You can
        return {!Command.Nop} to drop a command. *)

  (** {6 Predefined instances} *)

  val read_line :
    ?history : history ->
    ?complete : completion ->
    ?clipboard : clipboard ->
    ?mode : completion_mode ->
    ?prompt : prompt -> unit -> Text.t t Lwt.t

  val read_password :
    ?clipboard : clipboard ->
    ?style : password_style ->
    ?prompt : prompt -> unit -> Text.t t Lwt.t

  val read_keyword :
    ?history : history ->
    ?case_sensitive : bool ->
    ?mode : completion_mode ->
    ?prompt : prompt ->
    values :  (Text.t * 'value) list -> unit -> 'value t Lwt.t

  val read_yes_no :
    ?history : history ->
    ?mode : completion_mode ->
    ?prompt : prompt -> unit -> bool t Lwt.t
end
