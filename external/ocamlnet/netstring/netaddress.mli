(* $Id: netaddress.mli 1003 2006-09-24 15:17:15Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Parsing of mail addresses *)

(** Addresses indicate the senders and recipients of messages and
 * correspond to either an individual mailbox or a group of 
 * mailboxes.
 *)

type local_part = string
  (** Usually the user name *)

type domain = string
  (** The domain of the mailbox *)

type addr_spec = local_part * domain option
  (** The pair [local_part\@domain] as O'Caml type. The domain may be
   * missing.
   *)


(** A [mailbox] has a name, optionally a route (not used nowadays), and
 * a formal address specification.
 *
 * Create a [mailbox] with
 *
 * [ new mailbox ~name route addr_spec ]
 *
 * Pass [route = []] if not used (formerly, routes were used to specify
 * the way the mail should take from the sender to the receiver, and
 * contained a list of hostnames/IP addresses).
 *)
class mailbox :
  ?name:string -> string list -> addr_spec -> 
object
  method name  : string
    (** The name of the mailbox. Raises [Not_found] if not set *)
  method route : string list
    (** The route to the mailbox *)
  method spec  : addr_spec
    (** The formal address specification *)
end

(** A [group] has a name, and consists of a number of mailboxes.
 *
 * Create a group with [new group name mailboxes].
 *)
class group :
  string -> mailbox list ->
object
  method name : string
    (** The name of the group *)
  method mailboxes : mailbox list
    (** The member mailboxes *)
end


(** The union of [mailbox] and [group]
 *)
type t =
  [ `Mailbox of mailbox
  | `Group of group
  ]

exception Parse_error of int * string
  (** A parsing error. The [int] is the position in the parsed string *)


val parse : string -> t list
  (** Parse a list of addresses in string representation, and return
   * them as list of mailboxes or groups.
   *
   * Examples:
   * - [parse "gerd\@gerd-stolpmann.de"] returns a single [mailbox]
   *   without name and route, and the given spec
   * - [parse "Gerd Stolpmann <gerd\@gerd-stolpmann.de>"] returns a
   *   single [mailbox] with name and spec, but without route
   * - [parse "abc\@def.net, ghi"] returns two [mailbox]es without
   *   name and route, and the two specs. The second address only
   *   has a local part, but no domain.
   * - [parse "g:abc\@def.net, Me <me\@domain.net>;, gs\@npc.de"]
   *   returns one group [g] with members [abc\@def.net] and 
   *   [me\@domain.net], and another [mailbox] [gs\@npc.de].
   *
   * Old-style naming of mailboxes is not supported
   * (e.g. "gerd\@gerd-stolpmann.de (Gerd Stolpmann)" - the part
   * in parentheses is simply ignored.
   *)
