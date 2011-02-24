(* Addresses indicate the senders and recipients of messages and
 * correspond to either an individual mailbox or a group of 
 * mailboxes.
 *)

type local_part = string
type domain = string

type addr_spec = local_part * domain option

class mailbox
  ?(name : string option) (route : string list) (spec : addr_spec) =
object
  method name = match name with Some s -> s | _ -> raise Not_found
  method route = route 
  method spec = spec 
end

class group
  (name : string) (mailboxes : mailbox list) =
object
  method name = name
  method mailboxes = mailboxes
end

type t =
  [ `Mailbox of mailbox
  | `Group of group
  ]

let mbox_addr_spec spec =
  `Mailbox
    (new mailbox [] spec)
    
let mbox_route_addr personal (route, spec) =
  `Mailbox
    (new mailbox ?name:personal route spec)

open Mimestring

let rev = List.rev

exception Parse_error of int * string

let parse string =
  let scanner = create_mime_scanner
    ~specials:specials_rfc822
    ~scan_options:[]
    string
  in
  
  (* manage lookahead token *)
  let lookahead_et, lookahead =
    let et, t = Mimestring.scan_token scanner in
    ref et, ref t
  in
  
  let next () =
    let et, t = Mimestring.scan_token scanner in
    lookahead_et := et;
    lookahead := t
  in
  let peek () = !lookahead in
  
  (* parsing error - some kind of location/error recovery? *)
  let error s = 
    let pos = Mimestring.get_pos !lookahead_et in
    raise (Parse_error (pos, s)) in

  (* parse a list of elements *)
  let list elem next acc = next (elem () :: acc) in

  (* match a special token for a character *)
  let special c =
    match peek () with
      | Special c' when c = c' -> next ()
      | _ -> error (Printf.sprintf "expecting '%c'" c)
  in
  
  (* main entry point  *)
  let rec address_list acc =
    match peek () with
      | End                 -> rev acc
      | _                   -> list address next_address acc
	  
  and next_address acc = 
    match peek () with
      | End                 -> rev acc
      | Special ','         -> next (); address_list acc
      | _                   -> error "expecting ','"
	  
  (* RFC-1123 section 5.2.15: syntax definition of "mailbox" is changed
     to allow route address with no phrase *)
	  
  and address () =
    match peek () with
      | (Atom _ | QString _) -> address1 ()
      | Special '<'          -> mbox_route_addr None (route_addr ())
      | Special ','          -> next (); address ()
          (* RFC 2822 section 4.4: support for "null" members *)
      | _                    -> error "expecting address"
	  
  and address1 () =
    let w0 = word () in
    match peek () with
      | Special '@'          -> mbox_addr_spec (w0, Some (at_domain ()))
      | Special ('<'|':')    -> address2 (w0)
      | Special '.'          -> next (); mbox_addr_spec (addr_spec [w0])
      | (Atom _ | QString _) -> address2 (phrase [w0])
      | _                    -> error "syntax error"
	  
  and address2 name =
    match peek () with
      | Special '<'         -> mbox_route_addr (Some name) (route_addr ())
      | Special ':'         -> next (); group name
      | _                   -> error "expecting '<' or ':'"
	  
  and group name =
    let mboxes = mailbox_list_opt () in
    special ';';
    `Group (new group name mboxes)
      
  and mailbox_list_opt () =
    match peek () with
      | Special ';'         -> []
      | _                   -> list mailbox next_mailbox []

  and next_mailbox acc =
    match peek () with
      | Special ','         -> next (); list mailbox next_mailbox acc
      | _                   -> rev acc
	  
  (* reuse parsing code for address () and filter out group response *)
  and mailbox () =
    match address () with
      | `Mailbox m -> m
      | _ -> error "expecting mailbox"
	  
  and route_addr () =
    special '<';
    let x = match peek () with
      | (Atom _ | QString _) ->
	  let spec = addr_spec [] in
	  ([], spec)
      | Special '@' ->
	  let r = route () in
	  let spec = addr_spec [] in
	  (r, spec)
      | _ -> error "expecting local part or route address"
    in
    special '>';
    x
      
  and route () =
    let r = at_domain_list [] in
    special ':';
    r
      
  and addr_spec acc =
    let lp = local_part acc in
    match peek () with
      | Special '@'         -> (lp, Some (at_domain ()))
      | _                   -> (lp, None)
	  
  and local_part acc = list word next_local_part acc
  and next_local_part acc =
    match peek () with
      | Special '.'         -> next (); local_part acc
      | _                   -> String.concat "." (rev acc)
	  
  and at_domain_list acc = list at_domain next_at_domain_list acc
  and next_at_domain_list acc =
    match peek () with
      | Special ','         -> next (); at_domain_list acc
      | _                   -> rev acc
	  
  and at_domain () = 
    special '@'; domain []
      
  and domain acc = list subdomain next_subdomain acc
  and next_subdomain acc =
    match peek () with
      | Special '.'         -> next (); domain acc
      | _                   -> String.concat "." (rev acc)
	  
  and subdomain () =
    match peek () with
      | Atom s              -> next (); s
      | DomainLiteral s     -> next (); s
      | _                   -> error "expecting atom or domain"
	  
  and phrase acc = list word_or_dot next_phrase acc
  and next_phrase acc =
    match peek() with
      | (Atom _ | QString _ | Special '.')
                            -> phrase acc
      | _                   -> String.concat " " (rev acc)

  (* RFC 2822 section 4.1: support for '.' often used for initials in names *)
  and word_or_dot () =
    match peek () with
      | Atom s              -> next (); s
      | QString s           -> next (); s
      | Special '.'         -> next (); "."
      | _                   -> error "expecting atom or quoted-string"

  and word () =
    match peek () with
      | Atom s              -> next (); s
      | QString s           -> next (); s
      | _                   -> error "expecting atom or quoted-string"
	  
  in
  address_list []
