open User_sql.Types
open Wiki_types

(**
This is the wiki component of Ocsimore.

@author Jaap Boender
@author Piero Furiesi
@author Vincent Balat
*)

(** Exception raised when a wikipage already exists. The argument
   is the wikibox for the pre-existing page *)
exception Page_already_exists of wikibox

exception Css_already_exists



type 'a rights_sp =
  rights:Wiki_types.wiki_rights ->
  sp: Eliom_sessions.server_params ->
  'a



(** Creates the specified wiki if the user has enough permissions.
    Raise [Permission_denied] if it is not the case, and or fails with
    the same errors as [Wiki.create_wiki] if the wiki cannot be created.
    The options are the same as for the this function, except for
    the field [admin], which is used as the author of the container page,
    and becomes admin of the wiki. By default, no one can read
    the wiki.
*)
val create_wiki :
 (title:string ->
  descr:string ->
  ?path: string list ->
  ?staticdir:string ->
  ?boxrights:bool ->
  admins:user list ->
  readers: user list ->
  ?container_text:string ->
  model:Wiki_types.wiki_model ->
  unit ->
  wiki Lwt.t) rights_sp



val new_wikitextbox :
 ?db:Sql.db_t ->
 (content_type:'res Wiki_types.content_type ->
  wiki:wiki ->
  author:userid ->
  comment:string ->
  content:string ->
  unit -> wikibox Lwt.t) rights_sp


(** The next three functions save a wikibox and returns the new version id of
    this wikibox. For the css related functions, we check that the argument
    is indeed a proper css *)
val save_wikitextbox :
 (content_type:'res Wiki_types.content_type ->
  wb:wikibox ->
  content:string option ->
  int32 Lwt.t) rights_sp

val save_wikicssbox :
 (wiki:wiki ->
  content:string option ->
  wb:wikibox ->
  int32 Lwt.t) rights_sp

val save_wikipagecssbox :
 (wiki:wiki ->
  page:string ->
  content:string option ->
  wb:wikibox ->
  int32 Lwt.t) rights_sp



val set_wikibox_special_rights :
 (wb:wikibox ->
  special_rights:bool ->
  unit Lwt.t) rights_sp


val create_wikipage:
 (wiki:wiki ->
  page:string ->
  unit Lwt.t) rights_sp

val add_css:
  (wiki:wiki ->
   page:string option ->
   media:media_type ->
   ?wbcss:wikibox ->
   unit ->
   wikibox Lwt.t) rights_sp

val delete_css:
  (wiki:wiki ->
   page:string option ->
   wb:wikibox ->
   unit Lwt.t) rights_sp

val update_css:
  (wiki:wiki ->
   page:string option ->
   oldwb:wikibox ->
   newwb:wikibox ->
   media:media_type ->
   rank:int32 ->
   unit Lwt.t) rights_sp

(** Raised in case of a non-existing wikibox. The optional [int32]
   argument is the version number *)
exception Unknown_box of wikibox * int32 option


(** Returns the content of the wikibox if the user has enough rights,
    possibly for the given revision *)
val wikibox_content:
 (?version:int32 ->
  wikibox ->
  'res Wiki_types.wikibox_content Lwt.t) rights_sp

val wikibox_content':
 (?version:int32 ->
  wikibox ->
  (string option * int32) Lwt.t) rights_sp


val wikibox_history :
 (wb:wikibox ->
  (int32 * string * (* userid *) int32 * CalendarLib.Calendar.t) list Lwt.t)
 rights_sp


(** Returns the css for the specified wiki or the wikipage. The CSS are
    filtered for the ones the user can read *)
val wiki_css :
 (wiki:wiki ->
  (wikibox * (string * media_type * int32)) list Lwt.t) rights_sp

(** Same thing for a wikipage *)
val wikipage_css :
 (wiki:wiki -> page:string ->
  (wikibox * (string * media_type * int32)) list Lwt.t) rights_sp



(** Same as [Wiki_sql.update_wiki, except that this function fails
    with [Permission_denied] if the user has not enough rights to edit a wiki *)
val update_wiki :
 (?container:wikibox option->
  ?staticdir:string option ->
  ?path:string option ->
  ?descr:string ->
  ?boxrights:bool ->
  ?model:wiki_model ->
  ?siteid:string option ->
  wiki -> unit Lwt.t) rights_sp


val save_wikipage_properties :
  (?title:string ->
   ?wb:wikibox option ->
   ?newpage:string ->
   wikipage ->
   unit Lwt.t) rights_sp
