(* the frontend *)

type vcs_kind = Git | CVS | Mercurial | Subversion

val string_of_vcs_kind : vcs_kind -> string
val tool_of_vcs_kind : vcs_kind -> string

val detect_vcs_source_tree : Common.dirname -> vcs_kind option
