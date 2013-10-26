
val load_trees : Common.path list -> Module_js.parseinfo_map
val load_modules : Common.path list -> Module_js.moduleinfo_map

(** build a data structure describing module signatures for a set of JS files **)
(** and run a sequence of analyses on that data structure **)
val main : Common.path list -> unit
