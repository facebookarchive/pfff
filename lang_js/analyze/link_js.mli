
(** resolve references to extenal modules from a local shape **)
val resolve : Module_js.moduleinfo_map -> Module_js.shape -> unit

(** report missing dependencies **)
val missing_imports : Module_js.moduleinfo_map -> unit

(** topologically sort dependencies **)
val topsort : Module_js.moduleinfo_map -> unit

(** validate topological order **)
val validate_topsort : Module_js.moduleinfo_map -> unit
