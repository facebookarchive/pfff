
(** Analyze a JS file **)
(** params: file name, parse info **)
(** return: module exports **)
val analyze : Common.filename -> Module_js.parseinfo -> Module_js.shape

(** add module to module map **)
val export_module : Module_js.shape -> 
	Module_js.moduleinfo_map -> Module_js.moduleinfo_map
