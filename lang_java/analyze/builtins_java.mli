
(* will generate some java.lang.xxx.java files in dst by just extracting
 * the class definitions from src. Useful to build pfff/data/java_stdlib
 *)
val extract_from_sources: 
 skip_list:Skip_code.skip list ->
 src:Common.dirname -> dst:Common.dirname ->
 unit


