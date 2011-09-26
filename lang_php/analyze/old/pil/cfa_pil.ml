
(* http://en.wikipedia.org/wiki/Control_flow_analysis 
 *
 * Many analysis requires to do things interprocedurally to have
 * enough precision. A big issue is that to implement an interprocedural
 * analysis requires a call graph, which in the present of objects
 * (or higher order function) is difficult to compute as the control flow
 * depends on the dataflow. This call graph thus requires a class analysis
 * which requires itself an interprocedural analysis, and so we loop ...
 * This was noted first by Olin Shivers in his seminal paper on CFA.
 * Interprocedural -> Callgraph -> Class Analysis -> Dataflow -> Interprocedural
 * hmmm ...
 *)
