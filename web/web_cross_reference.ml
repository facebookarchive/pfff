(* 
 * The goal of this module is to provide a code browser a la LXR.
 * See  http://lxr.linux.no/#linux+v2.6.37.1/mm/compaction.c as an
 * example.
 * 
 * It's also an exercise in learning ocsigen. A code browser does
 * not require anything fancy like Depot. No need for a ORM,
 * or forms. Just need to htmlize a source file and add
 * hrefs into it to make it hypertextable.
 * 
 * todo: add search, add nice html, add fast html
 * 
 * alternatives:
 * - http://en.wikipedia.org/wiki/LXR_Cross_Referencer
 * - http://en.wikipedia.org/wiki/OpenGrok
 * 
 *)
