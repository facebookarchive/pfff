//Provides: caml_gc_get
function caml_gc_get() { caml_failwith("caml_gc_get"); }
//Provides: caml_gc_set
function caml_gc_set(x) { caml_failwith("caml_gc_set"); }
//Provides: caml_sys_getcwd
function caml_sys_getcwd(x) { caml_failwith("caml_sys_getcwd"); }

//Provides: re_search_forward
function re_search_forward(re, s, len) {
  caml_failwith("re_search_forward: " + s); 
}
//Provides: re_string_match
function re_string_match(re, s, len) { 
  if (s == "") {
    return false;
  } else {
    caml_failwith("re_string_match: " + s); 
  }
}
//Provides: unix_stat
function unix_stat(x) { caml_failwith("unit_stat"); }
