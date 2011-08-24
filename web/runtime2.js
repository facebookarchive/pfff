
//Provides: unix_inet_addr_of_string 
function unix_inet_addr_of_string(x) {
  // see unix.ml
  if (x == "0.0.0.0" || x == "127.0.0.1") {
    return x;
  } else {
    caml_failwith("unix_inet_addr_of_string: " + x); 
  }
}
//Provides: caml_sys_getenv
function caml_sys_getenv(x) { 
  // see the Unix and Win32 modules
  if(x == "TMPDIR" || x == "TEMP") {
    return "/tmp";
  } else {
    caml_failwith("caml_sys_getenv:" + x); 
  }
}

//Provides: unix_gettimeofday
function unix_gettimeofday(x) { caml_failwith("unix_gettimeofday"); }
