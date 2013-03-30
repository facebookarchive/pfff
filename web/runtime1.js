
//Provides: unix_inet_addr_of_string 
function unix_inet_addr_of_string(x) {
  // see unix.ml
  if (x == "0.0.0.0" || x == "127.0.0.1") {
    return x;
  } else {
    caml_failwith("unix_inet_addr_of_string: " + x); 
  }
}

//Provides: unix_gettimeofday
function unix_gettimeofday(x) { caml_failwith("unix_gettimeofday"); }

//Provides: caml_sys_exit
function caml_sys_exit(x) { caml_failwith("caml_sys_exit"); }

//Provides: caml_ml_output_char
function caml_ml_output_char(x) { caml_failwith("caml_ml_output_char"); }
