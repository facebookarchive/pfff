<?php

// see also builtins_tao.idl.php

// ------------------------------------------
// Misc
// ------------------------------------------

define('T_XHP_TEXT', 1);

// see http://www.intern.facebook.com/intern/hphp/doc/index.php?file=threading
// now in hphp_constants.php
//define('GLOBAL_STATE_IGNORE', 1);
//define('GLOBAL_STATE_OVERWRITE', 1);
//define('GLOBAL_STATE_SKIP', 1);
//define('GLOBAL_SYMBOL_GLOBAL_VARIABLE', 1);
//define('GLOBAL_SYMBOL_STATIC_VARIABLE', 1);
//define('GLOBAL_SYMBOL_CLASS_STATIC', 1);
//define('GLOBAL_SYMBOL_DYNAMIC_CONSTANT', 1);
//define('GLOBAL_SYMBOL_FILE_INCLUDE', 1);
//define('GLOBAL_SYMBOL_REDECLARED_FUNCTION', 1);
//define('GLOBAL_SYMBOL_REDECLARED_CLASS', 1);

// define('PHPMCC_USED_FAST_PATH', 1);

// ??? defined where ?

// was in www/conf/constants.php
// Programmers should require_conf('constants.php') but they dont.
// Those constants seems builtin now and put inside a if(!defined(...)
// So for now they are handled specially in check_module.ml
//define('DEBUG_LOG_NONE', 1);
//define('DEBUG_LOG_TRACE', 1);
//define('DEBUG_LOG_USER', 1);
//define('DEBUG_LOG_IP', 1);
//define('DEBUG_LOG_URL', 1);
//define('DEBUG_LOG_REFERER', 1);
//define('DEBUG_LOG_AGENT', 1);
//define('DEBUG_LOG_POST', 1);
//define('DEBUG_LOG_COOKIE', 1);
//define('DEBUG_LOG_ALL', 1);

//define('DEBUG_LOG_DEFAULT', 1);

//from _checkModuleLib.php in $extension_defines
define('MCC_NZLIB_COMPRESSION', 0);
define('MCC_ARG_FB_SERIALIZE_PREFIXES', 0);
 
define('MCC_COMPRESSION_THRESHHOLD', 0);

// ------------------------------------------
// In HPHP idl files ??
// ------------------------------------------

//define('JSON_FB_LOOSE', 0);
//define('JSON_FB_EXTRA_ESCAPES', 0);

// ------------------------------------------
// In hphp/facebook/extensions/ idl files
// ------------------------------------------

//TODO: autogenerate them too
function fbobj_hphp_register_config_func($str, $version) { }

function fbobj_hphp_create(int $fbtype, int $profile, int $fbid, array
                           $tao_response) { }

// ------------------------------------------
// used by third party code
// ------------------------------------------

define('_SYSTEM_TTFONTS', 0);

// defined ?? this flib/third-party/geojson/WKT/WKT.class.php defines
// only class GeoJSONWKT. Maybe some magic done around class name
class WKT { 
  public static function load($x) { }
}

// ------------------------------------------
// Defined in some scripts
// ------------------------------------------
// scripts/memcache/sync_mcconf_to_smc.php
define('SCRIPT_IDENTIFIER', 0);
define('SCRIPT_OWNER_FBID', 0);

// ------------------------------------------
// Covered by some if(function_exists(...))
// ------------------------------------------

// we need to include it there because scheck is not
// aware of the if(function_exists(...) idion. todo?

function syck_load($xs) { }

// defined in flib/autoload/autoload_map.php but skipped by codegraph
function __flib_autoload_get_function_map() { }
function __flib_autoload_get_class_map() { }
function __flib_autoload_get_type_map() { }
function __flib_autoload_get_constant_map() { }

// ------------------------------------------
// Now in HPHP idl files
// ------------------------------------------
// see also static $extension_functions
// in www/flib/_bin/_checkModuleLib.php
// function mysql_connect_with_db() { }
// function hphp_get_thread_id() { }
// function xhp_preprocess_code() { }
// function hphp_murmurhash($key, $len, $seed) { }

//class phpmcc {}
