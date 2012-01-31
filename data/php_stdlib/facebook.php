<?php

// ------------------------------------------
// Flib constants
// ------------------------------------------

// see also static $builtin_defines = ...
// in www/flib/_bin/_checkModuleLib.php
// now in hphp_constants.php
// define('CLOCK_THREAD_CPUTIME_ID', 1);

// flib/ code is supposed to never use flib_init() so never use
// any FLIB_CONTEXT_xxx constants, but some of our
// code sometimes do:
//   if (ModuleStack::getContext() === FLIB_CONTEXT_TEST) ...
define('FLIB_CONTEXT_TEST', 1);

// flib/init/http/safety.php also uses those constants to determine which 
// pages should be 404'ed in the secure tier.
// mostly copy paste of flib/__flib.php
define('FLIB_CONTEXT_NOOP', -1);
define('FLIB_CONTEXT_NULL', 0);
//define('FLIB_CONTEXT_TEST', 1); done above
define('FLIB_CONTEXT_SCRIPT', 2);
define('FLIB_CONTEXT_AJAX', 3);
define('FLIB_CONTEXT_WEB', 4);
define('FLIB_CONTEXT_WEB_LITE', 5);
define('FLIB_CONTEXT_WEB_INTERN', 6);
define('FLIB_CONTEXT_RESOURCE', 7);
define('FLIB_CONTEXT_AGENT', 8);
define('FLIB_CONTEXT_REDIRECT', 9);
define('FLIB_CONTEXT_LOGGING', 10);
define('FLIB_CONTEXT_ALITE', 11);
define('FLIB_CONTEXT_PAGELET', 12);
define('FLIB_CONTEXT_API', 13);
define('FLIB_CONTEXT_THRIFT', 14);
define('FLIB_CONTEXT_DEVSITE', 15);
define('FLIB_CONTEXT_SECURE_WEB', 16);
define('FLIB_CONTEXT_CANVAS', 17);
define('FLIB_CONTEXT_CODESITE', 18);
define('FLIB_CONTEXT_FBIPHONE_ENDPOINT', 19);
define('FLIB_CONTEXT_CANVAS_ALITE', 20);
define('FLIB_CONTEXT_EXTERNAL_DOMAIN', 21);

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

// used to be in flib/utils/strings/ but now is core
class StringBuffer {
}

// ------------------------------------------
// In HPHP idl files ??
// ------------------------------------------

//define('JSON_FB_LOOSE', 0);
define('JSON_FB_EXTRA_ESCAPES', 0);

// ------------------------------------------
// In hphp/facebook/extensions/ idl files
// ------------------------------------------

//TODO: autogenerate them too
function photodna_jpeg_get_hashes($filename) { }

function fbobj_hphp_register_config_func($str, $version) { }

function fbobj_hphp_create(int $fbtype, int $profile, int $fbid, array
                           $tao_response) { }

// ------------------------------------------
// used by third party code
// ------------------------------------------

define('_SYSTEM_TTFONTS', 0);

// defined in flib/third-party/tfpdf/font/unifont/ttfonts.php
// which is included via a dynamic include_once :( so hardcode it here
class TTFontFile { }

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

function hphp_murmurhash($key, $len, $seed) { }
function syck_load($xs) { }

// ------------------------------------------
// Now in HPHP idl files
// ------------------------------------------
// see also static $extension_functions
// in www/flib/_bin/_checkModuleLib.php
// function mysql_connect_with_db() { }

// function hphp_get_thread_id() { }

// function xhp_preprocess_code() { }

//class phpmcc {}

function mysql_fetch_result($conn) { }
function mysql_more_results($conn) { }
function mysql_next_result($conn) { }
function mysql_multi_query($sql, $conn) { }
