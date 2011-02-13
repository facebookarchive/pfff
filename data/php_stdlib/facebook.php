<?php

// see also static $builtin_defines =
// in www/flib/_bin/_checkModuleLib.php
define('CLOCK_THREAD_CPUTIME_ID', 1);

// flib/ code is supposed to never use flib_init() so never use
// any FLIB_CONTEXT_xxx constants, but some of our
// code sometimes do:
//   if (ModuleStack::getContext() === FLIB_CONTEXT_TEST) ...
define('FLIB_CONTEXT_TEST', 1);

// fib/init/http/safety.php also uses those constants to determine which 
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


define('T_XHP_TEXT', 1);

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

// ------------------------------------------
// Now in HPHP idl files
// ------------------------------------------
// see also static $extension_functions
// in www/flib/_bin/_checkModuleLib.php
// function mysql_connect_with_db() { }

// function hphp_get_thread_id() { }

// function xhp_preprocess_code() { }

//class phpmcc {}


// define('MCC_DELETE_NOTFOUND', 1);
// define('MCC_PROXY_GET_OP', 1);
// 
// define('MCC_IPPROTO_TCP', 1);
// define('MCC_IPPROTO_UDP', 1);
// 
// define('MCC_ARG_MIRROR_CFG_NAME', 1);
// define('MCC_ARG_MIRROR_CFG_MODEL', 1);
// define('MCC_ARG_MIRROR_CFG_SERVERPOOLS', 1);
// define('MCC_ARG_PERSISTENT', 1);
// define('MCC_ARG_SERVERS', 1);
// define('MCC_ARG_MIRROR_CFG', 1);
// define('MCC_ARG_DEFAULT_PREFIX', 1);
// define('MCC_ARG_FB_SERIALIZE_ENABLED', 1);
// define('MCC_ARG_FB_SERIALIZE_PREFIXES', 1);
// define('MCC_ARG_CONN_TMO', 1);
// define('MCC_ARG_CONN_NTRIES', 1);
// define('MCC_ARG_CONSISTENT_HASHING_PREFIXES', 1);
// define('MCC_ARG_TMO', 1);
// define('MCC_ARG_DGRAM_NTRIES', 1);
// define('MCC_ARG_DGRAM_TMO_WEIGHT', 1);
// define('MCC_ARG_UDP_REPLY_PORTS', 1);
// 
// define('MCC_HAVE_FB_SERIALIZATION', 1);
// 
// //??? facebook specific ?
// define('MCC_GET_RECORD_ERRORS', 1);
// define('MCC_DELETE_ERROR_LOG', 1);
// define('MCC_DELETE_DELETED', 1);
// 
// define('MCC_SERVER_DOWN', 1);
// 
// define('MCC_ARG_SERVER_RETRY_TMO_MS', 1);
// 
// define('MCC_ARG_DGRAM_TMO_THRESHOLD', 1);
// 
// define('MCC_ARG_WINDOW_MAX', 1);
// 
// define('MCC_HAVE_ZLIB_COMPRESSION', 1);
// 
// define('MCC_ARG_COMPRESSION_THRESHOLD', 1);
// 
// define('MCC_ARG_NZLIB_COMPRESSION', 1);
// 
// define('MCC_ARG_PROXY', 1);
// 
// define('MCC_ARG_PROXY_OPS', 1);
// 
// define('MCC_PROXY_DELETE_OP', 1);
// 
// define('MCC_PROXY_ARITH_OP', 1);
// 
// define('MCC_PROXY_UPDATE_OP', 1);
// 
// define('MCC_ARG_NPOOLPREFIX', 1);
// 
// define('MCC_HAVE_DEBUG_LOG', 1);
// 
// define('MCC_ARG_DEBUG_LOGFILE', 1);
// 
// define('MCC_COMPRESSION_THRESHOLD', 1);
// 
// define('MCC_NZLIB_COMPRESSION', 1);
// 
// define('MCC_CONN_TMO_MS', 1);
// 
// define('MCC_CONN_NTRIES', 1);
// 
// define('MCC_ARG_DEBUG', 1);
// 
// 
// define('MCC_DGRAM_NTRIES', 1);
// 
// define('MCC_DGRAM_TMO_WEIGHT', 1);
// 
// define('MCC_ARG_NODELAY', 1);
// 
// define('MCC_NODELAY', 1);
// 
// define('MCC_ARG_POLL_TMO', 1);
// 
// define('MCC_POLL_TMO_US', 1);
// 
// define('MCC_TMO_MS', 1);
// 
// define('MCC_UDP_REPLY_PORTS', 1);
// 
// define('MCC_WINDOW_MAX', 1);
// 
// define('MCC_ARG_TCP_INACTIVITY_TIME', 1);
// 
// define('MCC_TCP_INACTIVITY_TMO_DEFAULT', 1);
// 
// define('MCC_SERVER_RETRY_TMO_MS', 1);
// 
// define('MCC_DGRAM_TMO_THRESHOLD', 1);
// 
// define('MCC_POOLPREFIX_LEN', 1);
// 
// define('PHPMCC_USED_SLOW_PATH', 1);
// 
// define ('MCC_SERVER_UP', 1);
