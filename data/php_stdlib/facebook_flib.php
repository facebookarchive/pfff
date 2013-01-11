<?php

// defined in flib/third-party/tfpdf/font/unifont/ttfonts.php
// which is included via a dynamic include_once :( so hardcode it here
class TTFontFile { }

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
