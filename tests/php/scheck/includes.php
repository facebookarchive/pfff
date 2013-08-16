<?php

// the current analysis in include_require_php.ml is actually quite
// sophisticated and can cope with many include/require idioms
// (stuff using realpath, dirname, ../, etc)

include "cfg.php";

include dirname(__FILE__). "includes.php";
include __DIR__ . "includes.php";

//ERROR: file not found
include "unknown.php";
