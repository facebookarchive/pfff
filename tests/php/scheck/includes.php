<?php

// the current analysis in include_require_php.ml is actually quite
// sophisticated and can cope with many include/require idioms
// (stuff using realpath, dirname, ../, etc)

include "cfg.php";

//ERROR: file not found
include "unknown.php";
