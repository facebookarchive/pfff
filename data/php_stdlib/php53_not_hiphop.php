<?php

// libphutil is using that

// http://php.net/manual/en/function.finfo-file.php
function finfo_file () { }
//resource $finfo , string $file_name = NULL [, int $options = FILEINFO_NONE [, resource $context = NULL ]] )
//http://php.net/manual/en/function.finfo-open.php
function finfo_open() { }
// ([ int $options = FILEINFO_NONE [, string $magic_file = NULL ]] )
// http://php.net/manual/en/function.finfo-close.php
function finfo_close ( resource $finfo) { }

const FILEINFO_MIME = 0;

// http://www.php.net/manual/en/function.mime-content-type.php
function mime_content_type (string $filename) { }

