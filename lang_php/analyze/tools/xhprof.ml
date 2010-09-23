(* all_call_funcs2.php 

<?php

require_once 'master_include.php'; // from www/scripts
require_once $_SERVER['PHP_ROOT'].'/html/intern/phprof/phprof_report.php';

function t() {
  static $t = null;
  if ($t === null) {
    $t = microtime(true);
  }
  error_log((microtime(true) - $t)."\n");
}

$run_id = 79313898;
$xhprof_runs_impl = new FB_XHProfRuns();

$conn = phproflive_get_db('r');
t();
$ret = queryf($conn, 'SELECT raw FROM phprof_live WHERE id = %d', $run_id);
t();
$row = head(mysql_fetch_all_assoc($ret));
t();
$raw_ser = gzuncompress($row['raw']);
t();
$raw = unserialize($raw_ser);
t();
$funcs = array_keys($raw);
t();
//print_r($funcs);
print_r($raw);
t();
echo "\n";

*)
