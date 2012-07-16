<?php

function param_post($a, $b=null) { echo "$a, $b"; }
function param_get($a, $b=null)  { echo "$a, $b"; }
function param_request($a, $b=null) { echo "$a, $b"; }
class Param { 
  const STRING = 1;
}

param_post(array('post1'  => Param::STRING,
                 //ERROR: unused variable
                 'unused_post2'        => Param::STRING
                ));

param_get(array('get1'  => Param::STRING,
                 //ERROR: unused variable
                 'unused_get2'        => Param::STRING
                ));

param_request(array('req1'  => Param::STRING,
                 //ERROR: unused variable
                 'unused_req2'        => Param::STRING
                   ), 'param_');

echo $post_post1;
echo $get_get1;
echo $param_req1;

//ERROR: undefined variable
echo $post_foo;
//ERROR: undefined variable
echo $get_post1;
//ERROR: undefined variable
echo $get_req1;
