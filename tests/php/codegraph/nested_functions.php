<?php

// nested func are currently not in the graph
// but they can use stuff

if(true) {
  function nested_function() {
    echo 'foo';
  }
}
