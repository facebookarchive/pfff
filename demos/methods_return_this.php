<?php

class Foo {

  function ok__simple() {
    return $this;
  }

  function ok__last_stmt() {
    foo();
    return $this;
  }

  //TODO
  function ok__all_branches() {
    if(true) {
      return $this;
    } else {
      return $this;
    }
  }

  //TODO
  function bad__one_null_branch() {
    if(false) {
      return null;
    }
    return $this;
  }    

  function bad__one_implicit_null_branch() {
    if(false) {
      return $this;
    }
  }

}