<?php

function yieldAuthor() {
  yield wait_for($this->prepareOwnerID());
  $owner = null;
  yield wait_for(
    Ent::createLoader(
      'EntUser',
      $this->getViewerContext(),
      $this->getOwnerID(),
      $owner
    ));
  yield result($owner);
}
