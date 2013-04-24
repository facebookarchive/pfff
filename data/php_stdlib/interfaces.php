<?php
// essentially a copy paste of hphp/src/system/classes/*.php

// http://us2.php.net/manual/en/class.countable.php
interface Countable {
  public function count();
}

//http://php.net/manual/en/class.serializable.php
interface Serializable { 
  public function serialize();
  public function unserialize($serialized);
}

//http://php.net/manual/en/class.jsonserializable.php
interface JsonSerializable {
  public function jsonSerialize();
}

//http://php.net/manual/en/class.traversable.ph
interface Traversable {
}

interface Iterator extends Traversable {
  public function current();
  public function key();
  public function next();
  public function rewind();
  public function valid();
}

// http://php.net/manual/en/class.iteratoraggregate.php
interface IteratorAggregate {
  public function getIterator();
}

interface Iterable extends IteratorAggregate {
}


// http://php.net/manual/en/class.arrayaccess.php
// TODO: hphp/hphp/system/classes
interface ArrayAccess {
}

// TODO: hphp/hphp/system/classes
interface Awaitable {
  public function getWaitHandle();
}

// from hphp/system/classes/iterator.php
interface KeyedIterator extends Iterator {
}

interface KeyedIterable extends Iterable {
}
