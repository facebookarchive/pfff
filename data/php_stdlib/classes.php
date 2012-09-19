<?php

//http://php.net/manual/en/reserved.classes.php
class stdClass { }
//class Directory

// ***************************************************************************
// Exceptions
// ***************************************************************************

// http://us.php.net/manual/en/class.exception.php
class Exception { 
  /* Properties */
  protected /*string*/ $message ;
  protected /*int*/ $code ;
  protected /*string*/ $file ;
  protected /*int*/ $line ;
  /* Methods */
  public function __construct($message = "", $code = 0, $previous = NULL) { }
  //([ string $message = "" [, int $code = 0 [, Exception $previous = NULL ]]] )

//final public string getMessage ( void )
//final public Exception getPrevious ( void )
//final public int getCode ( void )
//final public string getFile ( void )
//final public int getLine ( void )
//final public array getTrace ( void )
//final public string getTraceAsString ( void )
//public string __toString ( void )
//final private void __clone ( void )

}

//http://us2.php.net/RuntimeException 
class RuntimeException extends Exception { }

class LogicException extends  Exception { 
  public function __construct($message, $code = 0, $previous = NULL) { }
}

//http://us.php.net/InvalidArgumentException
class InvalidArgumentException extends  LogicException { 
  public function __construct($message, $code = 0, $previous = NULL) { }
}

//http://us.php.net/ErrorException
class ErrorException extends Exception { }

class OutOfBoundsException extends Exception { }

//http://us3.php.net/UnexpectedValueException
class UnexpectedValueException extends Exception { }

class InvalidOperationException extends RuntimeException { }

//http://www.php.net/manual/en/class.badfunctioncallexception.php
class  BadFunctionCallException extends LogicException { }

//http://php.net/manual/en/class.badmethodcallexception.php
class BadMethodCallException extends  BadFunctionCallException { }

// ***************************************************************************
// Reflection
// ***************************************************************************

// http://php.net/manual/en/book.reflection.php
class ReflectionExtension { }
class ReflectionClass { }
class ReflectionFunction { }
class ReflectionMethod { }

//http://php.net/manual/en/class.reflectionobject.php
class ReflectionObject  { }

//http://php.net/manual/en/class.reflectionproperty.php
class ReflectionProperty { }

// ***************************************************************************
// Iterators
// ***************************************************************************

//http://us.php.net/ArrayIterator
class ArrayIterator { }

//http://us.php.net/DirectoryIterator
class DirectoryIterator { }

//http://us2.php.net/RecursiveIteratorIterator
class RecursiveIteratorIterator { }

//http://php.net/manual/en/class.recursivedirectoryiterator.php
class RecursiveDirectoryIterator { }

// ***************************************************************************
// Iterators
// ***************************************************************************
// ------------------------------------------
// Now in HPHP idl files
// ------------------------------------------

// http://us2.php.net/DOMDocument
//class DOMDocument { }

// http://us2.php.net/DOMXPath
//class DOMXPath { }

// http://us2.php.net/manual/en/class.datetime.php
//class DateTime { }

//class DateTimeZone { }

// http://us2.php.net/SimpleXMLElement
//class SimpleXMLElement { }

//http://us.php.net/PDO
//class PDO { }

//http://us.php.net/SQLite3
//class SQLite3 { }

