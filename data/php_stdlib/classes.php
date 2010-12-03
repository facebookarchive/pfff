<?php

// http://us.php.net/manual/en/class.exception.php
class Exception { 
  /* Properties */
  protected /*string*/ $message ;
  protected /*int*/ $code ;
  protected /*string*/ $file ;
  protected /*int*/ $line ;
  /* Methods */
  public function __construct($message, $code = 0, $previous = NULL) { }
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
class RuntimeException { }

class LogicException extends  Exception { 
  public function __construct($message, $code = 0, $previous = NULL) { }
}

//http://us.php.net/InvalidArgumentException
class InvalidArgumentException extends  LogicException { 
  public function __construct($message, $code = 0, $previous = NULL) { }
}

//http://us.php.net/ErrorException
class ErrorException { }

class OutOfBoundsException { }

// http://php.net/manual/en/book.reflection.php
class ReflectionExtension { }
class ReflectionClass { }
class ReflectionFunction { }
class ReflectionMethod { }

// http://us2.php.net/DOMDocument
class DOMDocument { }

// http://us2.php.net/DOMXPath
class DOMXPath { }


// http://us2.php.net/manual/en/class.datetime.php
class DateTime { }

class DateTimeZone { }

// http://us2.php.net/SimpleXMLElement
class SimpleXMLElement { }

//http://us.php.net/PDO
class PDO { }

//http://us.php.net/SQLite3
class SQLite3 { }

//http://us.php.net/SoapClient
class SoapClient { }

//http://us.php.net/DirectoryIterator
class DirectoryIterator { }

//http://us2.php.net/RecursiveIteratorIterator
class RecursiveIteratorIterator { }

//http://us.php.net/Collator
class Collator { }

//http://us.php.net/ArrayIterator
class ArrayIterator { }
