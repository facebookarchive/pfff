<?php

// now in builtins_constants.idl.php
//define('false', 0);
//define('true', 0);
//define('null', 0);

// PHP is case insensitive but pfff is not
// (now in hphp_constants.php)
//define('FALSE', 0);
//define('TRUE', 0);
//define('NULL', 0);

// http://php.net/manual/en/language.constants.predefined.php
define('__FILE__', 1);
define('__LINE__', 1);
define('DIR', 1);
define('__FUNCTION__', 1);
define('__CLASS__', 1);
define('__METHOD__', 1);
// 5.3
define('__NAMESPACE__', 1);
// 5.4
define('__TRAIT__', 1);

define('__function__', 1);
define('__class__', 1);
define('__file__', 1);

// ------------------------------------------
// now in IDL files
// ------------------------------------------

// used by sqlshim
//define('SQLITE3_INTEGER', 1);
//define('SQLITE3_FLOAT', 1);
//define('SQLITE3_NULL', 1);
//define('SQLITE3_BLOB', 1);
//define('SQLITE3_ASSOC', 1);
//define('SQLITE3_NUM', 1);

// ------------------------------------------
// now in hphp_constants.php
// ------------------------------------------

// emacs macros used to create this file:
//  (fset 'pfff-definize [?d ?e ?f ?i ?n ?e ?( ?' ?\C-e ?' ?, ?  ?1 ?) ?\; ?\C-a down])


//define('M_PI', 0);
//define('M_E', 0);
//define('PHP_INT_MAX', 0);
//define('PHP_INT_SIZE', 0);
//define('DIRECTORY_SEPARATOR', 1);

// http://php.net/manual/en/function.asort.php
//define('SORT_REGULAR', 1);
//define('SORT_STRING', 1);
//define('SORT_DESC', 1);
//define('SORT_ASC', 1);
//define('SORT_NUMERIC', 1);

//define('PREG_SET_ORDER', 1);
//define('PREG_PATTERN_ORDER', 1);
//define('PREG_SPLIT_NO_EMPTY', 1);
//define('PREG_OFFSET_CAPTURE', 1);
//define('PREG_SPLIT_DELIM_CAPTURE', 1);

//http://php.net/manual/en/function.preg-grep.php
//define('PREG_GREP_INVERT', 1);

// http://php.net/manual/en/function.htmlspecialchars.php
//define('ENT_QUOTES', 1);

//http://php.net/manual/en/function.parse-url.php
//define('PHP_URL_SCHEME', 1);
//define('PHP_URL_HOST', 1);
//define('PHP_URL_PORT', 1);
//define('PHP_URL_USER', 1);
//define('PHP_URL_PASS', 1);
//define('PHP_URL_PATH', 1);
//define('PHP_URL_QUERY', 1);
//define('PHP_URL_FRAGMENT', 1);

//http://php.net/manual/en/function.pathinfo.php
//define('PATHINFO_DIRNAME', 1);
//define('PATHINFO_BASENAME', 1);
//define('PATHINFO_EXTENSION', 1);
//define('PATHINFO_FILENAME', 1);

//http://php.net/manual/en/function.extract.php
//define('EXTR_OVERWRITE', 1);
//define('EXTR_SKIP', 1);
//define('EXTR_PREFIX_SAME', 1);
//define('EXTR_PREFIX_ALL', 1);
//define('EXTR_PREFIX_INVALID', 1);
//define('EXTR_IF_EXISTS', 1);
//define('EXTR_PREFIX_IF_EXISTS', 1);
//define('EXTR_REFS', 1);


//http://php.net/manual/en/function.assert-options.php
//define('ASSERT_ACTIVE', 1);
//define('ASSERT_WARNING', 1);
//define('ASSERT_BAIL', 1);
//define('ASSERT_QUIET_EVAL', 1);
//define('ASSERT_CALLBACK', 1);

// http://php.net/manual/en/errorfunc.constants.php
//define('E_ERROR', 1);
//define('E_NOTICE', 1);
//define('E_ALL', 1);
//define('E_STRICT', 1);
//define('E_RECOVERABLE_ERROR', 1);
//define('E_USER_ERROR', 1);
//define('E_USER_WARNING', 1);
//define('E_USER_NOTICE', 1);
//define('E_WARNING', 1);
//define('E_PARSE', 1);
//define('E_CORE_ERROR', 1);
//define('E_CORE_WARNING', 1);
//define('E_COMPILE_ERROR', 1);
//define('E_COMPILE_WARNING', 1);

// only for 5.3 ?
//define('E_DEPRECATED', 1);
//define('E_USER_DEPRECATED', 1);


//define('PHP_VERSION', 1);

//http://php.net/manual/en/function.syslog.php
//define('LOG_EMERG', 1);
//define('LOG_ALERT', 1);
//define('LOG_CRIT', 1);
//define('LOG_ERR', 1);
//define('LOG_WARNING', 1);
//define('LOG_NOTICE', 1);
//define('LOG_INFO', 1);
//define('LOG_DEBUG', 1);

//http://php.net/manual/en/iconv.constants.php
//define('ICONV_IMPL', 1);
//define('ICONV_VERSION', 1);
//define('ICONV_MIME_DECODE_STRICT', 1);
//define('ICONV_MIME_DECODE_CONTINUE_ON_ERROR', 1);

//http://php.net/manual/en/function.php-uname.php
//define('PHP_OS', 1);

//http://php.net/manual/en/function.array-change-key-case.php
//define('CASE_LOWER', 1);


//?
//define('STDERR', 1);
//define('STDIN', 1);
//define('STDOUT', 1);

//define('SEEK_CUR', 1);
//define('SEEK_END', 1);

//define('FILE_APPEND', 1);

//define('SIGKILL', 1);
//define('SIGCHLD', 1);
//define('SIGINT', 1);
//define('SIGTERM', 1);
//define('SIGUSR1', 1);

//http://www.php.net/manual/en/class.datetime.php#datetime.constants.types
//define('DATE_ATOM', 1);
//define('DATE_COOKIE', 1); 
//define('DATE_ISO8601', 1); 
//define('DATE_RFC850', 1); 
//define('DATE_RFC1036', 1); 
//define('DATE_RFC1123', 1); 
//define('DATE_RFC2822', 1); 
//define('DATE_RFC3339', 1); 
//define('DATE_RSS', 1); 
//define('DATE_W3C', 1); 
//define('DATE_RFC822', 1);

// http://php.net/manual/en/function.openssl-sign.php
//define('OPENSSL_ALGO_SHA1', 1);

// http://php.net/manual/en/function.htmlspecialchars.php
//define('ENT_NOQUOTES', 1);
//define('ENT_COMPAT', 1);

//http://php.net/manual/en/function.stream-socket-client.php
//define('STREAM_CLIENT_CONNECT', 1);
//define('STREAM_CLIENT_ASYNC_CONNECT', 1);
//define('STREAM_CLIENT_PERSISTENT', 1);

//define('STREAM_SHUT_RDWR', 1);

// http://php.net/manual/en/function.dns-get-record.php
//define('DNS_A', 1);

// http://php.net/manual/en/function.socket-send.php
//define('MSG_OOB', 1);
//define('MSG_EOR', 1);
//define('MSG_EOF', 1);
//define('MSG_DONTROUTE', 1);

// http://www.php.net/manual/en/function.socket-create.php
//define('AF_INET', 1);
//define('AF_INET6', 1);
//define('AF_UNIX', 1);

//http://php.net/manual/en/function.socket-set-option.php
//define('SOCK_STREAM', 1);
//define('SOCK_DGRAM', 1);
//define('SOL_TCP', 1);
//define('SOL_SOCKET', 1);
//define('SOL_UDP', 1);
//define('SO_SNDTIMEO', 1);
//define('SO_RCVTIMEO', 1);

//define('MYSQL_ASSOC', 1);

//http://us2.php.net/manual/en/function.mcrypt-encrypt.php
//define('MCRYPT_BLOWFISH', 1);
//define('MCRYPT_DES', 1);

//http://www.php.net/manual/en/mcrypt.constants.php
//define('MCRYPT_MODE_ECB', 1);
//define('MCRYPT_MODE_CBC', 1);
//define('MCRYPT_MODE_CFB', 1);
//define('MCRYPT_MODE_OFB', 1);
//define('MCRYPT_MODE_NOFB', 1);
//define('MCRYPT_MODE_STREAM', 1);
//define('MCRYPT_ENCRYPT', 1);
//define('MCRYPT_DECRYPT', 1);
//define('MCRYPT_DEV_RANDOM', 1);
//define('MCRYPT_DEV_URANDOM', 1);
//define('MCRYPT_RAND', 1);

//http://www.php.net/manual/en/mcrypt.ciphers.php
//define('MCRYPT_3DES', 1);  
//define('MCRYPT_ARCFOUR_IV', 1);
//define('MCRYPT_ARCFOUR', 1);
//define('MCRYPT_BLOWFISH', 1);   
//define('MCRYPT_CAST_128', 1);
//define('MCRYPT_CAST_256', 1);   
//define('MCRYPT_CRYPT', 1);
//define('MCRYPT_DES', 1);
//define('MCRYPT_DES_COMPAT', 1);
//define('MCRYPT_ENIGMA', 1);
//define('MCRYPT_GOST', 1);
//define('MCRYPT_IDEA', 1);
//define('MCRYPT_LOKI97', 1);
//define('MCRYPT_MARS', 1);
//define('MCRYPT_PANAMA', 1);
//define('MCRYPT_RIJNDAEL_128', 1);
//define('MCRYPT_RIJNDAEL_192', 1);
//define('MCRYPT_RIJNDAEL_256', 1);
//define('MCRYPT_RC2', 1);
//define('MCRYPT_RC4', 1);
//define('MCRYPT_RC6', 1);
//define('MCRYPT_RC6_128', 1);
//define('MCRYPT_RC6_192', 1);
//define('MCRYPT_RC6_256', 1);
//define('MCRYPT_SAFER64', 1);
//define('MCRYPT_SAFER128', 1);
//define('MCRYPT_SAFERPLUS', 1);
//define('MCRYPT_SERPENT(libmcrypt', 1);
//define('MCRYPT_SERPENT_128', 1);
//define('MCRYPT_SERPENT_192', 1);
//define('MCRYPT_SERPENT_256', 1);
//define('MCRYPT_SKIPJACK', 1);
//define('MCRYPT_TEAN', 1);
//define('MCRYPT_THREEWAY', 1);
//define('MCRYPT_TRIPLEDES', 1);
//define('MCRYPT_TWOFISH', 1);
//define('MCRYPT_TWOFISH128', 1);
//define('MCRYPT_TWOFISH192', 1);
//define('MCRYPT_TWOFISH256', 1);
//define('MCRYPT_WAKE', 1);
//define('MCRYPT_XTEA', 1);

// http://php.net/manual/en/function.str-pad.php
//define('STR_PAD_RIGHT', 1);
//define('STR_PAD_LEFT', 1);
//define('STR_PAD_BOTH', 1);

// http://us.php.net/getimagesize
//define('IMAGETYPE_GIF', 1);
//define('IMAGETYPE_JPEG', 1);
//define('IMAGETYPE_PNG', 1);

// http://php.net/manual/en/function.ldap-set-option.php
//define('LDAP_OPT_DEREF', 1);
//define('LDAP_OPT_SIZELIMIT', 1);
//define('LDAP_OPT_TIMELIMIT', 1);
//define('LDAP_OPT_NETWORK_TIMEOUT', 1);
//define('LDAP_OPT_PROTOCOL_VERSION', 1);
//define('LDAP_OPT_ERROR_NUMBER', 1);
//define('LDAP_OPT_REFERRALS', 1);
//define('LDAP_OPT_RESTART', 1);
//define('LDAP_OPT_HOST_NAME', 1);
//define('LDAP_OPT_ERROR_STRING', 1);
//define('LDAP_OPT_MATCHED_DN', 1);
//define('LDAP_OPT_SERVER_CONTROLS', 1);
//define('LDAP_OPT_CLIENT_CONTROLS', 1);


// http://us.php.net/manual/en/tokens.php
//define('T_ABSTRACT', 1);
//define('T_AND_EQUAL', 1);
//define('T_ARRAY', 1);
//define('T_ARRAY_CAST', 1);
//define('T_AS', 1);
//define('T_BAD_CHARACTER', 1);
//define('T_BOOLEAN_AND', 1);
//define('T_BOOLEAN_OR', 1);
//define('T_BOOL_CAST', 1);
//define('T_BREAK', 1);
//define('T_CASE', 1);
//define('T_CATCH', 1);
//define('T_CHARACTER', 1);
//define('T_CLASS', 1);
//define('T_CLASS_C', 1);
//define('T_CLONE', 1);
//define('T_CLOSE_TAG', 1);
//define('T_COMMENT', 1);
//define('T_CONCAT_EQUAL', 1);
//define('T_CONST', 1);
//define('T_CONSTANT_ENCAPSED_STRING', 1);
//define('T_CONTINUE', 1);
//define('T_CURLY_OPEN', 1);
//define('T_DEC', 1);
//define('T_DECLARE', 1);
//define('T_DEFAULT', 1);
//define('T_DIR', 1);
//define('T_DIV_EQUAL', 1);
//define('T_DNUMBER', 1);
//define('T_DOC_COMMENT', 1);
//define('T_DO', 1);
//define('T_DOLLAR_OPEN_CURLY_BRACES', 1);
//define('T_DOUBLE_ARROW', 1);
//define('T_DOUBLE_CAST', 1);
//define('T_DOUBLE_COLON', 1);
//define('T_ECHO', 1);
//define('T_ELSE', 1);
//define('T_ELSEIF', 1);
//define('T_EMPTY', 1);
//define('T_ENCAPSED_AND_WHITESPACE', 1);
//define('T_ENDDECLARE', 1);
//define('T_ENDFOR', 1);
//define('T_ENDFOREACH', 1);
//define('T_ENDIF', 1);
//define('T_ENDSWITCH', 1);
//define('T_ENDWHILE', 1);
//define('T_END_HEREDOC', 1);
//define('T_EVAL', 1);
//define('T_EXIT', 1);
//define('T_EXTENDS', 1);
//define('T_FILE', 1);
//define('T_FINAL', 1);
//define('T_FOR', 1);
//define('T_FOREACH', 1);
//define('T_FUNCTION', 1);
//define('T_FUNC_C', 1);
//define('T_GLOBAL', 1);
//define('T_GOTO', 1);
//define('T_HALT_COMPILER', 1);
//define('T_IF', 1);
//define('T_IMPLEMENTS', 1);
//define('T_INC', 1);
//define('T_INCLUDE', 1);
//define('T_INCLUDE_ONCE', 1);
//define('T_INLINE_HTML', 1);
//define('T_INSTANCEOF', 1);
//define('T_INT_CAST', 1);
//define('T_INTERFACE', 1);
//define('T_ISSET', 1);
//define('T_IS_EQUAL', 1);
//define('T_IS_GREATER_OR_EQUAL', 1);
//define('T_IS_IDENTICAL', 1);
//define('T_IS_NOT_EQUAL', 1);
//define('T_IS_NOT_IDENTICAL', 1);
//define('T_IS_SMALLER_OR_EQUAL', 1);
//define('T_LINE', 1);
//define('T_LIST', 1);
//define('T_LNUMBER', 1);
//define('T_LOGICAL_AND', 1);
//define('T_LOGICAL_OR', 1);
//define('T_LOGICAL_XOR', 1);
//define('T_METHOD_C', 1);
//define('T_MINUS_EQUAL', 1);
//define('T_ML_COMMENT', 1);
//define('T_MOD_EQUAL', 1);
//define('T_MUL_EQUAL', 1);
//define('T_NAMESPACE', 1);
//define('T_NS_C', 1);
//define('T_NS_SEPARATOR', 1);
//define('T_NEW', 1);
//define('T_NUM_STRING', 1);
//define('T_OBJECT_CAST', 1);
//define('T_OBJECT_OPERATOR', 1);
//define('T_OLD_FUNCTION', 1);
//define('T_OPEN_TAG', 1);
//define('T_OPEN_TAG_WITH_ECHO', 1);
//define('T_OR_EQUAL', 1);
//define('T_PAAMAYIM_NEKUDOTAYIM', 1);
//define('T_PLUS_EQUAL', 1);
//define('T_PRINT', 1);
//define('T_PRIVATE', 1);
//define('T_PUBLIC', 1);
//define('T_PROTECTED', 1);
//define('T_REQUIRE', 1);
//define('T_REQUIRE_ONCE', 1);
//define('T_RETURN', 1);
//define('T_SL', 1);
//define('T_SL_EQUAL', 1);
//define('T_SR', 1);
//define('T_SR_EQUAL', 1);
//define('T_START_HEREDOC', 1);
//define('T_STATIC', 1);
//define('T_STRING', 1);
//define('T_STRING_CAST', 1);
//define('T_STRING_VARNAME', 1);
//define('T_SWITCH', 1);
//define('T_THROW', 1);
//define('T_TRY', 1);
//define('T_UNSET', 1);
//define('T_UNSET_CAST', 1);
//define('T_USE', 1);
//define('T_VAR', 1);
//define('T_VARIABLE', 1);
//define('T_WHILE', 1);
//define('T_WHITESPACE', 1);
//define('T_XOR_EQUAL', 1);

// http://us.php.net/pcntl_waitpid
//define('WNOHANG', 1);
//define('WUNTRACED', 1);



// http://php.net/manual/en/features.file-upload.errors.php
//define('UPLOAD_ERR_OK', 1);
//define('UPLOAD_ERR_INI_SIZE', 1);
//define('UPLOAD_ERR_FORM_SIZE', 1);
//define('UPLOAD_ERR_PARTIAL', 1);
//define('UPLOAD_ERR_NO_FILE', 1);
//define('UPLOAD_ERR_NO_TMP_DIR', 1);
//define('UPLOAD_ERR_CANT_WRITE', 1);
//define('UPLOAD_ERR_EXTENSION', 1);

//http://php.net/manual/en/function.curl-setopt.php
//http://www.php.net/manual/en/curl.constants.php
// some of them are in builtins_curl.idl.php but not all :(
//define('CURLOPT_AUTOREFERER', 1);
//define('CURLOPT_COOKIESESSION', 1);
//define('CURLOPT_DNS_USE_GLOBAL_CACHE', 1);
//define('CURLOPT_DNS_CACHE_TIMEOUT', 1);
//define('CURLOPT_FTP_SSL', 1);
//define('CURLFTPSSL_TRY', 1);
//define('CURLFTPSSL_ALL', 1);
//define('CURLFTPSSL_CONTROL', 1);
//define('CURLFTPSSL_NONE', 1);
//define('CURLOPT_PRIVATE', 1);
//define('CURLOPT_FTPSSLAUTH', 1);
//define('CURLOPT_PORT', 1);
//define('CURLOPT_FILE', 1);
//define('CURLOPT_INFILE', 1);
//define('CURLOPT_INFILESIZE', 1);
//define('CURLOPT_URL', 1);
//define('CURLOPT_PROXY', 1);
//define('CURLOPT_VERBOSE', 1);
//define('CURLOPT_HEADER', 1);
//define('CURLOPT_HTTPHEADER', 1);
//define('CURLOPT_NOPROGRESS', 1);
//define('CURLOPT_NOBODY', 1);
//define('CURLOPT_FAILONERROR', 1);
//define('CURLOPT_UPLOAD', 1);
//define('CURLOPT_POST', 1);
//define('CURLOPT_FTPLISTONLY', 1);
//define('CURLOPT_FTPAPPEND', 1);
//define('CURLOPT_FTP_CREATE_MISSING_DIRS', 1);
//define('CURLOPT_NETRC', 1);
//define('CURLOPT_FOLLOWLOCATION', 1);
//define('CURLOPT_FTPASCII', 1);
//define('CURLOPT_PUT', 1);
//define('CURLOPT_MUTE', 1);
//define('CURLOPT_USERPWD', 1);
//define('CURLOPT_PROXYUSERPWD', 1);
//define('CURLOPT_RANGE', 1);
//define('CURLOPT_TIMEOUT', 1);
////define('CURLOPT_TIMEOUT_MS', 1);
//define('CURLOPT_TCP_NODELAY', 1);
//define('CURLOPT_POSTFIELDS', 1);
//define('CURLOPT_PROGRESSFUNCTION', 1);
//define('CURLOPT_REFERER', 1);
//define('CURLOPT_USERAGENT', 1);
//define('CURLOPT_FTPPORT', 1);
//define('CURLOPT_FTP_USE_EPSV', 1);
//define('CURLOPT_LOW_SPEED_LIMIT', 1);
//define('CURLOPT_LOW_SPEED_TIME', 1);
//define('CURLOPT_RESUME_FROM', 1);
//define('CURLOPT_COOKIE', 1);
//define('CURLOPT_SSLCERT', 1);
//define('CURLOPT_SSLCERTPASSWD', 1);
//define('CURLOPT_WRITEHEADER', 1);
//define('CURLOPT_SSL_VERIFYHOST', 1);
//define('CURLOPT_COOKIEFILE', 1);
//define('CURLOPT_SSLVERSION', 1);
//define('CURLOPT_TIMECONDITION', 1);
//define('CURLOPT_TIMEVALUE', 1);
//define('CURLOPT_CUSTOMREQUEST', 1);
//define('CURLOPT_STDERR', 1);
//define('CURLOPT_TRANSFERTEXT', 1);
//define('CURLOPT_RETURNTRANSFER', 1);
//define('CURLOPT_QUOTE', 1);
//define('CURLOPT_POSTQUOTE', 1);
//define('CURLOPT_INTERFACE', 1);
//define('CURLOPT_KRB4LEVEL', 1);
//define('CURLOPT_HTTPPROXYTUNNEL', 1);
//define('CURLOPT_FILETIME', 1);
//define('CURLOPT_WRITEFUNCTION', 1);
//define('CURLOPT_READFUNCTION', 1);
//define('CURLOPT_PASSWDFUNCTION', 1);
//define('CURLOPT_HEADERFUNCTION', 1);
//define('CURLOPT_MAXREDIRS', 1);
//define('CURLOPT_MAXCONNECTS', 1);
//define('CURLOPT_CLOSEPOLICY', 1);
//define('CURLOPT_FRESH_CONNECT', 1);
//define('CURLOPT_FORBID_REUSE', 1);
//define('CURLOPT_RANDOM_FILE', 1);
//define('CURLOPT_EGDSOCKET', 1);
//define('CURLOPT_CONNECTTIMEOUT', 1);
////define('CURLOPT_CONNECTTIMEOUT_MS', 1);
//define('CURLOPT_SSL_VERIFYPEER', 1);
//define('CURLOPT_CAINFO', 1);
//define('CURLOPT_CAPATH', 1);
//define('CURLOPT_COOKIEJAR', 1);
//define('CURLOPT_SSL_CIPHER_LIST', 1);
//define('CURLOPT_BINARYTRANSFER', 1);
//define('CURLOPT_NOSIGNAL', 1);
//define('CURLOPT_PROXYTYPE', 1);
//define('CURLOPT_BUFFERSIZE', 1);
//define('CURLOPT_HTTPGET', 1);
//define('CURLOPT_HTTP_VERSION', 1);
//define('CURLOPT_SSLKEY', 1);
//define('CURLOPT_SSLKEYTYPE', 1);
//define('CURLOPT_SSLKEYPASSWD', 1);
//define('CURLOPT_SSLENGINE', 1);
//define('CURLOPT_SSLENGINE_DEFAULT', 1);
//define('CURLOPT_SSLCERTTYPE', 1);
//define('CURLOPT_CRLF', 1);
//define('CURLOPT_ENCODING', 1);
//define('CURLOPT_PROXYPORT', 1);
//define('CURLOPT_UNRESTRICTED_AUTH', 1);
//define('CURLOPT_FTP_USE_EPRT', 1);
//define('CURLOPT_HTTP200ALIASES', 1);
//define('CURLOPT_HTTPAUTH', 1);
//define('CURLAUTH_BASIC', 1);
//define('CURLAUTH_DIGEST', 1);
//define('CURLAUTH_GSSNEGOTIATE', 1);
//define('CURLAUTH_NTLM', 1);
//define('CURLAUTH_ANY', 1);
//define('CURLAUTH_ANYSAFE', 1);
//define('CURLOPT_PROXYAUTH', 1);
//define('CURLCLOSEPOLICY_LEAST_RECENTLY_USED', 1);
//define('CURLCLOSEPOLICY_LEAST_TRAFFIC', 1);
//define('CURLCLOSEPOLICY_SLOWEST', 1);
//define('CURLCLOSEPOLICY_CALLBACK', 1);
//define('CURLCLOSEPOLICY_OLDEST', 1);
//define('CURLINFO_PRIVATE', 1);
//define('CURLINFO_EFFECTIVE_URL', 1);
//define('CURLINFO_HTTP_CODE', 1);
//define('CURLINFO_HEADER_OUT', 1);
//define('CURLINFO_HEADER_SIZE', 1);
//define('CURLINFO_REQUEST_SIZE', 1);
//define('CURLINFO_TOTAL_TIME', 1);
//define('CURLINFO_NAMELOOKUP_TIME', 1);
//define('CURLINFO_CONNECT_TIME', 1);
//define('CURLINFO_PRETRANSFER_TIME', 1);
//define('CURLINFO_SIZE_UPLOAD', 1);
//define('CURLINFO_SIZE_DOWNLOAD', 1);
//define('CURLINFO_SPEED_DOWNLOAD', 1);
//define('CURLINFO_SPEED_UPLOAD', 1);
//define('CURLINFO_FILETIME', 1);
//define('CURLINFO_SSL_VERIFYRESULT', 1);
//define('CURLINFO_CONTENT_LENGTH_DOWNLOAD', 1);
//define('CURLINFO_CONTENT_LENGTH_UPLOAD', 1);
//define('CURLINFO_STARTTRANSFER_TIME', 1);
//define('CURLINFO_CONTENT_TYPE', 1);
//define('CURLINFO_REDIRECT_TIME', 1);
//define('CURLINFO_REDIRECT_COUNT', 1);
//define('CURL_TIMECOND_IFMODSINCE', 1);
//define('CURL_TIMECOND_IFUNMODSINCE', 1);
//define('CURL_TIMECOND_LASTMOD', 1);
//define('CURL_VERSION_IPV6', 1);
//define('CURL_VERSION_KERBEROS4', 1);
//define('CURL_VERSION_SSL', 1);
//define('CURL_VERSION_LIBZ', 1);
//define('CURLVERSION_NOW', 1);
//define('CURLE_OK', 1);
//define('CURLE_UNSUPPORTED_PROTOCOL', 1);
//define('CURLE_FAILED_INIT', 1);
//define('CURLE_URL_MALFORMAT', 1);
//define('CURLE_URL_MALFORMAT_USER', 1);
//define('CURLE_COULDNT_RESOLVE_PROXY', 1);
//define('CURLE_COULDNT_RESOLVE_HOST', 1);
//define('CURLE_COULDNT_CONNECT', 1);
//define('CURLE_FTP_WEIRD_SERVER_REPLY', 1);
//define('CURLE_FTP_ACCESS_DENIED', 1);
//define('CURLE_FTP_USER_PASSWORD_INCORRECT', 1);
//define('CURLE_FTP_WEIRD_PASS_REPLY', 1);
//define('CURLE_FTP_WEIRD_USER_REPLY', 1);
//define('CURLE_FTP_WEIRD_PASV_REPLY', 1);
//define('CURLE_FTP_WEIRD_227_FORMAT', 1);
//define('CURLE_FTP_CANT_GET_HOST', 1);
//define('CURLE_FTP_CANT_RECONNECT', 1);
//define('CURLE_FTP_COULDNT_SET_BINARY', 1);
//define('CURLE_PARTIAL_FILE', 1);
//define('CURLE_FTP_COULDNT_RETR_FILE', 1);
//define('CURLE_FTP_WRITE_ERROR', 1);
//define('CURLE_FTP_QUOTE_ERROR', 1);
//define('CURLE_HTTP_NOT_FOUND', 1);
//define('CURLE_WRITE_ERROR', 1);
//define('CURLE_MALFORMAT_USER', 1);
//define('CURLE_FTP_COULDNT_STOR_FILE', 1);
//define('CURLE_READ_ERROR', 1);
//define('CURLE_OUT_OF_MEMORY', 1);
//define('CURLE_OPERATION_TIMEOUTED', 1);
//define('CURLE_FTP_COULDNT_SET_ASCII', 1);
//define('CURLE_FTP_PORT_FAILED', 1);
//define('CURLE_FTP_COULDNT_USE_REST', 1);
//define('CURLE_FTP_COULDNT_GET_SIZE', 1);
//define('CURLE_HTTP_RANGE_ERROR', 1);
//define('CURLE_HTTP_POST_ERROR', 1);
//define('CURLE_SSL_CONNECT_ERROR', 1);
//define('CURLE_FTP_BAD_DOWNLOAD_RESUME', 1);
//define('CURLE_FILE_COULDNT_READ_FILE', 1);
//define('CURLE_LDAP_CANNOT_BIND', 1);
//define('CURLE_LDAP_SEARCH_FAILED', 1);
//define('CURLE_LIBRARY_NOT_FOUND', 1);
//define('CURLE_FUNCTION_NOT_FOUND', 1);
//define('CURLE_ABORTED_BY_CALLBACK', 1);
//define('CURLE_BAD_FUNCTION_ARGUMENT', 1);
//define('CURLE_BAD_CALLING_ORDER', 1);
//define('CURLE_HTTP_PORT_FAILED', 1);
//define('CURLE_BAD_PASSWORD_ENTERED', 1);
//define('CURLE_TOO_MANY_REDIRECTS', 1);
//define('CURLE_UNKNOWN_TELNET_OPTION', 1);
//define('CURLE_TELNET_OPTION_SYNTAX', 1);
//define('CURLE_OBSOLETE', 1);
//define('CURLE_SSL_PEER_CERTIFICATE', 1);
//define('CURLE_GOT_NOTHING', 1);
//define('CURLE_SSL_ENGINE_NOTFOUND', 1);
//define('CURLE_SSL_ENGINE_SETFAILED', 1);
//define('CURLE_SEND_ERROR', 1);
//define('CURLE_RECV_ERROR', 1);
//define('CURLE_SHARE_IN_USE', 1);
//define('CURLE_SSL_CERTPROBLEM', 1);
//define('CURLE_SSL_CIPHER', 1);
//define('CURLE_SSL_CACERT', 1);
//define('CURLE_BAD_CONTENT_ENCODING', 1);
//define('CURLE_LDAP_INVALID_URL', 1);
//define('CURLE_FILESIZE_EXCEEDED', 1);
//define('CURLE_FTP_SSL_FAILED', 1);
//define('CURLFTPAUTH_DEFAULT', 1);
//define('CURLFTPAUTH_SSL', 1);
//define('CURLFTPAUTH_TLS', 1);
//define('CURLPROXY_HTTP', 1);
//define('CURLPROXY_SOCKS5', 1);
//define('CURL_NETRC_OPTIONAL', 1);
//define('CURL_NETRC_IGNORED', 1);
//define('CURL_NETRC_REQUIRED', 1);
//define('CURL_HTTP_VERSION_NONE', 1);
//define('CURL_HTTP_VERSION_1_0', 1);
//define('CURL_HTTP_VERSION_1_1', 1);
//define('CURLM_CALL_MULTI_PERFORM', 1);
//define('CURLM_OK', 1);
//define('CURLM_BAD_HANDLE', 1);
//define('CURLM_BAD_EASY_HANDLE', 1);
//define('CURLM_OUT_OF_MEMORY', 1);
//define('CURLM_INTERNAL_ERROR', 1);
//define('CURLMSG_DONE', 1);

// http://php.net/manual/es/libxml.constants.php
//define('LIBXML_COMPACT', 1);
//define('LIBXML_DTDATTR', 1);
//define('LIBXML_DTDLOAD', 1);
//define('LIBXML_DTDVALID', 1);
//define('LIBXML_NOBLANKS', 1);
//define('LIBXML_NOCDATA', 1);
//define('LIBXML_NOEMPTYTAG', 1);
//define('LIBXML_NOENT', 1);
//define('LIBXML_NOERROR', 1);
//define('LIBXML_NONET', 1);
//define('LIBXML_NOWARNING', 1);
//define('LIBXML_NOXMLDECL', 1);
//define('LIBXML_NSCLEAN', 1);
//define('LIBXML_PARSEHUGE', 1);
//define('LIBXML_XINCLUDE', 1);
//define('LIBXML_ERR_ERROR', 1);
//define('LIBXML_ERR_FATAL', 1);
//define('LIBXML_ERR_NONE', 1);
//define('LIBXML_ERR_WARNING', 1);
//define('LIBXML_VERSION', 1);
//define('LIBXML_DOTTED_VERSION', 1);

//http://www.php.net/manual/en/function.setlocale.php
//define('LC_ALL', 1);
//define('LC_COLLATE', 1);
//define('LC_CTYPE', 1);
//define('LC_MONETARY', 1);
//define('LC_NUMERIC', 1);
//define('LC_TIME', 1);
//define('LC_MESSAGES', 1);

// http://php.net/manual/en/function.microtime.php
//define('CLOCK_MONOTONIC', 1);

//http://php.net/manual/en/function.flock.php
//define('LOCK_SH', 1);
//define('LOCK_EX', 1);
//define('LOCK_UN', 1);

// http://php.net/manual/en/function.mb-convert-case.php
//define('MB_CASE_UPPER', 1);
//define('MB_CASE_LOWER', 1);
//define('MB_CASE_TITLE', 1);
