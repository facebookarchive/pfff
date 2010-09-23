      
                                        
                         README: library "PCRE-OCaml"
                         ****************************
                  Copyright   (C)   2008  Markus Mottl (1)  
                  ==========================================
                          Vienna, November 29, 2008
                          =========================
  

1  Directory contents
*=*=*=*=*=*=*=*=*=*=*

   
                                        
------------------------------------------------------------------------------
|        Changes          |              History of code changes             |
------------------------------------------------------------------------------
|        INSTALL          |            Short notes on compiling and          |
|                         |               installing the library             |
------------------------------------------------------------------------------
|        LICENSE          |        "GNU LESSER GENERAL PUBLIC LICENSE"       |
------------------------------------------------------------------------------
|        Makefile         |                    Top Makefile                  |
------------------------------------------------------------------------------
|     OCamlMakefile       |           Makefile for easy handling of          |
|                         |             compilation of not so easy           |
|                         |     OCaml-projects. It generates dependencies    |
|                         |           of OCaml-files automatically,          |
|                         |           is able to handle "ocamllex"-,         |
|                         |         "ocamlyacc"-, IDL- and C-files and       |
|                         |           generates native- or byte-code         |
|                         |           as executable or as library -          |
|                         |          with thread-support if you want!        |
------------------------------------------------------------------------------
|       README.txt        |                     This file                    |
------------------------------------------------------------------------------
|      README.win32       |      Platform-specific information for Win32     |
------------------------------------------------------------------------------
|    examples/subst/      |          Example for fast and convenient         |
|                         |         substitution of patterns in files        |
------------------------------------------------------------------------------
|   examples/pcregrep/    |             Basic "grep"-like command.           |
------------------------------------------------------------------------------
|     examples/cloc/      |     Removes comments + empty lines in C-files    |
------------------------------------------------------------------------------
|  examples/count_hash/   |            Counts equal words in texts           |
------------------------------------------------------------------------------
|          lib/           |  OCaml-library for interfacing the PCRE-C-library|
|                         |      Contains lots of higher level functions     |
------------------------------------------------------------------------------
|    pcre_make.win32/     |             Additional files for Win32           |
------------------------------------------------------------------------------
                                        
  
  

2  What is the "PCRE-OCaml"-library?
*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

  
  This OCaml-library interfaces the PCRE (Perl-compatibility regular
expressions) library which is written in C. it can be used for matching
regular expressions which are written in "PERL"-style.
  
   Searching for, replacing or splitting text should become much easier with
this library.
  

3  Why would you need it?
*=*=*=*=*=*=*=*=*=*=*=*=*

  
  Here is a list of features:
  
  
   - 
 The PCRE-library by Philip Hazel has been under development for quite some
   time now and is fairly advanced and stable. It implements just about all of
   the convenient functionality of regular expressions as one can find them in
   PERL. The higher-level functions written in OCaml (split, replace), too,
   are compatible to the corresponding PERL-functions (to the extent that
   OCaml allows). Most people find the syntax of PERL-style regular
   expressions more straightforward than the Emacs-style one used in the
   "Str"-module.
 
   - 
 It is reentrant - and thus thread safe. This is not the case with the
   "Str"-module of OCaml, which builds on the GNU "regex"-library. Using
   reentrant libraries also means more convenience for programmers. They do
   not have to reason about states in which the library might be in.
 
   - 
 The high-level functions for replacement and substitution, they are all
   implemented in OCaml, are much faster than the ones of the "Str"-module. In
   fact, when compiled to native code, they even seem to be significantly
   faster than those of PERL (PERL is written in C).
 
   - 
 You can rely on the data returned being unique. In other terms: if the result
   of a function is a string, you can safely use destructive updates on it
   without having to fear side effects.
 
   - 
 The interface to the library makes use of labels and default arguments to
   give you a high degree of programming comfort. 
  
  

4  How can you use it?
*=*=*=*=*=*=*=*=*=*=*=

  
  Most functions allow additional parameters - they often have to be
translated to an internal format for the PCRE. Two ways of passing arguments
are possible in all such cases: the one is convenient, the other improves
speed. You can also often leave away such arguments - the intuitive default (=
no special behaviour) will be used instead then.
  
   Convenient way of passing arguments - flags passed as list:
  
   
<<  regexp ~flags:[`ANCHORED; `CASELESS] "some_pattern"
>>
  
  This makes it easy to pass flags on the fly. They will be translated to the
internal format automatically. However, if this happens to be in a loop, this
translation will occur on each iteration. If you really need to save as much
performance as possible, you should use the next approach.
  
   Efficient way of passing flags - translate them before:
  
   
<<  let iflags = cflags [`ANCHORED; `CASELESS] in
    for i = 1 to 1000 do
      regexp ~iflags "some runtime-constructed pattern"
    done
>>
  
  Factoring out the translation of flags for regular expressions may save some
cycles, but don't expect too much. You can save more CPU time when lifting the
creation of regular expressions out of loops. E.g. instead of:
  
   
<<  for i = 1 to 1000 do
      split ~pat:"[ \t]+" "foo bar"
    done
>>
  
  write:
  
   
<<  let rex = regexp "[ \t]+" in
    for i = 1 to 1000 do
      split ~rex:"foo bar"
    done
>>
  
  Take a look at the interface "pcre.mli" to see, which ways exists to pass
parameters and to learn about the defaults.
  

5  Contact information
*=*=*=*=*=*=*=*=*=*=*=

  
  In the case of bugs, feature requests and similar, you can contact me here:
  
     markus.mottl@gmail.com
  
   Up-to-date information concerning this library should be available here:
  
     http://www.ocaml.info/ocaml_sources
  
   Enjoy!!
  
   
-----------------------------------------------------------------------------
  
   This document was translated from LaTeX by HeVeA (2).
--------------------------------------
  
  
 (1) http://www.ocaml.info/
 
 (2) http://hevea.inria.fr/index.html
