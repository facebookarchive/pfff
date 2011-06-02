// ****************************************************************************
// Prelude
// ****************************************************************************
// this file is to be used with the -macros option of the C/C++ parsers in pfff

// ****************************************************************************
// MacroString
// ****************************************************************************

/* String macros are normally handle quite well by my lalr(k) technique,
 * but sometimes it's not enough, for instance in the XX YY case, it could
 * be considered as a declaration with XX being a typedef, so we would
 * have an ambiguity. So at least by adding this special case, we can
 * catch more correct string-macro, no more a XX YY but now a good
 * "XX" YY 
 * 
 * cf include/linux/kernel.h
 *
 * For stringification I need to have at least a witness, a string, 
 * and sometimes have just printk(KERN_WARNING MYSTR) and it could
 * be transformed in a typedef later, so better to at least
 * transform in string already the string-macro we know.
 * 
 * Perhaps better to apply also as soon as possible the 
 * correct macro-annotation tagging (__init & co) to be able to
 * filter them as soon as possible so that they will not polluate
 * our pattern-matching that come later.
 */

/* EX_TABLE & co. 
 *
 * Replaced by a string. We can't put everything as comment
 * because it can be part of an expression where we wait for
 * something, where we wait for a string. So at least we 
 * must keep the EX_TABLE token and transform it as a string.
 *
 * normally not needed if have good stringification of macro 
 * but those macros are sometimes used multiple times 
 * as in EX_TABLE(0b) EX_TABLE(1b)  and we don't detect
 * it well yet.
 */

//#define EX_TABLE(x)  "TOTO"


// ****************************************************************************
// MacroIterator
// ****************************************************************************

// foreach

// ****************************************************************************
// MacroDeclarator
// ****************************************************************************

// also static DECLARATOR(x);

// also LIST_HEAD stuff 
// used in qemu, freebsd

// ****************************************************************************
// MacroStmt
// ****************************************************************************
// with or without parameters, but no ';'

// ****************************************************************************
// MacroField
// ****************************************************************************

// ****************************************************************************
// MacroInitializer
// ****************************************************************************

// no PTVirg or shortcut for array designator


// ****************************************************************************
// MacroAttributes
// ****************************************************************************
// TODO: could perhaps generalize via "__.*"

// linkage

// params (IN, OUT)

// windows: WINAPI, STDCALL, ...

//#define __init YACFE_ATTRIBUTE

// ****************************************************************************
// MacroKeywordAlias
// ****************************************************************************

// const, often defined via macro for backward compatibility with old compiler
// I guess.

// 

// private/public

// ****************************************************************************
// Prototype
// ****************************************************************************

// __P

// PARAMS

// ****************************************************************************
// Declarator
// ****************************************************************************

/* cf gcc-linux.h
 * A trick to suppress uninitialized variable warning without generating any
 * code
 */

// #define uninitialized_var(x) x = x
// as in u16 uninitialized_var(ioboard_type);	/* GCC be quiet */ 

// ****************************************************************************
// Misc
// ****************************************************************************

// LIST_HEAD

// GENTEST, GENHEADER

// structure
// MACHINE_START 

// iterator defined two times

// higher order, ASSERTCMP

// parts of stuff, start of stuff

// begin end wierd, as in C++ firefox and NS_DECLARE_BEGIN/END with code in
// the middle

// testcase, reflexivity on name

// IDENT in sparse, wierd case

// if-like macros

//#define G_BEGIN_DECLS


// ****************************************************************************
// Facebook stuff
// ****************************************************************************

#define FBUNIT_TEST(a) void a()

#define FOR_EACH(a,b) for(;;;)

#define lexical_cast static_cast

