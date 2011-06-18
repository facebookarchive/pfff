// ****************************************************************************
// Prelude
// ****************************************************************************
// this file must be passed to the -macros option of the C/C++ parsers in pfff

// ****************************************************************************
// MacroString
// ****************************************************************************

/* String macros are normally handle quite well by the lalr(k) technique,
 * but sometimes it's not enough. For instance in the XX YY case, it could
 * be considered as a declaration with XX being a typedef, so we would
 * have an ambiguity. So by adding a few special cases (e.g. KERN_WARNING
 * for the linux kernel), we can catch more correct string-macros.
 * I need to have at least one witness, a string.
 */
//#define KERN_WARNING "WARNING"

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

//#define __init YACFE_ATTRIBUTE

// TODO: could perhaps generalize via "__.*"

// linkage

// params (IN, OUT)

// windows: WINAPI, STDCALL, ...

// ****************************************************************************
// MacroKeywordAlias
// ****************************************************************************

// const, often defined via macro for backward compatibility with old compiler
// I guess.

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
// should be moved in a facebook.h at some point instead of abusing macros.h

#define FBUNIT_TEST(a) void a()
#define FBUNIT_FIXTURE_TEST(a, b) void a()

// boost?
#define FOR_EACH(a,b) for(;;)

// boost too?
#define lexical_cast static_cast
#define dynamic_pointer_cast static_cast

// google gflags
#define DEFINE_int32(a,b,c) int a = b
#define DEFINE_string(a,b,c) int a = b
