#define A_CONSTANT 42

// if you modify the header file, you may need to regenerate
// all .clang, so make clean; make


extern int global;

int bar(int);

#if defined(__cplusplus)
#define	__BEGIN_DECLS	extern "C" {
#define	__END_DECLS	}
#else
#define	__BEGIN_DECLS
#define	__END_DECLS
#endif

__BEGIN_DECLS
void func_in_extern_C();
__END_DECLS

//extern "C" {
//void func_really_in_extern_C();
//}

