

// do not compute deps inside macro, will get undefined typedef for instance 
// here
#define va_start(list, start) list =\
	(sizeof(start) < 4?\
		(char*)((int*)&(start)+1):\
		(char*)(&(start)+1))


#define use_global_bad(a) \
   undefined_global_ok = a;
