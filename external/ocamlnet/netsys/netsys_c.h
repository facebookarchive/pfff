/* $Id: netsys_c.h 1550 2011-02-15 13:51:12Z gerd $ */

#include "config.h"

/* Linux: make all system prototypes available */
#define _GNU_SOURCE

/* POSIX: we want POSIX.1-2008 if possible */
/* #define _XOPEN_SOURCE 700 */


#ifdef _WIN32
#include "config_win32.h"
#include <stdio.h>

#define _WIN32_WINNT 0x0502
#define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <errno.h>
#include <aclapi.h>

#ifndef FILE_FLAG_FIRST_PIPE_INSTANCE
#define FILE_FLAG_FIRST_PIPE_INSTANCE 0x00080000
#endif

#else

#include "config_posix.h"
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif

#include <string.h>

#include "caml/misc.h"
/* misc.h also includes OCaml's config.h
   All HAS_* macros come from there. All HAVE_* macros come from our own
   config.h
*/

#include "caml/mlvalues.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/fail.h"
#include "caml/signals.h"
#include "caml/custom.h"
#include "caml/callback.h"
#include "caml/bigarray.h"


#ifdef HAVE_POLL
#define CONST_POLLIN POLLIN
#define CONST_POLLPRI POLLPRI
#define CONST_POLLOUT POLLOUT
#define CONST_POLLERR POLLERR
#define CONST_POLLHUP POLLHUP
#define CONST_POLLNVAL POLLNVAL
#else
#define CONST_POLLIN 0x1
#define CONST_POLLPRI 0x2
#define CONST_POLLOUT 0x4
#define CONST_POLLERR 0x8
#define CONST_POLLHUP 0x10
#define CONST_POLLNVAL 0x20
#endif

/**********************************************************************/
/* From memory.h                                                      */
/**********************************************************************/

#ifdef FANCY_PAGE_TABLES
#define Not_in_heap 0
#define In_heap 1
#define In_young 2
#define In_static_data 4
#define In_code_area 8
int caml_page_table_add(int kind, void * start, void * end);
int caml_page_table_remove(int kind, void * start, void * end);
#endif

/**********************************************************************/
/* From unixsupport.h                                                 */
/**********************************************************************/

#define Nothing ((value) 0)

#ifdef _WIN32

struct filedescr {
  union {
    HANDLE handle;
    SOCKET socket;
  } fd;
  enum { KIND_HANDLE, KIND_SOCKET } kind;
  int crt_fd;
};

#define Handle_val(v) (((struct filedescr *) Data_custom_val(v))->fd.handle)
#define Socket_val(v) (((struct filedescr *) Data_custom_val(v))->fd.socket)
#define Descr_kind_val(v) (((struct filedescr *) Data_custom_val(v))->kind)
#define CRT_fd_val(v) (((struct filedescr *) Data_custom_val(v))->crt_fd)

#define NO_CRT_FD (-1)

/* These functions are actually defined in unixsupport_w32.c */

value netsysw32_unix_error_of_code (int errcode);
void netsysw32_unix_error (int errcode, char * cmdname, value arg) Noreturn;
void netsysw32_uerror (char * cmdname, value arg) Noreturn;

void netsysw32_win32_maperr(DWORD errcode);

value netsysw32_win_alloc_handle_or_socket(HANDLE);
value netsysw32_win_alloc_handle(HANDLE);
value netsysw32_win_alloc_socket(SOCKET);

/* Keep the API: */

#define unix_error_of_code         netsysw32_unix_error_of_code
#define unix_error                 netsysw32_unix_error
#define uerror                     netsysw32_uerror
#define win32_maperr               netsysw32_win32_maperr
#define win_alloc_handle_or_socket netsysw32_win_alloc_handle_or_socket
#define win_alloc_handle           netsysw32_win_alloc_handle
#define win_alloc_socket           netsysw32_win_alloc_socket

#else

/* POSIX */

/* Since OCaml 3.10 there is unixsupport.h, and we could also include
   this file.
*/

extern value unix_error_of_code (int errcode);
extern void unix_error (int errcode, char * cmdname, value arg) Noreturn;
extern void uerror (char * cmdname, value arg) Noreturn;

#define DIR_Val(v) *((DIR **) &Field(v, 0))

#endif

/**********************************************************************/
/* From signals.h                                                     */
/**********************************************************************/

CAMLextern int caml_convert_signal_number (int);
CAMLextern int caml_rev_convert_signal_number (int);

/**********************************************************************/
/* From socketaddr.h                                                  */
/**********************************************************************/

#ifdef HAS_SOCKLEN_T
typedef socklen_t socklen_param_type;
#else
typedef int socklen_param_type;
#endif

union sock_addr_union {
    struct sockaddr s_gen;
#ifndef _WIN32
    struct sockaddr_un s_unix;
#endif
    struct sockaddr_in s_inet;
#ifdef HAS_IPV6
    struct sockaddr_in6 s_inet6;
#endif
};

#define GET_INET_ADDR(v) (*((struct in_addr *) (v)))

#ifdef HAS_IPV6
#define GET_INET6_ADDR(v) (*((struct in6_addr *) (v)))
#endif


/**********************************************************************/
/* bigarrays                                                          */
/**********************************************************************/

extern int caml_ba_element_size[];
