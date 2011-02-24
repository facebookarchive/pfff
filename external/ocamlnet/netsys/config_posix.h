/* What one can expect from any POSIX system: */

#define HAVE__EXIT
#define HAVE_SYSCONF
#define HAVE_POSIX_SIGNALS
  /* sigaction, sigprocmask etc. */
#define HAVE_POSIX_PROCESS_GROUPS
  /* getpgid, setpgid */
#define HAVE_POSIX_PROCESS_SESSIONS
  /* getsid, setsid */
#define HAVE_POSIX_TTY
  /* tcgetpgrp, tcsetpgrp, ctermid, ttyname. 
     Implies HAVE_POSIX_PROCESS_GROUPS, HAVE_POSIX_PROCESS_SESSIONS,
     and HAVE_POSIX_SIGNALS
   */
#define HAVE_POSIX_UID
  /* setreuid, getreuid */
#define HAVE_FSYNC
#define HAVE_FDATASYNC
#define HAVE_POLL
#define HAVE_FORK_EXEC
  /* fork, execve, pipe */
#define HAVE_MMAP

