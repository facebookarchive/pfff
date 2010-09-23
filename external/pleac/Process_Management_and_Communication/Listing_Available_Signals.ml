(* ********************************************************************** *)
(* Listing Available Signals *)
(* ********************************************************************** *)
let pleac_Listing_Available_Signals () = 
  % echo 'module M = Sys;;' | ocaml | grep 'val sig'
      val sigabrt : int
      val sigalrm : int
      val sigfpe : int
      val sighup : int
      val sigill : int
      val sigint : int
      val sigkill : int
      val sigpipe : int
      val sigquit : int
      val sigsegv : int
      val sigterm : int
      val sigusr1 : int
      val sigusr2 : int
      val sigchld : int
      val sigcont : int
      val sigstop : int
      val sigtstp : int
      val sigttin : int
      val sigttou : int
      val sigvtalrm : int
      val sigprof : int
  
  % grep -A1 'val sig' sys.mli
  val sigabrt : int
  (** Abnormal termination *)
  --
  val sigalrm : int
  (** Timeout *)
  --
  val sigfpe : int
  (** Arithmetic exception *)
  --
  val sighup : int
  (** Hangup on controlling terminal *)
  --
  val sigill : int
  (** Invalid hardware instruction *)
  --
  val sigint : int
  (** Interactive interrupt (ctrl-C) *)
  --
  val sigkill : int
  (** Termination (cannot be ignored) *)
  --
  val sigpipe : int
  (** Broken pipe *)
  --
  val sigquit : int
  (** Interactive termination *)
  --
  val sigsegv : int
  (** Invalid memory reference *)
  --
  val sigterm : int
  (** Termination *)
  --
  val sigusr1 : int
  (** Application-defined signal 1 *)
  --
  val sigusr2 : int
  (** Application-defined signal 2 *)
  --
  val sigchld : int
  (** Child process terminated *)
  --
  val sigcont : int
  (** Continue *)
  --
  val sigstop : int
  (** Stop *)
  --
  val sigtstp : int
  (** Interactive stop *)
  --
  val sigttin : int
  (** Terminal read from background process *)
  --
  val sigttou : int
  (** Terminal write from background process *)
  --
  val sigvtalrm : int
  (** Timeout in virtual time *)
  --
  val sigprof : int
  (** Profiling interrupt *)
  

