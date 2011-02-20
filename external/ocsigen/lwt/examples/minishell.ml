
(* A very simple shell *)

open Lwt
open Lwt_term

module TextSet = Set.Make(Text)

(* Build the list of executales of the system *)
let executables = lazy begin
  (* We use a preemptive thread here because it is not possible to
     list cooperatively a directory: *)
  Lwt_preemptive.detach begin fun () ->
    List.fold_left
      (fun set path ->
         Array.fold_left
           (fun set fname ->
              if try Unix.access (Filename.concat path fname) [Unix.X_OK]; true with _ -> false then
                TextSet.add fname set
              else
                set)
           set
           (Sys.readdir path))
      TextSet.empty
      (Text.split
         ~sep:":"
         (try Sys.getenv "PATH" with Not_found -> "/usr/bin:/bin"))
  end ()
end

(* Prompt creation *)
let prompt exit_code =
  let tm = Unix.localtime (Unix.time ()) in
  let code = string_of_int exit_code in
  let path = Sys.getcwd () in
  let path =
    try
      let home = Sys.getenv "HOME" in
      if Text.starts_with path home then
        Text.splice path 0 (Text.length home) "~"
      else
        path
    with Not_found ->
      path
  in
  let path_len = Text.length path in
  let columns = React.S.value Lwt_term.columns in
  let size_for_path = columns - 24 - Text.length code in
  let path =
    if path_len > size_for_path then
      if size_for_path >= 2 then
        ".." ^ Text.slice path (path_len - size_for_path + 2) path_len
      else
        path
    else
      path
  in
  [Bold;
   fg lblue; Text "─( ";
   fg lmagenta; textf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec;
   fg lblue; Text " )─< ";
   fg lyellow; Text path;
   fg lblue; Text " >─"; Text(Text.repeat (columns - 24 - Text.length code - Text.length path) "─"); Text "[ ";
   fg (if exit_code = 0 then lwhite else lred); Text code;
   fg lblue; Text " ]─";
   fg lred; Text(try Sys.getenv "USER" with Not_found -> "");
   fg lgreen; Text "@";
   fg lblue; Text(Unix.gethostname ());
   fg lgreen; Text " $ "]

(* Read lines and execute them.

   @param exit_code is the exit code of the previous command *)
let rec loop exit_code history =
  lwt line = Lwt_read_line.read_line
    ~history
    ~complete:(fun (before, after) ->
                 lwt words = Lazy.force executables in
                 return (Lwt_read_line.complete "" before after words))
    ~prompt:(prompt exit_code)
    ()
  in
  lwt code = Lwt_unix.system line >|= function
    | Unix.WEXITED code ->
        code
    | _ ->
        255
  in
  loop code (Lwt_read_line.add_entry line history)

let () = Lwt_main.run (loop 0 [])
