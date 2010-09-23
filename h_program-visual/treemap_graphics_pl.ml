open Common 

module Flag = Flag_program_visual

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let info_of_file_default = (fun status (f, size) ->
  let s = f in
  if status.Graphics.button 
  then begin 
    pr2 (spf "%s" f);
    if !Flag.use_emacsclient
    then 
      Sys.command (spf "/home/pad/packages/Linux/bin/emacsclient -n %s" f) +> ignore;
  end;
  
  if status.Graphics.keypressed (* Graphics.key_pressed () *)
  then (raise (UnixExit  0));
  s
)


(*****************************************************************************)
(* Graphics helpers *)
(*****************************************************************************)

let init_graph big_screen = 

  let w_view_hint, h_view_hint = 
    if big_screen
    then
      2300, 1500 
    else
      900, 640
  in
  let h_status = 30 in
  let w_legend = 200 in
  
  Graphics.open_graph 
    (spf " %dx%d" (w_view_hint + w_legend) (h_view_hint+ h_status));
  Graphics.set_color (Graphics.rgb 1 1 1);
  Treemap.current_dim ~w_legend ~h_status


let draw_legend dim = 

  let legendx = dim.T.w_view + 10 in
  let legendy = ref 200 in
  Archi_code.source_archi_list +> List.rev +> List.iter (fun kind ->
    legendy += 20;
    let color = color_of_source_archi kind in
    let c = Color.degrade color Color.Degrade2 in
    Graphics.set_color c;
    Graphics.fill_rect legendx !legendy 10 10;
    Graphics.moveto (legendx + 20) (!legendy);
    Graphics.draw_string (Archi_code.s_of_source_archi kind);
    ()
  );
  ()


(*****************************************************************************)
(* Treemap *)
(*****************************************************************************)

let treemap_code ?(big_screen=false) dir ~func =

  let dim = init_graph big_screen in
  draw_legend dim;
  let treemap = func dir in

  Treemap.display_treemap_interactive 
    ~algo:!Flag.algo 
    ~info_of_file_under_cursor:info_of_file_default
    treemap dim
  ;
  ()


(* todo: remove duplication, was from treemap_pfff.ml *)
let treemap_code_pfff ?(big_screen=false) dir =

  let dim = TPL.init_graph big_screen in
  TPL.draw_legend dim;

  let treemap = code_treemap dir in

  Treemap.display_treemap_interactive 
    ~algo:!Flag.algo
    ~drawing_file_hook:(fun rect (f, size, aref_status)  mat ->
      !aref_status +> List.iter (fun _vid ->
        Graphics.set_color (Color.c "firebrick");
        let pt = F.random_point_in_rect rect in
        Graphics.fill_circle 
          pt.F.x
          pt.F.y
        4
      )
    )
    ~info_of_file_under_cursor:(fun status (f, size, aref_status) ->
      let file = Common.filename_without_leading_path dir f in
      let s = 
        match !aref_status with
        | [] -> file
        | [(vid, _info)] -> 
            spf "%s ! %s" file (Git.commit_summary ~basedir:dir vid)
        | (vid, _info)::y::xs ->
            spf "%s !! %s ..." file (Git.commit_summary ~basedir:dir vid)
      in
      if status.Graphics.button 
      then begin 
        pr2 (spf "%s" f);
        if !Flag.use_emacsclient
        then 
          Sys.command (spf "/home/pad/packages/Linux/bin/emacsclient -n %s" f) +> ignore;

        !aref_status +> List.iter (fun (vid, _info) ->
          pr2_xxxxxxxxxxxxxxxxx ();
          (* TODO do slice relevant ? *)
          let raw_info = Git.commit_raw_patch ~basedir:dir vid in
          raw_info +> List.iter pr2;
          pr2_xxxxxxxxxxxxxxxxx ();
          pr2 (Git.commit_summary ~basedir:dir vid)
        );
      end;
      if status.Graphics.keypressed (* Graphics.key_pressed () *)
      then raise (UnixExit 0);
      s
    )
    treemap dim
  ;
  ()



let actions () = [
  "-test_treemap_code", "<dir>",
  Common.mk_action_1_arg (treemap_code ~big_screen:!Flag.big_screen
                             ~func:code_treemap
  );
  "-test_treemap_linux", "<dir>",
  Common.mk_action_1_arg (treemap_code ~big_screen:!Flag.big_screen
                             ~func:Treemap_ex_linux.code_treemap
  );
  "-test_treemap_pfff", "<dir>",
  Common.mk_action_1_arg (treemap_code_pfff ~big_screen:!Flag.big_screen);
]
