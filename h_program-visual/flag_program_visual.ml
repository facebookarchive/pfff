open Common

let algo = 
  ref (Treemap.Ordered Treemap.PivotByMiddle)

let big_screen = 
  ref false

let use_emacsclient = ref false

let use_git = 
  ref false



let cmdline_flags () = [
  "-algorithm", Arg.String (fun s ->
    algo := Treemap.algo_of_s s;
  ), 
  (spf " <algo> (choices are: %s, default = %s" 
      (Treemap.algos +> List.map Treemap.s_of_algo +> Common.join ", ")
      (Treemap.s_of_algo !algo));
  
  "-big_screen", Arg.Set big_screen, 
  " ";
  "-use_emacsclient", Arg.Set use_emacsclient, 
  " ";
  "-use_git", Arg.Set use_git, 
  " ";
]
