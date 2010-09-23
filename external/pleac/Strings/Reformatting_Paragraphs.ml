(* ********************************************************************** *)
(* Reformatting Paragraphs *)
(* ********************************************************************** *)
let pleac_Reformatting_Paragraphs () = 
  
  (* We can emulate the Perl wrap function with the following function *)
  let wrap width s =
    let l = Str.split (Str.regexp " ") s in
    Format.pp_set_margin Format.str_formatter width;
    Format.pp_open_box Format.str_formatter 0;
    List.iter 
      (fun x -> 
        Format.pp_print_string Format.str_formatter x;
        Format.pp_print_break Format.str_formatter 1 0;) l;
    Format.flush_str_formatter ();;
  
  (*
  # let st = "May I say how lovely you are looking today... this wrapping has done wonders for your figure!\n";;
  val st : string =
    "May I say how lovely you are looking today... this wrapping has done wonders for your figure!\n"
  
  # print_string (wrap 50 st);;
  May I say how lovely you are looking today...
  this wrapping has done wonders for your figure!
  
  # print_string (wrap 30 st);;
  May I say how lovely you are
  looking today... this
  wrapping has done wonders for
  your figure!
  *)
  
  (* Note that this version doesn't allow you to specify an opening or standard
   * indentation (I am having trouble getting the Format module to behave as I
   * think it should...).  However, if one only wants to print spaces there
   * instead of arbitrary line leaders, we can use the following version *)
  
  let wrap ?(lead=0) ?(indent=0) width s =
    let l = Str.split (Str.regexp " ") s in
    Format.pp_set_margin Format.str_formatter width;
    Format.pp_open_box Format.str_formatter 0;
    Format.pp_print_break Format.str_formatter lead indent;
    List.iter 
      (fun x -> 
        Format.pp_print_string Format.str_formatter x;
        Format.pp_print_break Format.str_formatter 1 indent;) l;
    Format.flush_str_formatter ();;
  
  (*
  # print_string (wrap 20 st);;
  May I say how
  lovely you are
  looking today...
  this wrapping has
  done wonders for
  your figure!
   - : unit = ()
  
  # print_string (wrap ~lead:6 ~indent:2 20 st);;
        May I say how
    lovely you are
    looking today...
    this wrapping has
    done wonders for
    your figure!
  
  # print_string (wrap ~lead:2 20 st);;
    May I say how
  lovely you are
  looking today...
  this wrapping has
  done wonders for
  your figure!
  *)
  

