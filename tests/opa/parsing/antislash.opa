
  ws_length(s) =
    len = length(s)
    rec aux(pos) =
      if Int.equals(pos,len) then
        pos
      else
        match get(pos,s) with
        | " " | "\n" | "\r" | "\t" | /*"\v"*/"\11" | "\0" -> aux(pos+1)
        | _ -> pos
    aux(0)
