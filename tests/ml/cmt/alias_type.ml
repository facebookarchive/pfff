type list2 = int list

type use_list2 = 
  | Constructor of list2

let f = function
  | Constructor [] -> 1
  | _ -> 2

