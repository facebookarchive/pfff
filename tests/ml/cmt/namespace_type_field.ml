type project = A | B

type record = {
  project: project;
}

let f x = 
  match x with
  { project = v } -> v

let g x =
  match x with
  | A -> 1
  | _ -> 2




