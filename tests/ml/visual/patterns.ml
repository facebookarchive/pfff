open X
module A = Y

let x = function
  | Xxx(Y1, Y2, Y3) -> raise Todo
  | Xxx( [Y1, Y2, Y3]) -> raise Todo
