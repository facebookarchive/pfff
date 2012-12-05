
let h x = X.g x

module X = struct
    let f x = x
end

open X

let g x = f x

let k x = g x
