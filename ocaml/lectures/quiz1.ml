let rec factorial n acc =
  if n == 0 then acc
  else
    factorial (n - 1) (n*acc)

let result = factorial 5 1
type shape = 
    | Square of float 
    | Mix of shape * shape
let c = Square 2.0 in Mix (c,c)