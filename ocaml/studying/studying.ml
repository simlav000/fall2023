type 'b church = ('b -> 'b) -> 'b -> 'b

let to_int (n: int church) : int =
  n (fun x -> x + 1) 0

let is_zero (n: 'b church) : bool =
  n (fun _ -> false) true

let is_zero_simon (n: 'b church) : bool = 
  let s z = false in 
  n s true

let is_odd (n : 'b church) : bool =
  let switch = false in
  n (fun switch -> switch == false) switch

let simon_is_odd (n: 'b church) : bool =
  let s z = z == false in 
  n s false

