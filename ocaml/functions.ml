let exp_int base exponent = float_of_int base ** float_of_int exponent |> int_of_float
let sqrt (n : int) : int = 
  let rec ssqrt (i : int) n : int =
    if i * i > n 
    then i - 1
    else ssqrt (i + 1) n
  in ssqrt 0 n


let is_prime (n : int) : int = 
  let rec go (i : int) n : int =
    (*if we get to this point it's prime*)
    if i = n
    then 1 

    (*ints less than 2 are prime, return 1*)
    else if n <= 2
    then 1 

    (*if the n was divisible by i on the way from 2 to n, it is not prime*)
    else if n mod i = 0
    then 0

    else go (i + 1) n
  in go 2 n

let hailstone (n : int) : int = 
  (* idea: Make inner function take as input both n, and another int
     to keep track of the count to be returned later *)
  let rec hs_count n count = 
    if n = 1
    then count
    else if n mod 2 = 0
    then hs_count (n / 2) (count + 1) 
    else hs_count (3*n + 1) (count + 1)
  in hs_count n 0






let hi 
