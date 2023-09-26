(* HIGHER ORDER FUNCTIONS PART 2 *)

(* But first, a look at built-in OCaml lists *)

type 'a list = 
    | []
    | (::) of 'a * 'a list

(*
  The built-in Nil is written suggestively as empty brackets.
  The built-in Cons is an infix operator :: (still pronounced "cons").
  In words:
   * [] (nil) has type 'a list for any type 'a
   * x :: xs (x cons xs) has type 'a list provided that x : 'a and xs : 'a list

  Example:
    1 :: 2 :: []
*)

let l1 = 1 :: 2 :: 3 :: []

(* We also have access to the other way to build lists, namely: *)

let l2 = [1 ; 2; 3]

(* and we may jump between syntax *)

let l3 = 1 :: [2 ; 3]

(* OCAML OPTIONS *)

(* remember the rank_making_result type from a previous lecture? *)
type rank = 
  | Ace 
  | Queen 
  | King 
  | Jack
  | Numeric of int

type rank_making_result = 
  | BadNumber
  | GoodRank of rank

(* This type is intended to stop the user from being able to request the generation of a bogus card *)
(* We had also seen the find_result type *)

type find_result = 
  | NotFound
  | Found of rank

(* OCaml has a very similar polymorphic type to both of these *)

type 'a option = 
  | None
  | Some of 'a

(* We will use it a lot, and you will also see it in many library functions *)

(* EXERCISE: write a function which reverses a list of any generic type *)

let rev (l : 'a list) : 'a list = 
  let rec go l l' =
    match l with
    | [] -> l'
    | e :: l -> go l (e :: l')
  in go l ([])

(* DEMO: Higher-order functio with option as return type *)

let rec find (p : 'a -> bool) (xs : 'a list) : 'a option =
  match xs with
  | [] -> None 
  | x :: xs ->
    if p x then Some x
    else find p xs 

type 'b church = ('b -> 'b) -> 'b -> 'b

let to_int (n : int church) : int =
  let succ acc = acc + 1 in
  n succ 0
  
(* these church numbers are higher order functions which take two inputs.
    > The first input is a function of type ('b -> 'b)
    > The second input is a value of 'b. 
    > finally, it returns something of type 'b
    >>> this is why we have "type 'b church = ('b -> 'b) -> 'b -> 'b"
   In the to_int function, we are defining a helper function called succ, which takes as input 
   an accumulator (which is an integer), and returns the successor of said integer. I.E., it is 
   a function from (int -> int). Therefore, 'b is an int in the context of the to_int function.

   The magic happens when we write "in n succ 0"
   'n' here is a FUNCTION of type "int church", which takes as input the function succ (int -> int), and
   an initial accumulator of 0 (also an int) and returns an int. I.E. (int -> int) -> int -> int.
*)





