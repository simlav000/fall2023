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





