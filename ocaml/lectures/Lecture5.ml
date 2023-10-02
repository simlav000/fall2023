(* HIGHER ORDER FUNCTIONS *)

type suit = Diamonds | Clubs | Hearts | Spades
type rank = Ace | Jack | Queen | King | Numeric of int
type card = suit * rank
type hand = Empty | More of card * hand

let f x y = x + y   (* val f: int -> int -> int = <fun> We know this function takes in an int and outputs an int thanks to the integer "+" operator *)
let g x y = x +. y  (* val g: float -> float -> float = <fun> We know this one outputs floats thanks to the "+." operator *)
let h x y = (x, y)  (* val h: 'a -> 'b -> 'a * 'b = <fun> *)
(* This one is not so clear. 'a and 'b are type variables. They can stand in for any type only one at at time, but are type agnostic*)

let id x = x        (* val 'a -> 'a = <fun> *)

let rec oops x = oops x (* Let's try to trace what happens if we were to call oops 5 *)

(*
   oops 5 
   oops 5
   oops 5
   oops 5
   ...
*)

(* This function never returns... so what is it's type? *)

(* 
  As it turns out, it is 'a -> 'b. It is promising to return a value of a type it knows nothing about.
  This makes no sense, since it cannot conjure up some 'b out of nowhere.
  It does not break it's promise however. It simply never returns.
*)

let rec hand_length (h: hand) : int =
  match h with
  | Empty -> 0
  | More (_, h) -> 1 + hand_length h

(*
   This function is nice, but it only works on our special type of "hand". What we really want is a polymorphic function 
   that can work on a list of anything. To start with this, let's make a generic list type.
*)

type 'a mylist =           
  | Nil
  | Cons of 'a * 'a mylist (* This is similar to the Java list <T> *)

let naturals = Cons (0, Cons (1, Cons (2, Nil)))
let cat_names = Cons ("Socks", Cons ("Whiskers", Nil))
(* let bogus = Cons (10, Cons("Illegal", Nil)) <- not allowed *)

(* This is very powerful. The mylist type now allows us to make lists of any type, so long as only one type is present in any list*)

let rec length (l: 'a mylist) : int =
  match l with
  | Nil -> 0
  | Cons (_, l) -> 1 + length l

(* Same function as before! But generic as fuck *)

let rec cat (l1 : 'a mylist) (l2 : 'a mylist) : 'a mylist = 
  match l1 with
  | Nil -> l2
  | Cons (e, l1') -> Cons (e, cat l1' l2)

let one_two_three = Cons (1, Cons (2, Cons (3, Nil)))
let four_five_six = Cons (4, Cons (5, Cons (6, Nil)))

(*
   cat one_two_three four_five_six
   match Cons( 1, Cons (2, Cons (3, Nil))) with
   | Nil -> Cons (4, Cons (5, Cons (6, Nil)))
   | Cons (e, l1) -> Cons (e, cat l1 (Cons (4, Cons (5, Cons (6, Nil)))))

   Match with second clause, return Cons (1, cat (Cons (2, Cons (3, Nil))) ((Cons (4, Cons (5, Cons (6, Nil))))) )
   ... same idea for all elements e of l1 until we hit Nil
   Cons (1, Cons (2, Cons (3, cat Nil ((Cons (4, Cons (5, Cons (6, Nil))))))))
   But with the base case, 
   match l1 with
   | Nil -> l2
   reads:
   match Nil with
   | Nil -> (Cons (4, Cons (5, Cons (6, Nil))))
   and so l2 gets appended to the end of the list in place of l1's Nil
   *)

(* 
	 Next we are taked with writing a function: find_all which returns a hand containing all the 
	 cards of some desired suit from a given hand
*)

let rec find_all (search : suit) (h: hand) : hand =
  match h with
  | Empty -> Empty
  | More ((s, r), h) when s = search -> More ((s, r), find_all search h) (* if suit matches, we nest it in this new hand *)
  | More (_, h) -> final_ all search h (* if suit doesn't match, we keep going down the hand without appending to hand *)

(* We want to make a  more generic version of this, some sort of filter for our list *)

let rec filter (p: 'a -> bool) (xs: 'a mylist) : 'a mylist = 
  (* 
     here we're taking as input a list of anything (xs), 
     and we also take in a FUNCTION p to check a certain property that will do the filtering
  *)
  match xs with
  | Nil -> Nil
  | Cons (x, xs') when p x -> Cons (x, filter p xs')
  | Cons (x, xs') -> filter p xs'

let queen_of_hearts = (Hearts, Queen)
let ace_of_spades = (Spades, Ace)
let three_of_clubs = (Clubs, Numeric 3)

let as_hand = More (queen_of_hearts, More (ace_of_spades, More (three_of_clubs, Empty)))
let as_list = Cons (queen_of_hearts, Cons (ace_of_spades, Cons (three_of_clubs, Nil  )))

let find_all_list (search: suit) (l: card mylist) : card mylist = 
  let correct_suit (s,_) = (s = search) in 
  filter correct_suit l

(* 
   This is an okay, and fairly familiar way to accomplish the desired result.
   We can also do something similar to python lambda functions, where we refuse 
   to give the inner function a name.
*)

let find_all_list' (search: suit) (l: card mylist) : card mylist =
  filter (fun (s,_) -> s = search) l

(* This is exactly the same as before. You may choose between either syntax. Whatever seems more readable in the moment *)

(* 
   As an exercise, try to write:
   let rec map (f: 'a -> 'b) (xs: 'a mylist) : 'b mylist = 
    failwith "TODO"
    
   example:
   let x = [1; 2; 3;]
   map string_of_int x
   >>> ["1"; "2"; "3"]
*)