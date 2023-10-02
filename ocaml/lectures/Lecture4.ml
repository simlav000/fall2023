(* 
  type rank = int
  type suit = Diamonds | Spades | Hearts | Clubs
  type card = suit * rank

  let get_card_name ((suit, rank): card) : string = 
      let suit_name = match suit with
        | Hearts   ->  "hearts"
        | Spades   ->  "spades"
        | Clubs    ->  "clubs"
        | Diamonds ->  "diamonds"
      in
      let rank_name = match rank with
        | 1       -> "ace"
        | 11      -> "jack"
        | 12      -> "queen"
        | 13      -> "king"
        | _       -> string_of_int rank
      in 
      rank_name ^ " of " ^ suit_name

    The above definition of a card in OCaml is alright, but it still allows the user to define
    cards such as "the -1 of hears" so we would like a way around this. We could always go

    type rank =
    | One
    | Two
    | Three
    | ...

    This sucks.
    Instead, we can do
*)
type suit = Diamonds | Spades | Hearts | Clubs

type rank = 
  | Ace
  | King
  | Queen
  | Jack
  | Num of int

type card = suit * rank

(*
   We should make sure that the user never has direct access to the constructors of rank. Instead, we provide them
   functions to construct them intead, and those functions would check the necessary conditions first, stopping them 
   from making a -1 of hearts
*)

type rank_making_result = 
      | BadNumber
      | Rank of rank

let make_num_card n = if 2 <= n && n <= 10 then Rank (Num n) else BadNumber

(*
   Equipped with this function, we can return either a card of the desired rank if an appropriate value is requested,
   and return a warning that the operation was unsuccessful. We will later learn some ways to do this more abstractly.
*)

let ace_of_spades = (Spades, Ace)
let ten_of_diamonds = (Diamonds, Num 10)

(*
    Here, Num is a consructor. So is Diamond, and Queen, and etc. When we wrote Num of int, int is 
    like the field being set to Num. Now let's fix get_card_name to work with this new rank definition.
*)

let get_card_name' ((suit, rank): card) : string =
  let suit_name = match suit with
        | Hearts   ->  "hearts"
        | Spades   ->  "spades"
        | Clubs    ->  "clubs"
        | Diamonds ->  "diamonds"
      in
      let rank_name = match rank with
        | Ace       -> "ace"
        | Jack      -> "jack"
        | Queen     -> "queen"
        | King      -> "king"
        | Num n     -> string_of_int n
        (*    ^ here we name the int that was packaged with Num when defining a card, like in the ten_of_diamonds to n    *)
      in 
      rank_name ^ " of " ^ suit_name

(* FORMAL SEMANTICS OF PATTERN MATCHING*)

(*
   match e with p_1 -> e_1 | ... | p_N -> e_N
   Syntax:
    * e is an expression, usually a variable
    * Each p_i is a pattern, i.e.
      * A variable such as x or a wildcard _. These variables are bound in the corresponding expression e_i.
      * A tuple of other patterns: (p_1,....,p_N)
      * A constructor applied to patterns: Cnstr (p_1,...,p_N)
    * Each e_i is an expression to evaluate, contigent on p_i matching, together with variable bindings
      coming from p_i

   Operational Semantics:
    1. Evaluate e to a value v.
    2. Line up v against each pattern p_i until we find one that matches
    3. Patterns can contain variables,; these becomes bound to the corresponding part of v
    4. The associated expression of the first matching pattern is evaluated with any new 
       bindings and gives the result of the whole match-expression
*)

(** RECURSIVE TYPES **)

type hand = 
    | Empty
    | More of card * hand

let e = Empty                             (* start with empty hand *)
let hand1 = More ((Diamonds, Num 3), e)   (* Add 3 of diamonds to hand e *)
let hand2 = More ((Spades, Num 5), hand1) (* Add 5 of spades to hand hand1*)

(*
      This is a list that can hold cards. Here is a tail-recursive function that computes
      the number of cards in a hand 
*)

let length hand =
  let rec length' hand partial_length = match hand with
    | Empty           -> partial_length
    | More (_, hand') -> length' hand' (partial_length + 1)
  in length' hand 0

(*
     let find_card (suit : suit) (h: hand) : ??? = 
     write a function that finds the first card in this hand that has the given suit 
     it is possible for there to be a bogus hand not containing any cards of this suit
*)

type find_result = 
	| NotFound
	| Found of card

let rec find_card (suit: suit) (h: hand) : find_result =
	match h with
	| Empty -> NotFound 
	| More ((s, r), h') -> 
		if s = suit then Found (s, r) else find_card suit h'
		(* check if c has the given suit: return Found c if it does, otherwise, recurse on h' *)

(* 
	 Next we are taked with writing a function: find_all_cards which returns a hand containing all the 
	 cards of some desired suit from a given hand
*)

let find_all_cards (suit: suit) (h: hand) : hand =
	(* construct a hand consisting of all the cards of the given suit from the given hand *)
	failwith "TODO"



