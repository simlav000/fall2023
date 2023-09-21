(* TAIL RECURSION *)
(*
  consider the function:
*)

let rec sum n =
  if n = 0 then 0 else n + sum (n - 1)

(*
   this sums up all integers from 0 to n.
   the call stack looks a something like this:
    1. sum 5                                      -> (call the function with input )
    2. if 5 = 0 then 0 else 5 + sum (5 - 1)       -> substitute input value to function (this step will be omitted from following list)
    3. if flase then 0 else 5 + sum (4)           -> evaluate expression / choose else branch
    4. 5 + sum(4)                                 -> recursive call of sum(4)
    5. 5 + (4 + sum(3))                           -> recursive call of sum(3)
    6. 5 + (4 + (3 + sum(2)))                     -> recursive call of sum(2)
    7. 5 + (4 + (3 + (2 + sum(1))))               -> recursive call of sum(1)
    8. 5 + (4 + (3 + (2 + (1 + sum(0)))))         -> recursive call of sum(0)
    9. 5 + (4 + (3 + (2 + (1 + 0))))              -> reach base case
   10. 5 + ( 4 + (3 + (2 + 1)))                   -> add
   11. 5 + (4 + (3 + 3))                          -> add
   12. 5 + (4 + 6)                                -> add
   13. 5 + 10                                     -> add
   14. 15                                         -> finally return 15

   We can observe how tall the stack is getting by noting how nested the parentheses get
   Stack growth here is a consequence of our code structure:
    * there is something to do (namely add) AFTER the recursive calls complete
    * OCaml allows us to recycle stack frames if there is "nothing to do" after a recurive call!

   Let's improve upon this function by making it tail-recursive
*)

let sum_tr n =
  let rec sum' partial_sum n = 
    if n = 0 then partial_sum else
      sum' (partial_sum + n) (n - 1)
  in sum' 0 n

(*
   This does the same thing but let us now investigate the stack trace:
    1. sum_tr 5                                    -> (call the function with input )
    2. if 5 = 0 then 0 else sum' (0 + 5) (5 - 1)   -> substitute input value to function
    3. if false then 0 else sum' 5 4               -> evaluate expression / choose else branch
    4. sum' 5 4                                    -> make recursive call of sum' 5 4
    5. sum' 9 3                                    -> recursive call
    6. sum' 12 2                                   -> recursive call
    7. sum' 14 1                                   -> recursive call
    8. sum' 15 0                                   -> recursive call
    9. if 0 = 0 then 15 else sum' (15 + 0) (0 - 1) -> reach base case / choose if branch
   10. 15                                          -> Done!

   With this algorithm, we have no nesting of parentheses. The stack frame used for recursion can be recycled as 
   there is no need to create new ones!
   Note that in this algorithm, the partial_sum variable is passed in through the function doing most of the work
    * It is convention to make a function like sum' (which has a variable input the user does not care about) a nested function of 
      sum_tr so that the user does not need to constantly call sum_tr 0 n (where n is the number they care about summing). It is not the job
      of the user to know that the first variable is being used to remember the partial_sum.
    * A conventional name for such a variable is an "accumulator" often called "acc". It is better to provide a more relevant name, but acc is fine.
*)

let factorial (n: int) : int =
  let rec factorial' partial_product n =
    if n = 0 then
      partial_product
    else
      factorial' (partial_product * n) (n - 1)
    in factorial' 1 n

(* TYPES IN FUNCTIONAL PROGRAMMING *)

type number = int 

let factorial (n: number) : number =
  let rec factorial' partial_product n =
    if n = 0 then
      partial_product
    else
      factorial' (partial_product * n) (n - 1)
    in factorial' 1 n

(*
  >> utop # #use "Lecture3.ml";;
  >> val sum : int -> int = <fun>
  >> val sum_tr : int -> int = <fun>
  >> val factorial : int -> int = <fun>
  >> type number = int
  >> val factorial : number -> number = <fun>

  What we have just done here is defined a type synonym. The toplevel now recognizes "number" as a stand-in for int
*)

type student = string       (* Define a student to be represented by a string*)
type course = student list  (* Define a course to be a list of students*)

(* TUPLES *)
let mytuple = "We", "don't", "need", "parentheses"

(* 
  >> utop # #use "Lecture3.ml";;
  >> val mytuple : student * student * student * student =
     ("We", "don't", "need", "parentheses")
  
  Because I made a type synonym between student and string, and so when I defined mytuple, OCaml thinks it is a tuple of students
*)

(* LISTS *)
let students : course = 
  [
    "Simon";
    "Grimace";
  ]

(* 
   * This forces my list to be a course, with the strings being synonym'd to students
   * Any given list can only contain values of the same type
   * To include more types in a list, you can populate a list with tuples holding different types
      - this forces the list to only be able to hold that specific kind of tuple
*)

(*
   We can do something like:
*)

type suit = string

(* 
   and given we had already made a synonym between student and string, we can (unfortunately) get away with:
*)

let uhoh = 
  [
    "Simon";
    "Hearts";
  ]

(*
   and now OCaml thinks: val uhoh : suit list = ["Simon"; "Hearts"]
    - This is not good
*)

(* ALTERNATIVES *)

type suit = Diamonds | Spades | Hearts | Clubs
type card = suit * number

(*
   >> utop # Diamonds;;
   >> - : suit = Diamonds

    Now we can make sure OCaml knows that any Diamonds, Spades, Hearts or Clubs are a suit!
  * Just make sure to not do something like:

type gemstones = Diamonds | Rubies | Sapphires

  * "Diamonds" is a now gemstone, not a suit anymore
*)

let get_card_name ((suit, rank): card) : string = 
(*                    ^      ^
   destructuring binding, similar to unpacking in python
*)
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

(* MATCH STATEMENT AND FUNCTION SHORTHAND *)

let rec factorial_again (n : int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)

(*
   it is so common for a function to take some argument (such as n) and to attempt to take it apart
   using match statements, that the following shorthand was created, accomplishing the same thing
*) 

let rec factorial_again' = function
  | 0 -> 1
  | _ -> n * factorial (n - 1)