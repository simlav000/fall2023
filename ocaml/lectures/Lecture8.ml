(* MIDTERM NOTES *)
(*
   - four questions
   - 1 short answer, 3 long answer

   * Short answer:
    - typechecking, scoping, tail-recursion

   * Long answer:
    - recursive algorithms,
    - tail-recrusive implementations
    - higher-order functions

   * Practice questions: https://github.com/tsani/comp-302-review
*)

(* so last class, we looked at some tree searching algorithms using continuations *)

type 'a tree = 
  | Empty 
  | Node of 'a tree * 'a * 'a tree 

(* We saw that we can split the option in the return type of 'find' into two separate continuations*)

let rec find (p: 'a -> bool) (t: 'a tree) : 'a option =
  match t with 
  | Empty -> None
  | Node (l,x,r) when p x -> Some x
  | Node (l,x,r) -> 
    match find p l with 
    | Some x -> Some x
    | None -> find p r

(* Using the general recipe for converting to Continuation-passing-style, it was possible to write 
   a CPS version of the above function *)

let rec find' (p: 'a -> bool) (t: 'a tree) (return: 'a option -> 'r) : 'r =
  match t with 
  | Empty -> return None 
  | Node (l,x,r) when p x -> return (Some x)
  | Node (l,x,r) ->
    find' p l (function
      | Some x -> return (Some x)
      | None -> find' p r return)

(* 
  However, we observed that _really_ the original functions have two different ways of returning
   1. A successful return (Some x)
   2. An unsuccessful one (None)
   These different ways of returning can be captured by separate continuations
*)

let rec find_best (p: 'a -> bool) (t: 'a tree) (fail : unit -> 'r) (succeed: 'a -> 'r) : 'r =
  match t with
  | Empty -> fail ()
  | Node (l,x,r) when p x -> succeed x
  | Node (l,x,r) -> find_best p l (fun () -> find_best p r fail succeed) succeed

(*
  This last line is a little loaded so let's break it down. If we reach the last branch, then we are 
  at a node containing some x that failed to satisfy p. In this case, we call find_best on the left 
  child of the current node. If doing so should fail, we would get a () which would call find_best on the 
  right subtree. If this should also fail, we call the fail continuation. Otherwise, we succeed with the 
  value of x satisfying p
*)

let t = Node (
  Node (Empty, 3, Empty),
  5,
  Node (
    Node (Empty, 6, Empty),
    9,
    Node (Empty, 4, Empty)
  )
)

(* with this tree, we can call the function by doing something like *)

(* find_best (fun x -> x mod 2 == 0) t (fun () -> "oh no") (fun x -> "found the number " ^ (string_of_int x));; *)

(* 
  We're actually already used to this sort of thing when coming from languages like Python and J*v%,
  which have exceptions!
  In these languages, any function can either return normally, or it can fail by throwing an exception. OCaml also has 
  exceptions. Let's see how to rewrite the above example using exceptions instead
*)

exception NotFound 
(*
   ^ Defines a new exception.
   In OCaml, exceptions are the constructors of the special built-in type 'exn'. Normally, we define all the constructors 
   of a type upfront, but exn, being special, allows us to add new constructors to it later, such as now
*)

let rec find3 (p: 'a -> bool) (t: 'a tree) : 'a = 
  match t with 
  | Empty -> raise NotFound (* raise: exn -> 'a *)
  | Node (l,x,r) when p x -> x
  | Node (l,x,r) ->
    try find3 p l with  (* In try .... with, if the ... throws, then we pattern match on the exception *)
      | NotFound -> find3 p r 

type coin = int 

let canada : coin list = [200; 100; 25; 10; 5]
let usa : coin list = [25; 10; 5; 1]

exception Change

let rec change (cs: coin list) (amt: int) : coin list =
  match cs with 
  | _ when amt <= 0 -> []
  | [] -> raise Change 
  | c :: cs -> 
    try c :: change (c :: cs) (amt - c) with 
    | Change -> change cs amt
