type 'a mylist = Nil | Cons of 'a * 'a mylist

let test_my_list = Cons ( 1, Cons (2, Cons (3, Nil)))
let test_list_1 = [1;2;3]
let test_list_2 = [4;5;6]

let rec list_of_mylist (l: 'a mylist) : 'a list =
    match l with 
    | Nil -> [] 
    | Cons (x,l') -> x :: (list_of_mylist l')

let rec mylist_of_list (l: 'a list) : 'a mylist = 
  match l with 
  | [] -> Nil 
  | x :: l' -> Cons (x, mylist_of_list l')

let rec append (l1: 'a mylist) (l2: 'a mylist) : 'a mylist =
  match l1 with 
  | Nil -> l2 
  | Cons (x, l1') -> Cons (x, append l1' l2)

let rec map (f: 'a -> 'b) (l: 'a mylist): 'b mylist =
  match l with 
  | Nil -> Nil 
  | Cons (e, l') -> Cons (f e, map f l')

let length (l: 'a mylist) : int = 
  let rec length' l acc = 
    match l with 
    | Nil -> acc 
    | Cons(x, l') -> length' l' (acc + 1)
  in length' l 0

(* fold_right : ('a -> 'b -> 'b) -> 'a mylist -> 'b -> 'b *)
  (* Mimics the List.fold_right functions.
     `fold_right f l e` eliminates a list into a type 'b of your
     choosing by replacing Nil with `e` and replacing Cons with `f`.

     Rank: *
   *)

let rec fold_right (f: 'a -> 'b -> 'b) (l: 'a mylist) (e: 'b) : 'b = 
  match l with
  | Nil -> e 
  | Cons(x,l') -> f x (fold_right f l' e)

(* fold_left : ('b -> 'a -> 'b) -> 'b -> 'a mylist -> 'b *)
  (* First, implement fold_left using recursion and pattern
     matching.

     Rank: *
   *)

let rec fold_left (f: 'a -> 'b -> 'b) (l: 'a mylist) (e: 'b) : 'b =
  match l with 
  | Nil -> e
  | Cons(x,l') -> fold_left f l' (f x e)

 (* Theoretically, any recursive function you can write for lists
     can be implemented with fold_right, so in particular, one should
     be able to implement fold_left in terms of fold_right.
   *)
  (* fold_left' : ('b -> 'a -> 'b) -> 'b -> 'a mylist -> 'b *)
  (* Implement fold_left' in terms of fold_right, without using
     recursion or pattern matching at all.
     Note: reversing the list doesn't help.

     Rank: ***
   *)

let fold_left_with_right (f: 'b -> 'a -> 'b) (e: 'b) (l: 'a mylist) : 'b =
  fold_right (fun a b -> f b a) (fold_right (fun a b -> append b (Cons(a, Nil))) l Nil) e

(* map' : ('a -> 'b) -> 'a mylist -> 'b mylist *)
  (* Implement map' using fold_right so that it does the same thing as
     map.

     Rank: **
   *)

let map_fold (f: 'a -> 'b) (l: 'a mylist) : 'b mylist =
  fold_right (fun x acc -> Cons(f x, acc)) l Nil
  (*              ^ This character here stands in for the elements of the list. 
     It doesn't seem clear that that is the case until we take a look at fold_right once more.
     let rec fold_right (f: 'a -> 'b -> 'b) (l: 'a mylist) (e: 'b) : 'b = 
      match l with
      | Nil -> e 
      | Cons(x,l') -> f x (fold_right f l' e)
     Notice that fold_right takes as input the anonymous function (fun x acc -> Cons(f x, acc))
     This means that "x" stands in for the elements of type 'a in the list l we feed fold_right.
  *)

(* Implement the function
     combine : 'a mylist -> 'b mylist -> ('a * 'b) mylist
     which pairs up the elements of the input lists.
     Assume that the input lists have the same length.

     Implement this as a recursive function.
     Rank: *
*)

let rec combine (l1: 'a mylist) (l2: 'b mylist) = 
    match l1, l2 with 
    | Nil, Nil -> Nil
    | Cons(x1, l1'), Cons(x2, l2') -> Cons((x1,x2), combine l1' l2')
    | _ -> failwith "Lengths mismatched"

 (* The OCaml List module defines
     List.map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list

     Implement
     map2 : ('a -> 'b -> 'c) -> 'a mylist -> 'b mylist -> 'c mylist
     using `combine` and `map`
     such that
     map2 f (Cons x1, (Cons x2, ...)) (Cons y1, (Cons y2, ...)) = (Cons f x1 y1, (Cons f x2 y2, ...))
     
     Assume the input lists have the same length (no need to check
     this).

     Rank: ***
   *)

let rec map2 (f: 'a -> 'b -> 'c) (l1: 'a mylist) (l2: 'b mylist) : 'c mylist =
  match l1, l2 with 
  | Nil, Nil -> Nil 
  | Cons (x1, l1'), Cons (x2, l2') -> Cons (f x1 x2, map2 f l1' l2')
  | _ -> failwith "Lengths mismatched"

let add x y = x + y
let list1 = mylist_of_list [1; 2; 3]
let list2 = mylist_of_list [10; 20; 30]
let result = map2 add list1 list2

(* It's easy to implement reverse using fold_left.
    rev : 'a mylist -> 'a mylist
    Reverses a list.

    Implement it using fold_left.
    Rank: **
  *)

(*
 applies f to all elements in a list and accumulates the result of said operation
 * think f = fun x acc -> x + acc, so acc eventually holds the sum of the elements
              item in your list
                       | accumulator
                       |     |   result        initial condition                  
                       v     v     v                  v     
let rec fold_left (f: 'a -> 'b -> 'b) (l: 'a mylist) (e: 'b) : 'b =
  match l with 
  | Nil -> e
  | Cons(x,l') -> fold_left f l' (f x e)
*)

let rec rev (l: 'a mylist) : 'a mylist =
  fold_left (fun x acc -> Cons(x, acc)) l Nil
(* 
                            [1; 2; 3]
              x:             1 :: [2; 3] 
                             v
    accumulator:             1 ::  []
              x:        1 :: 2 :: [3]
                             v
    accumulator:             2 ::  [1]
              x:   1 :: 2 :: 3
                             v
    accumulator:             3 ::  [2; 1]
    return:                 [3; 2; 1]
    utop # rev test_my_list;;

x: 1, acc: Nil
x: 2, acc: 1 :: []
x: 3, acc: 2 :: 1 :: []
- : int mylist = 3 :: 2 :: 1 :: []
*)



let rec show_mylist : 'a mylist -> string = function
  | Nil -> "Nil"
  | Cons(x, xs) -> "Cons(" ^ string_of_int x ^ ", " ^ show_mylist xs ^ ")"


(* for_all : ('a -> bool) -> 'a mylist -> bool *)
  (* `for_all p l` = true
     if and only if every element of `l` satisfies `p`.
     That is, for all `x` in `l`, we have `p x` is true.

     Hint: start by thinking about whether `for_all p Nil` should be
     true or false.

     Implement this function in terms of fold_right.

     Rank: **
   *)

let for_all (p: 'a -> bool) (l: 'a mylist) : bool =
  let rec for_all' p l (result: bool) = 
    match l, result with 
    | Nil, _ -> true 
    | Cons(x,l'),_ -> 
      if p x then for_all' p l' true 
      else false 
    in for_all' p l true

(* now let's do it in terms of fold_right *)
let for_all_fold (p: 'a -> bool) (l: 'a mylist) : bool =
  fold_right (fun x acc -> p x && acc) l true

(* exists : ('a -> bool) -> 'a mylist -> bool *)
(* `exists p l` is true
    if and only if
    there is some x in l such that p x is true.

    Hint: start by thinking about whether `exists p Nil` should be
    true or false.

    Rank: **
  *)

(*let exists (p: 'a -> bool) (l: 'a mylist) : bool = 
  let rec exists' p l result = 
    match l, result with 
    | Nil -> false 
    | (Cons (x, l'), _) -> *)

(*      
     Let's try to re-write append.
     Notice in that implementation that the second
     list is only needed at the very end.
     This suggests that we can use partial evaluation to construct a
     non-recursive function that just waits for the second list.

     Implement the function
     app_gen : 'a list -> 'a list -> 'a list
     such that
     app_gen l1 computes a non-recursive function f
     such that
     f l2 computes the concatenation of l1 and l2.

     Rank: *
*)

(* Let's talk about code generation / partial evaluation. *)

(* The canonical example: pow

    We can calculate n^k using a simple recursive program.
*)

let rec pow k n =
  if k = 0 then 1
  else pow (k-1) n * n

(* Rewrite this function so it performs partial evaluation.

    That is, implement the function
    pow_gen : int -> int -> int
    such that
    pow_gen k computes a NON-RECURSIVE function f
    such that
    f n computes n^k

    In other words, the function f that is returned must not contain
    a recursive call to pow_gen.
    By the way, this rules out the putative answer
    pow_gen k = pow k
    by just partially applying pow !

    Rank: *
*)

let rec pow_gen (k: int) : (int -> int) = 
  if k = 0 then fun _ -> 1
  else 
    fun n -> n * (pow_gen (k-1) n)
(*      ^ this n is the next argument we will be giving it later 
*)

(* 
  now so something similar with append.
  we can use partial evaluation to construct a
  non-recursive function that just waits for the second list.

  Implement the function
  app_gen : 'a list -> 'a list -> 'a list
  such that
  app_gen l1 computes a non-recursive function f
  such that
  f l2 computes the concatenation of l1 and l2.

  Rank: *
*)
(*
let rec append (l1: 'a mylist) (l2: 'a mylist) : 'a mylist =
  match l1 with 
  | Nil -> l2 
  | Cons (x, l1') -> Cons (x, append l1' l2)
*)

let rec app_gen (l1: 'a list) : ('a list -> 'a list) =
  match l1 with 
  | [] -> (fun l2 -> l2)
  | x :: l1' -> (fun l2 -> x :: (app_gen l1' l2))

(* Consider a type of binary tree. *)
type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

type 'a bst = (int * 'a) tree

(* In this problem, we consider a binary SEARCH tree, so the
    elements are ordered.
    That is, for any tree t = Node (l, (k, v), r)
    we have that every key k' in l is LESS THAN (or equal) k
    and every key k' in r in GREATER THAN (or equal) k.

    First, implement the function
    insert : 'a bst -> int -> 'a -> 'a bst
    such that
    insert t k v
    inserts the value v with the key k in the BST t.
    This should overwrite any previous value associated with k in the BST.

    Rank: *
  *)

let rec insert (tree:'a bst) (key: int) (value: 'a) :'a bst = 
  match tree with
  |Empty -> Node (Empty, (key, value), Empty)
  |Node (l,(k,v), r) -> 
      if k = key then (Node (l, (k, value),r)) else
      if k > key then (Node (insert l key value, (k,v), r)) else 
        (Node (l,(k,v), insert r key value)) 

let rec insert_tr tree key value return =
  match tree with 
  | Empty -> return (Node (Empty, (key, value), Empty))
  | Node (l, (k,v), r) ->
    if k = key then return (Node (l, (k, value) , r)) else
    if k > key then insert_tr l key value (fun leftResult -> return (Node (leftResult, (k,v), r)))
    else insert_tr r key value (fun rightResult -> return (Node (l, (k,v), rightResult)))

