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


(*
let rec rev (l: 'a mylist) : 'a mylist =
  fold_left (fun x acc ->
    let result = Cons(x, acc) in
    (* Print the values of x and acc *)
    print_endline ("x: " ^ string_of_int x ^ ", acc: " ^ show_mylist acc);
    result
  ) l Nil
*)


