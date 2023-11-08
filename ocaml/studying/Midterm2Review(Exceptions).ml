(* Exceptions *)

(*
  OCaml, as any other flavor of the ML-language family is a safe language. This is ensured by stati type hecking and by 
  dynamic checks tat rule out violations that cannot be detected statically. Examples of run-time exceptions are:
  # 3 / 0;;
  Exception: Division_by_zero.
  Here the expression 3 / 0 will type check, but evaluation will incur a runtime fault that is signalled by raising the exception
  Division_by_zero. Tihs is an example where 3 / 0 has a type but not a value. It does have an effect too! Namely, to raise said exception. 
*)

let head (x::t) = x 

(*
  Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  []
  val head : 'a list -> 'a = <fun>
  utop # head [];;
  Exception: Match_failure ("Midterm2Review(Exceptions).ml", 12, 9).

  The function definition of head and the call head [] both type check. However, head [] does not have a value, but has an effect. 
  So far, we have considered built-in exceptions. Since they are pre-defined, they have a built-in meaning. However, we can also introduce
  and define our own exceptions to signal a specific program error.
*)

exception Domain 

let fact n = 
  let rec f n = 
    if n = 0 then 1
    else n * f (n - 1)
  in 
  if n < 0 then raise Domain 
  else f n

(* It's all about control *)

(*
  One of the most interesting uses of exceptions is for backtracking or more generally, controlling the exectution of behaviour. Backtracking 
  can be implemented by looking greedily for a solution, and if we get stuck, we undo the most recent greedy decision and try again 
  to find a solution from that point. There are many examples where solutions can be effectively found using backtracking. A classic example 
  is searching for an element in a tree. 
  
  Specifically, we want to implement a function "find t k" where t is a binary tree and k is a key. The function find has the type 
  ('a * 'b) tree -> 'a -> 'b, i.e. we store pairs of keys and data in the tree. Given the key k (the first component ('a)) we return the 
  corresponding data entry d, if the pair (k,d) exists in the tree. We make no assumptions about the tree being a binary search tree. Hence,
  when we try to look for the key, we may need to traverse the entire tree.
*)

exception Not_found

type key = int 

type 'a tree = 
  | Empty 
  | Node of 'a tree * (key * 'a) * 'a tree 

let rec find t k = 
  match t with 
  | Empty -> raise Not_found
  | Node (l, (k',v), r) -> 
    if k' = k then v 
    else try find l k with 
    | Not_found -> find r k

(* let's try to make this rail-recursive using CPS *)

let find_tr t k =
  let rec find_helper t k fail succeed = 
  match t with 
  | Empty -> fail ()
  | Node (l, (k',v), r) -> 
    if k' = k then succeed v 
    else find_helper l k (fun () -> find_helper r k fail succeed) succeed
  in find_helper t k (fun () -> raise Not_found) (fun x -> x)