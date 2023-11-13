(* BACKTRACKING *)

(*
  We have talked about three ways to write a backtracking algorithm
   1) Options
   2) CPS
   3) Exceptions
  Since I have a whole file on Exceptions, I'll focus on Options and CPS here, though I might 
  implement all three for a few problems.
  First, let's invent a problem to solve that may involve backtracking. These types of problems tend to be 
  the ones where trying every possibility is a sensible thing to do. They tend to entail us iterating over 
  some data structure, often times a list or a tree, trying something and recursing, and then doing back 
  and *not* trying it if trying it has failed. 

  Let's do this for the following problem:
  Given an input list and an integer n, return as output a sublist whose sum evaluates to n

  The idea for this is that we will *try* to include some element in the list, and recurse, trying over 
  and over until we either have gone over our capacity n, or there are no elements left. If we have failed 
  to reach out goal, we go back to the point where we *tried* adding the element, and we exclude it this time.
*)

(* With options *)

let make_sublist_optn (l : int list) (n : int) : int list option = 
  let rec go l n (acc : int list) =
  match l with 
  | _ when n = 0 -> Some acc (* this is the success condition *)
  | _ when n < 0 -> None     (* this is one failure condition (namely, we went over the capacity) *)
  | [] -> None               (* this is another failure condition (we ended under the capacity) *)
  | x :: l' -> 
    match go l' (n - x) (x :: acc) with 
    | None -> go l' n acc 
    | Some acc -> Some acc
  in go l n []

let make_sublist_optn_tr (l : int list) (n : int) : int list option = 
  let rec go l n success failure = 
    match l with
    |[] -> 
        if n = 0 then success (Some [])
        else failure ()
    |x::xs ->
        go xs (n-x) 
          (fun s -> let Some y = s in success (Some (x::y)))
          (fun () -> go xs n success failure)
  in go l n (fun x -> x) (fun () -> None)

(* Now let's do this with exceptions *)

exception Backtrack 

let make_sublist_exn l n = 
  let rec go l n acc = 
    match l with 
    | [] -> 
      if n = 0 then acc
      else raise Backtrack
    | x :: l' ->
      try go l' (n - x) (x :: acc) with 
      | Backtrack -> go l' n acc
    in go l n []

(* And we can make this version tail-recursive using CPS! *)

let make_sublist_exn_tr l n = 
  let rec go l n fail succ = 
    match l with 
    | [] -> 
      if n = 0 then succ [] 
      else fail () 
    | x :: l' -> 
      go l' (n - x) (fun result -> go l' n fail succ) (fun result -> succ (x :: result))
    in go l n (fun () -> raise Backtrack) (fun x -> x)