(* CONTINUATIONS *)

(* let's talk performance *)

let rec append l1 l2 = match l1 with 
  | [] -> l2
  | x :: xs -> x :: append xs l2

(* 
   We had written this function a few lectures ago, and we didn't make it tail 
   recursive. This was because doing so is tricky. It is still valuable however.
   
   Leaving this as a regular recursive function implies that it takes up O(n) in 
   stack space, and has an O(n) time complexity, where n is the length of l1.
*)

(* This begs the question: Are there functions we can't write tail recursively? *)
(*** NO! ***)
(* It can always be done *)

let rec append_tr (l1: 'a list) (l2: 'a list) (return: 'a list -> 'r) : 'r = 
  match l1 with 
  | [] -> return l2
  | x :: xs ->
    append_tr xs l2 (fun r -> return (x :: r))

(* let's take a look at a trace of this function in action *)
(*
   consider this example (the base case)
   # append_tr [] [1;2] (fun x -> x)
   This matches with the first branch, meaning we call "return l2"
   return is defined as x -> x meaning it simply results in l2
   
   consider now a more complicated example 
   # append_tr [1;2] [3;4] (fun x -> x)

   1. Call append_tr [1;2] [3;4] (fun x -> x)
   2. match [1;2] with
   | [] -> return [3;4]   (since l1 is not empty, we go to the next case)
   | x :: xs ->
     Call append_tr [2] [3;4] (fun r -> return (1 :: r))  
     (Note: x = 1, xs = [2])

   3. match [2] with
      | [] -> return [3;4]   (l1 is not empty, we go to the next case)
      | x :: xs ->
        Call append_tr [] [3;4] (fun r -> return (2 :: r))
        (Note: x = 2, xs = [])

   4. match [] with
        | [] -> return [3;4]   (l1 is empty, so we return [3;4])

   5. Return [3;4] to the previous call (in step 3), so we get:
      return (2 :: [3;4])

   6. Return (2 :: [3;4]) to the previous call (in step 2), so we get:
      return (1 :: (2 :: [3;4]))

   7. Return (1 :: (2 :: [3;4])) to the initial call (in step 1), so the final result is:
      [1; 2; 3; 4]

    Another way of seeing it:
    (fun r' -> (fun r -> return (1 :: r)) (2 :: r')) ls      # so r' is ls and gets subbed into 2 :: r'
               (fun r -> return (1 :: r)) (2 :: ls)         # so (2 :: ls) is r and gets subbed into 1 :: r
                         return (1 :: (2 :: ls))
                         Done!
  *)

(*
   More generally: 
   * the return function is an example of a continuation
   * In general, a continuation is a representation of the execution state of a program at a certain point in time
   * Usualy, the execution state we want to represent and manipulate ourselves is the call stack
   * Specifically: what work is left to do? In other words; "what happens when we return?". Continuations
     let us manipulate what happens next directly

   * Some languages provide second-class continuations at least in some form: they provide constructs specifically for
     saving, manipulating, and restoring execution states.
       * Async programming using async/await (C#, JavaScript)
       * Coroutines and generators (JavaScript, Lua, Python)

   * Some language provide first-class continuations: you can save execution states, and manipulate them just like you 
     would any other function. Calling the function re-instates the saved execution state. 
       * Scheme and some other Lisps
       * sml/NJ (similar to OCaml)
    
   * Remark: perfomance
        * append will outperform append_tr on small lists. On (very) large lists, append will crash while append_tr will thrive.
        * Both append and append_tr use O(n) memory, but it's the TYPE of memory used that is different
            * append uses the STACK which is not very large (~8 MB)
            * append_tr uses HEAP which is plentiful        (~idk GB)
        * However, our continuations in the form of CLOSURES (functions capturing an environment) incurs an extra time and space penalty

   * Recap: how to convert to continuation-passing style
        1. change the type signature: add the continuation
           e.g. append: 'a list -> 'a list -> 'a list
           becomes:     'a list -> 'a list -> ('a list -> 'r) -> 'r
        2. Move all the work that should happen after the recursive call into the continuation
           e.g.:
           | x :: xs -> let ys = append xs l2 in x :: ys
           becomes:
           | x :: xs -> append_tr xs l2 (fun ys -> return (x :: ys))
           and returning becomes an explicit call to the continuation
   *)

(* ADVANCED CONTROL FLOW WITH CONTINUATIONS *)
(*
   Recap: dealing with failure:
    * You can use the option type to model computations which may fail, e.g. finding a value in a tree satisfying some predicate.
    find: ('a -> bool) -> 'a tree -> 'a option
    * Since there may be no such value satisfying the function 'a -> bool, we return a 'a option so that we can return None 
      to signify that the lookup has failed
   * We can use the general recipe to convert this function to continuation-passing style, but it turns out that we can do even better!
*)

(* DEMO *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

let rec find (p: 'a -> bool) (t: 'a tree) : 'a option =
  match t with 
  | Empty -> None
  | Node (l, x, r) -> 
      if p x then Some x
      else match find p l with 
        | Some x -> Some x
        | None -> find p r

(*
  consider this a rudimentary first-try for a find function. It is not tail recursive. The type signature of this function is:
  val find: ('a -> bool) -> 'a tree -> 'a option = <fun>
  We want to apply the recipe to turn the type signature into:
  val find: ('a -> bool) -> 'a tree -> ('a option -> 'r) -> 'r = <fun>
*)

let rec find_tr (p: 'a -> bool) (t: 'a tree) (return: 'a option -> 'r) : 'r =
  match t with 
  | Empty -> return None
  | Node (l, x, r) -> 
      if p x then return (Some x)
      else find_tr p l (fun result -> match result with
        | Some x -> return (Some x)
        | None -> find_tr p r return
      )

let example1 = Empty
let example2 = Node ( Node ( Empty, 
                             5, 
                             Node (Empty,
                                   42, 
                                   Node(Empty, 8, Empty))), 
                      2 , 
                      Empty)

(* the implementation of find_tr is somewhat worse than the original one. Here's an improvement we can make *)                    

let rec find_cps (p: 'a -> bool) (t: 'a tree) (fail: unit -> 'r) (succeed: 'a -> 'r): 'r =
  match t with 
  | Empty -> fail ()
  | Node (l, x, r) ->
    if p x then succeed x 
    else find_best p l (fun () -> find_best p r fail succeed) succeed
