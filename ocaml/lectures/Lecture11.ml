(* Induction Proofs *)

(* Prove all things; hold fast that which is true (Thessalonians 5:21)*)
(* Oay, we can't prove ALL things... Let's prove _some_ *)

type 'a list = 
  | []
  | (::) of 'a * 'a list 
(*
  That is:
  > [] (nil) is a 'a list
  > x :: xs is also a list (provided that x : 'a and xs : 'a list)
  This is an inductive definition! Lists are defined in terms of lists, but always building up in some sound way
  An inductive proof is like a reursive program using pattern matching
  Think of it like an algorithm.

  Base case: Prove the property for the case of []. Every recursive algorithm needs a base case!
  Step case: Prove the property for the case x :: xs. Assume the property holds for the sublist xs.
             This is the inductive hypothesis and corresponds to making a recursive call!
  Now for any given list we can "run" our proof on it to construct a specific proof for that list.
*)

let rec (@) (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with 
  | [] -> l2 
  | x :: xs -> x :: (xs @ l2)

(*
  Observation 1: [] @ l = l 
    It is clear from the definition of @ that this ought to be true
*)

(*
  Theorem 1: l @ [] = l
    This one is a bit trickier as we are not matching [], we match l.
  Proof. By induction on 'l'.

  Base case: l = []
    WTS: [] @ [] = []
    LHS = [] @ []
        = []                   - by definition of (@)
        = RHS
  
  Step case: l = x :: xs
  IH: xs @ [] = xs
  WTS: (x :: xs) @ [] = x :: xs
  LHS = (x :: xs) @ []
      = x :: (xs @ [])         - by definition of (@)
      = x :: (xs)              - by IH
      = x :: xs                - removing parentheses
      = RHS
  ▢
*)

(*
   Theorem 2: (Append is associative)
   for any l1, l2, l3 : 'a list, we have:
   l1 @ (l2 @ l3) = (l1 @ l2) @ l3

   Induction on l1:
   Base case: l1 = []
   WTS: [] @ (l2 @ l3) = ([] @ l2) @ l3

   LHS = [] @ (l2 @ l3)
       = l2 @ l3             - by definition of @ 
       
   RHS = ([] @ l2) @ l3
       = (l2) @ l3           - definition of @
       = l2 @ l3             - removing parentheses
       = LHS

  Step case: l1 = x :: xs 
  IH: xs @ (l2 @ l3) = (xs @ l2) @ l3
  WTS: (x :: xs) @ (l2 @ l3) = ((x :: xs) @ l2) @ l3

  LHS = (x :: xs) @ (l2 @ l3)
      =  x :: (xs @ (l2 @ l3))     - definition of @
      = x :: ((xs @ l2) @ l3)      - by IH
      = ( (x ::(xs @ l2)) @ l3)    - by definition of @ (backwards!)
      = ( (x :: xs) @ l2) @ l3     - by definition of @ (backwards!)
      = RHS
 ▢
*)

let rec rev (l : 'a list) : 'a list =
  match l with 
  | [] -> []
  | x :: xs -> rev xs @ [x]

(*
   Theorem 3: (Reverse anti-commutes with append)
   for any l1, l2 : 'a list, we have 
       rev (l1 @ l2) = rev l2 @ rev l1

   Proof: induction on l1

   Base case: l1 = []
   WTS: rev ([] @ l2) = rev l2 @ rev []

   LHS = rev ([] @ l2)
       = rev l2                      - by definition of (@)
       
   RHS = rev l2 @ rev []
       = rev l2 @ []                 - by definition of rev
       = rev l2                      - by theorem 1
       = LHS
    
   Step case: l1 = x :: xs
   IH: rev (xs @ l2) = rev l2 @ rev xs 
   WTS: rev ((x :: xs) @ l2) = rev l2 @ rev (x :: xs)

   LHS = rev ((x :: xs) @ l2)
       = rev (x :: (xs @ l2))        - by defn. of (@)
       = rev (xs @ l2) @ [x]         - by defn. of rev
       = (rev l2 @ rev xs) @ [x]     - by IH

   RHS = rev l2 @ rev (x :: xs)
       = rev l2 @ (rev xs @ [x])     - by defn. of rev
       = (rev l2 @ rev xs) @ [x]     - by theorem 2 
       = LHS
  ▢
*)

(* 
   Theorem 4: Reversing a list twice
   For any list l : 'a list, we have
   rev (rev l) = l

   Proof. by induction on l

   Base case: l = []
   WTS: rev (rev []) = []

   LHS = rev (rev []) 
       = rev []    - by defn. of rev
       = []        - by defn. of rev

   Step case: l = x :: xs
   WTS: rev (rev (x :: xs)) = x :: xs
   IH: rev (rev xs) = xs 

   LHS = rev (rev (x :: xs))
       = rev (rev xs @ [x])             - by defn. of rev
       = rev [x] @ rev (rev xs)         - by theorem 3
       = rev (x :: []) @ rev (rev xs)   - rewriting x (syntax sugar can be rewritten for free)
       = (rev [] @ [x]) @ rev (rev xs)  - by defn. of rev  
       = ([] @ [x]) @ rev (rev xs)      - by defn. of rev
       = [x] @ rev (rev xs)             - by defn. of (@)
       = [x] @ xs                       - by IH
       = (x :: []) @ xs                 - rewriting x
       = x :: ([] @ xs)                 - defn. of (@)
       = x :: xs                        - defn. of (@)
       = RHS
  ▢
*)