(* Prove, using induction, the following theorems.

    THEOREM 1.
    For any (f : 'a -> 'b) and any (l : 'a list)
    length (map f l) = length l

    THEOREM 2.
    For any (f : 'a -> 'b) and any l1 and l2 of type 'a list
    append (map f l1) (map f l2) = map f (append l1 l2)

    Rank: *
*)
type 'a mylist = 
  | Nil
  | Cons of 'a * 'a mylist

(*
let rec map f l = match l with
| Nil -> Nil
| Cons(x,xs) -> let r = f x in Cons(r, (map f xs))

let rec length l =
   match l with
   | Nil -> 0
   | Cons(x,xs) -> 1 + length xs

*)

(*
  Theorem 1:
  Proof. By inductin on l
  Base case: l = Nil
  WTS: length (map f Nil) = length Nil
  
  LHS = length (map f Nil)
      = length Nil                  - by defn. of map 
     
  RHS = length Nil
      = LHS 

  Step case: l = Cons (x, xs)
  WTS: length (map f (Cons(x,xs))) = length (Cons(x,xs))
  IH: length (map f xs) = length xs

  RHS = length (map f (Cons(x,xs)))
      = length (Cons (f x, map f xs)         - by defn. of map
      = 1 + length (map f xs)                - by defn. of length
      = 1 + length xs                        - by IH

  LHS = length (Cons(x,xs))
      = 1 + length xs
      = RHS
  ▢
*)

let rec (@) (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1 with 
  | [] -> l2 
  | x :: xs -> x :: (xs @ l2)

(*
  THEOREM 2.
  For any (f : 'a -> 'b) and any l1 and l2 of type 'a list
  append (map f l1) (map f l2) = map f (append l1 l2)

  Proof. Induction on l1
  Base case: l1 = Nil
  WTS: append (map f Nil) (map f l2) = map f (append Nil l2)

  LHS = append (map f Nil) (map f l2)
      = append Nil (map f l2)                          - by defn. of map
      = map f l2                                       - by defn. of append
      
  RHS = map f (append Nil l2)
      = map f l2                                       - by defn. of append
      = LHS

  Step case: l1 = Cons(x,xs)
  WTS: append (map f Cons(x,xs)) (map f l2) = map f (append Cons(x,xs) l2)
  IH:  append (map f xs) (map f l2) = map f (append xs l2)

  LHS = append (map f Cons(x,xs)) (map f l2) 
      = append (Cons(f x, (map f xs))) (map f l2)       - by defn. of map
      = Cons(f x , append (map f xs) (map f l2))        - by defn. of append 
      = Cons(f x, map f (append xs l2))

  RHS = map f (append Cons(x,xs) l2)
      = map f (Cons(x, append xs l2))                   - by defn. of append 
      = Cons( f x, map f (append xs l2))                - by defn. of map 
      = LHS
  ▢
*)

let rec fold_right f l acc = match l with
  | Nil -> acc
  | Cons(x,xs) -> f x (fold_right f xs acc)

let for_all p l =
    fold_right (fun x acc -> acc && p x) l true

let exists p l =
    fold_right (fun x acc -> acc || p x) l false

let for_all' p l =
   not (exists (fun x -> not (p x)) l)

(*
  Prove, using induction, the following.

    THEOREM:
    for any (p : 'a -> bool) and any (l : 'a mylist),
    for_all p l = for_all' p l

  Proof. Induction on l
  Base case: l = Nil 
  WTS: for all p [] = for_all' p Nil 

  LHS = for_all p Nil
      = fold_right (fun x acc -> acc || p x) Nil true       - by defn. of for_all
      = true

  RHS = for_all' p Nil
      = not (exists (fun x -> not (p x)) Nil)
      = not (fold_right (fun x -> not (p x) Nil false) 
      = not (false)
      = true
      = LHS

  Step case: L = Cons(x,xs)
  WTS: for all p Cons(x,xs) = for_all' p Cons(x,xs)
  IH: for_all p xs = for_all' p xs 
                   = not (exists (not p) xs)

  LHS = for_all p Cons(x,xs)
      = fold_right (fun x acc -> acc && p x) Cons(x,xs) true
      = f x (fold_right f xs acc)
      = f x f x (fold_right f xs acc)

  



*)