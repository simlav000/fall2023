(* proofs part 2 *)

(* 
Let's try to prove some more interesting claims. Things from last week:
   
let rec (@) (l1: 'a list) (l2: 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | x :: xs -> x :: (xs @ l2)

let rec rev (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | x :: sx -> rev xs @ [x] 

Observation 1: [] @ l = l
Theorem 1: l @ [] = l

Theorem 2: l1 @ (l2 @ l3) = (l1 @ l2) @ l3 

Theorem 3: rev (l1 @ l2) = rev l2 @ rev l1

theorem 4: rev (rev l) = l

let rec fact (n: int) : int = 
  match n with 
  | 0 -> 1
  | _ -> n * fact( n - 1)

let rec fact_tr (n: int) (acc: int) : int =
  match n with 
  | 0 -> acc
  | _ -> fact_tr (n-1) (n * acc)

Theorem 5: fact n = fact tr n 1

Proof: Induction on n 

CASE: n = 0
  WTS: fact 0 = fact_tr 0 1 

  LHS = fact 0 
      = 1             - definition of fact
  RHS = fact_tr 0 1
      = 1             - definition of fact_tr
      = LHS

CASE: n != 0
  WTS: fact n = fact_tr 0 1
  IH: fact (n-1) = fact_tr (n-1) 1

  LHS = fact n 
      = n * fact (n-1)             - definition of fact

  RHS = fact_tr n 0 
      = fact_tr (n-1) (n * acc)    - definition of fact_tr
      STUCK ;O;

What we wanted on the RHS of the inductive hypothesis was fact_tr (n-1) (?acc)
What if we claim instead: fact n = fact_tr n acc
Unfortunately this isn't even true

Instead, let's claim:
  acc * fact n = fact_tr (n-1) acc

Lemma 5: acc * fact n = fact_tr n acc

Proof: 
Induction on n.

CASE: n = 0
  WTS: acc * fact 0 = fact_tr 0 acc

  LHS = acc * 1     - definition of fact 
      = acc         - x * 1 = x

  RHS = fact_tr 0 acc
      = acc         - definition of fact_tr 
      = LHS
      
CASE: n != 0 
  WTS: acc * fact n = fact_tr n acc 
  IH: for any acc, acc * fact (n-1) = fact_tr (n-1) acc

  LHS = acc * fact n
      = acc * (n * fact (n - 1))    - definition of fact
      = (acc * n ) * fact (n - 1)   - Multiplication is associative
      = fact_tr (n-1) (acc * n)

  RHS = fact_tr n acc
      = fact_tr (n-1) (n * acc)     - definition of fact
      = fact_tr (n-1) (acc * n)     - Multiplication is commutative 
      = LHS

Theorem 5: fact n = fact_tr n 1 
Proof: Lemma 5 with acc = 1
QED

let rec rev_tr (l: 'a list) (acc: 'a list) : 'a list = 
  match l with 
  | [] -> acc
  | x::xs -> rev_tr xs (x :: acc)

Goal: rev l = rev_tr l []

Lemma 6: rev l @ acc = rev_tr l acc
Proof: 
Induction on l:
CASE: l = []
  
  LHS = rev [] @ acc
      = [] @ acc           - by definition of function
      = acc                - By theorem 1: [] @ l = l 

  RHS = rev_tr [] acc
      = acc                - By definition of function
      = LHS

CASE: l = x::xs
  IH: for any acc, rev xs @ acc = rev_tr xs acc

  LHS = rev (x::xs) @ acc
      = (rev xs @ [x]) @ acc        - By definition of rev
      = rev xs @ ([x] @ acc)        - By theorem 2 (backwards!)
      = rev xs @ (x :: acc)         - Definition of @
      = rev_tr xs (x :: acc)        - IH with x::acc
      
  RHS = rev_tr (x::xs) acc
      = rev_tr xs (x :: acc)        - By definition of rev_tr
      = LHS

Theorem 6: rev l = rev_tr l []
Proof:
  Lemma 6 with acc = []

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with 
  | [] -> []
  | x::xs -> f x :: map f xs

let rec map_tr (f: 'a -> 'b) (l: 'a list) (return : 'b list -> 'r) : 'r = 
  math l with 
  | [] -> return []
  | x::xs -> map_tr f xs (fun ys -> return (f x :: ys))

Goal: map f l = map_tr f l (fun x -> x)

Lemma 7: return (map f l) = map_tr f l return
Proof: 
Induction on l
CASE: l = []
  WTS: return (map f []) = map_tr f [] return

  LHS = return (map f [])
      = return []              - defn. of map
  
  RHS = map_tr f [] return 
      = return []
      = LHS

CASE: x::xs
  WTS: return map (f x (x::xs)) = map_tr f (x::xs) return
  IH: for any (function) return, return (mat f xs) = map_tr f xs return

  LHS = return (map f (x::xs)) 
      = return (f x :: map f xs)     - defn. of map

  RHS = map_tr f (x::xs) return 
      = map_tr f xs (fun ys -> return (f x :: ys))    - defn. of map_tr
      = (fun ys -> return (f x :: ys)) (map f xs)     - <- IH (backwards!)
      = return (f x :: map f xs)                      - by function evaluation
      = LHS