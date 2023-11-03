(* Induction on Trees

Binary Search Trees

*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

type 'a bst = (int, 'a tree)

let rec lookup (k : int) (t : 'a bst) : 'a option =
  match t with
  | Empty -> None
  | Node (l, (k',v), r) ->
    if      k < k' then lookup k l
    else if k > k' then lookup k r
    else                Some v

let rec insert ((k,v) : int * 'a) (t : 'a bst) : 'a bst =
  match t with
  | Empty -> Node (Empty, (k,v), Empty)
  | Node (l, (k',v'), r) ->
    if      k < k' then Node (insert (k,v) l, (k',v'), r)
    else if k > k' then Node (l, (k',v'), insert (k,v) r)
    else                Node (l, (k,v), r)

(*
   Theorem: You get back what you put in!
   if k : int, and v : 'a, and t : 'a bst, we have: 
    lookup k (insert (k, v) t) = Some v

   Proof: Induction on t

   CASE: t = Empty

   LHS = lookup k (insert (k, v) Empty)
       = lookup k (Node (Empty, (k,v), Empty))                    - defn. of insert
       = if k < k then ... else if k > k then ... else Some v     - defn. of lookup
       = Some v                                                   - evaluate else
       = RHS

   CASE: t = Node (l, (k',v'), r)
   where:
    l : 'a bst
    r: 'a bst
    k' : int
    v: 'a

    IH1: lookup k (insert (k, v) l) = Some v
    IH2: lookup k (insert (k, v) r) = Some v

    LHS = lookup k (insert (k, v) (Node (l, (k',v'), r)))
        SUBCASE 1: k < k'
          = lookup (Node ( insert (k, v) l, (k',v'), r))           
          = lookup k (k, v) (insert (k,v) l)                       - by insert since k < k'        
          = lookup k (insert (k,v) l)                              - by lookup since k < k'
          = Some v                                                 - by IH1
          = RHS

        SUBCASE 2: k > k'
          = lookup k (Node ( l, (k',v'), insert (k, v) r))           
          = lookup k (k, v) (insert (k,v) r)                       - by insert since k > k'        
          = lookup k (insert (k,v) r)                              - by lookup since k > k'
          = Some v                                                 - by IH2
          = RHS

        SUBCASE 3: k = k'
          = lookup k (Node l, (k,v), r)                            - by insert since k = k'
          = Some v                                                 - by lookup since k = k'
          = RHS

*)

(* Accumulators *)
type tree =
  | Leaf of int
  | Node of tree * tree

let rec sum t = match t with
  | Leaf n -> n
  | Node (l,r) -> sum l + sum r

let rec sum' t acc = match t with
  | Leaf n -> acc + n
  | Node (l,r) -> sum' l (sum' r acc)

(*
   Theorem: sum and sum' do the same thing

   if t : tree then sum t = sum' t 0
   
   Lemma: sum t + acc = sum' t acc

   Proof: 
   Induction on t.

   CASE: t = Leaf n 
   WTS: sum (Leaf n) + acc = sum' (Leaf n) acc
      LHS = sum (Leaf n) + acc 
          = n + acc                 - defn. of sum

      RHS = sum' (Leaf n) acc 
          = acc + n                 - defn. of sum'
          = n + acc                 - + commutes
          = LHS

   CASE: t = Node(l,r)
   IH1: for any acc, sum l + acc = sum' l acc
   IH2: for any acc, sum r + acc = sum' r acc
   WTS: sum (Node (l,r)) + acc = sum' (Node (l,r)) acc 
      LHS = sum (Node (l,r)) + acc 
          = (sum l + sum r) + acc        - defn. of sum

      RHS = sum' (Node (l,r)) acc 
          = sum' l (sum' r acc)          - defn. of sum'
          = sum l + (sum' r acc)         - by <- IH1 with acc := sum' r acc
          = sum l + (sum r + acc)        - by <- IH2 with acc := acc
          = (sum l + sum r) + acc        - + associates
          = LHS

  (reminder) Theorem: sum t = sum' t 0
  Proof: 
    Lemma with acc = 0.
  Done.
*)

