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

  let rec insert t k v =
    match t with 
    | Empty -> Node (Empty, (k,v), Empty)
    | Node (l, (k', v'), r) -> 
      if k = k' then Node (l, (k, v), r)
      else if k < k' then Node (insert l k v, (k', v'), r)
      else Node (r, (k',v'), insert r k v)

(* 
  Idea: As always in OCaml, our types are immutable. This means that to insert an item in a tree, we need to 
  rebuild the tree with the new element in the correct spot. If we're given an empty tree, or we recurse all 
  the way down to a leaf node, it makes sense to return a node containing the desired (k,v) with no subtrees. 
  In the case where we recurse onto a Node containing the key we're looking for, in rebuilding our tree, we 
  swap the value out with our value, and keep the left and right subtrees as-is. 
  In the case where our key is more or less than the key of the Node we've recursed on, we further recurse 
  deeper on the correct subtree, making sure to rebuild the opposite side with the original subtree. 

  This implementation is not tail-recursive as the result of insert l k v or insert r k v are being awaited 
  to build the Node in the case where we recurse on the left or right subtrees. The recursive calls must be 
  evaluated first before the Node constructor can complete its task.
*)

(* Let's make it tail recursive using CPS *)

let insert_tr t k v =
  let rec insert_tr' t k v return = 
    match t with 
    | Empty -> return (Node (Empty, (k,v), Empty))
    | Node (l, (k',v'), r) -> 
      if k = k' then return (Node (l, (k,v), r))
      else if k < k' then insert_tr' l k v (fun result -> Node (result, (k',v'), r))
      else insert_tr' r k v (fun result -> Node (l, (k',v'), result))
    in insert_tr' t k v (fun x -> x)

(* testing *)

let my_tree = Node(Empty, (1,1), Node (Empty, (2,2), Empty))

(* there appears to be a problem *)

(* Lazy Programming *)

type 'a susp = Susp of (unit -> 'a)
let delay f = Susp f
let force (Susp f) = f ()


(* Usual definition of streams. *)
type 'a stream =
  { hd : 'a
  ; tl : 'a stream susp
  }

(** Implement the function
  nth : int -> 'a str -> 'a
  such that
  nth n s computes the nth element of the stream s.

  Rank: *
*)

let rec nth (n : int) (strm : 'a stream) : 'a = 
    if n = 0 then strm.hd 
    else nth (n - 1) (force strm.tl)

(** The fibonacci sequence is defined as
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    Rewriting the last equation by shifting n gives:
    fib (n+2) = fib (n+1) + fib n

    Construct the stream `fib` of all fibonacci numbers.

    Rank: **

    There is a "fancy" way to do this, like I tried in class, using
    higher-order functions on streams.

    HINT: There is also a more "pedestrian" way.
    Use a helper function to calculate, given fib n and fib (n+1),
    the fibonacci sequence starting from fib (n+2).
  *)

let fib = 
  let rec go n m = 
    {
      hd = n;
      tl = delay (fun () -> go m (n+m));
    }
  in go 0 1

(** In mathematics, a sequence is defined as a function from the
    natural numbers to some other set. That is, for each natural
    number `n`, there is an element of the sequence a_n.

    Define the function
    seq : (int -> 'a) -> 'a str
    such that given f : int -> 'a
    (which represents a mathematical sequence)
    seq f computes the stream
    f 0; f 1; f 2; ...

    Rank: *
  *)
  let seq f = 
    let rec go f n = 
      {
        hd = f n;
        tl = delay (fun () -> go f (n+1))
      }
    in go f 0

(** The Wallis product, published in 1656, is an infinite product
    whose limit is pi/2. It is among the oldest known ways to
    calculate pi to arbitrary precision.

    It is defined as:
    (2/1 * 2/3) * (4/3 * 4/5) * (6/5 * 6/7) * (8/7 * 8/9) * ...

    Notice that the nth factor in the product is given by the
    formula:

    a_n = 2n/(2n-1) * 2n/(2n+1)
        = 4n^2/(4n^2 - 1)
    (Remark: this sequence begins at n=1 !)

    Denote by W_n the Wallis product truncated at factor n.
    So W_1 = a_1 = 2/1 * 2/3
    W_2 = a_2 * W_1
    W_3 = a_3 * W_2 = a_3 * a_2 * a_1
    and so on.

    https://en.wikipedia.org/wiki/Wallis_product
  *)

(** Write a recursive function
    f : int -> float
    such that
    f n computes W_n.

    Then, use `seq f` to obtain the sequence of all approximations
    to the Wallis product.

    HINTS:
    - You will need to use float_of_int to make this work.
    - Since the wallis product starts at n=1, you will need to do
      some shifting to make it work with your `seq`, which begins
      with `f 0`.

    Rank: *
  *)

let wallis_1 =
  let rec f n =
    if n = 0 then 4.0 /. 3.0
    else
      let x = (float_of_int) (4*(n+1)*(n+1)) in 
      let y = (float_of_int) (4*(n+1)*(n+1) - 1) in 
      x /. y *. (f (n-1))
    in
  seq f 

(** This previous method is quite inefficient, since it recalculates
    the same things over and over again, through the recursive
    function `f`.

    Recall:
    a_n = 4n^2/(4n^2 - 1)
    and
    W_(n+1) = a_n * W_n

    Using this, directly construct the stream

    wallis_2 : float str

    which contains all successive approximations of the Wallis
    product.

    Rank: **
  *)

  let wallis_2 = 
    let rec go n w_n = 
      let x = (float_of_int) (4 * (n+1) * (n+1)) in 
      let y = (float_of_int) (4 * (n+1) * (n+1) - 1) in 
      let a_n = x /. y in 
      {
        hd = w_n;
        tl = delay (fun () -> go (n + 1) (a_n *. w_n));
      }
    in go 1 (4.0 /. 3.0)

(** The super-Catalan numbers are a two-dimensional generalization
    of the Catalan numbers.

    We have this closed form equation in terms of factorials:

              (2m)! (2n)!
    C(m, n) = ------------
              (m+n)! m! n!

    Implement the function
    superc : int -> int -> int
    such that
    superc m n = C(m, n)

    Implement factorial recursively as a helper.
    Recall
    0! = 1
    (n+1)! = n! * (n + 1)
  *)
  let superc m n = 
    let rec fact k = 
      if k = 0 then 1
      else k * fact (k - 1) in 
      let numerator = (fact (2 * m)) * (fact (2 * n)) in 
      let denominator = (fact (n + m)) * (fact n) * (fact m) in
     (numerator / denominator)
  
  (** An infinite two-dimensional grid of integers can be modelled
      with the type `int str str`.
      We can think of this as an infinite stream of infinite columns.

      Using superc, construct the infinite grid of super-Catalan
      numbers
      supercatalan : int str str
      such that
      nth m (nth n supercatalan) = superc m n = C(m, n)

      Hint: First solve the subproblem of calculating, for some fixed
      k, the stream
      column : int str
      such that
      nth m column = superc m k
      Then, generalize this to generate the sequence of all the columns.

      Rank: **
   *)

let supercatalan = 
  seq (fun n -> seq (fun m -> superc n m))
      