
type 'a tree = Empty | Tree of 'a tree * 'a * 'a tree

let maxk (x : 'a) (y : 'a) (k : 'a -> 'r) : 'r =
  if x > y then k x else k y

let sum n =
  let rec sum' n k =
    match n with 
    | 0 -> k 0
    | n -> sum' (n-1) (fun r -> k (r + n))
  in sum' n (fun x -> x)

let rec tree_depth_cps (t : 'a tree) (return : int -> 'r) : 'r = 
match t with
| Empty -> return 0
| Tree (l, _, r) -> 
  tree_depth_cps l (fun leftDepth -> tree_depth_cps r (fun rightDepth -> maxk (leftDepth + 1) (rightDepth + 1) return))

let rec traverse_cps (t : 'a tree) (return : 'a list -> 'r) : 'r = 
  match t with
  | Empty -> return []
  | Tree (l,x,r) ->
      return (traverse_cps l (fun result -> x :: result @ traverse_cps r (fun result -> result)))

let test_tree = 
  Tree(
    Tree(Empty, "b", Empty),
    "a",
    Empty
)

let big_tree = 
  Tree(
      Tree(
        Tree(Empty, 2, Empty),
        7,
        Tree(
          Tree(Empty, 5, Empty),
          6,
          Tree(Empty, 11, Empty)
        )
      ),
      1,
      Tree(
        Empty,
        9,
        Tree(
          Tree(Empty, 5, Empty),
          9,
          Empty
        )
      ) 
    )

let rec tree_depth_cps (t : 'a tree) (return : int -> 'r) : 'r = 
match t with
| Empty -> return 0
| Tree (l, _, r) -> 
  tree_depth_cps l (fun leftDepth -> 
    tree_depth_cps r (fun rightDepth -> 
      maxk (leftDepth + 1) (rightDepth + 1) return))


let rec tree_max_cps (t : int tree) (return : int -> 'r) : 'r = 
  match t with 
  | Empty -> return (-1)
  | Tree (l,x,r) ->
      tree_max_cps l (fun leftResult -> 
          tree_max_cps r (fun rightResult -> 
              maxk (maxk x leftResult (fun x -> x)) rightResult return))

let rec find_subtree (ps : 'a list) (t : 'a tree) =
  match (t, ps) with 
  | (Empty, x :: ps) -> None (* If we have an empty tree but are listing some prefix, we can't return a subtree *)
  | (Empty, []) -> Some Empty (* Other sort of trivial case *)
  | (Tree (l,x,r), []) -> Some t
  | (Tree (l,x,r), n :: ps') -> 
    if n == x
    then
      match find_subtree ps' l with 
        | None -> find_subtree ps' r 
        | Some tree -> Some tree
    else 
      None

let rec find_subtree_cps (ps: 'a list) (t: 'a tree) (succeed : 'a tree -> 'r) (fail : unit -> 'r) : 'r =
  match (t, ps) with 
  | (Empty, x :: ps) -> fail ()
  | (Empty, []) -> succeed (Some Empty)
  | (Tree (l,x,r), []) -> succeed (Some t)
  | (Tree (l,x,r), n :: ps') -> 
    if n == x
    then
      match find_subtree ps' l with 
        | None -> find_subtree ps' r 
        | Some tree -> succeed (Some tree)
    else 
      fail ()

