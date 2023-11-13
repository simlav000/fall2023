(* Modules *)

module type Comparable = sig
  type t 
  val compare : t -> t -> int
end
module Int : Comparable with type t = int = struct 
  type t = int
  let compare x y = x - y
end

module String : Comparable with type t = string = struct 
  type t = string
  let compare = compare
end

module Down (E : Comparable) : Comparable with type t = E.t = struct 
  type t = E.t 
  let compare x y = E.compare y x
end

module DownInt = Down(Int)
(*
  Can't compare functions using built-in compare function
  Can use #show in toplevel to show module definitions
*)

module IntSet = Set.Make(Int)

let remove_duplicates (xs: int list) : int list = 
  IntSet.elements (IntSet.of_list xs)

(* this is fast but returns the unique elements sorted *)

module IntMap = Map.Make(Int)
module StrSet = Set.Make(String)

(* MIDTERM REVIEW *)
(*
  1. Backtracking
  2. Exceptions
  3. Options
  4. CPS
  5. References (Encoding OOP)
  6. Induction

  Laziness:
  Streams and simple suspension stuff
*)

type 'a susp = Susp of (unit -> 'a )
let mk_susp (f : unit -> 'a) = Susp f


let rec fib n = 
  if n <= 1 then n
  else fib (n-1) + fib(n-2)


type 'a stream = 
{
  hd : 'a;
  tl: 'a stream susp
}

let force (Susp f) = f ()
(*
let rec zip_with (f: 'a -> 'b -> 'c) (xs : 'a stream) (ys : 'b stream) : 'c stream = 
{
  hd = ?;
  tl = Susp (fun () -> ?)
}
*)
(* write boilerplate like so. tl is always a susp*)
let rec zip_with (f: 'a -> 'b -> 'c) (xs : 'a stream) (ys : 'b stream) : 'c stream = 
{
  hd = f xs.hd ys.hd;
  tl = Susp (fun () -> zip_with f (force xs.tl) (force ys.tl))
}

let rec nats_from n = 
  {
    hd = n;
    tl = Susp (fun () -> nats_from (n + 1))
  }

let all_nats = nats_from 0
let weird_nats = nats_from 1

let odd_nats = zip_with (+) all_nats weird_nats

let rec take (n: int) (s: 'a stream) : 'a list = 
  if n <= 0 
    then []
  else s.hd :: take (n-1) (force s.tl)


exception Backtrack

type coint = int 
type coints = coint list 
type change = coints 

let make_change (cs : coints) (target: int) : change = 
  let rec go cs target = 
    match cs with 
    | _ when target = 0  -> []
    | _ when target < 0 -> raise Backtrack
    | [] -> raise Backtrack
    | c :: cs -> 
      try c :: (go (c :: cs) (target - c)) with 
      | Backtrack -> go cs target
    in go cs target

let make_change_2 (cs: coints) (target: int) : change = 
  let rec go cs target throw return = 
    match cs with 
    | _ when target = 0 -> return []
    | _ when target < 0 -> throw ()
    | [] -> throw ()
    | c :: cs -> 
      go (c :: cs) (target - c) (fun result -> go cs target throw return) (fun result -> return (c :: result))
    in go cs target (fun () -> raise Backtrack) (fun x -> x)
