(* References and OOP in OCaml *)

(*
   Recap on Effects:

   * exceptions are an effect. E.G:

   let head (x :: _) = x 
   
   raises Match_failure if given an empty list

   * Nontermination is kind of an effect as well 

   let rec oops x = oops x 

   is an infinite loop

   * Key idea of effects: They are not part of the type of a function. No way of knowing whether a function raises an exception
     Best we can do is make an educated guess based on the type
  
   head has type 'a list -> 'a; IF IT RETURNS, it produces a list element
   oops has type 'a -> 'b: IF IT RETURNS, it gives you anything you want ( but it neve will >:) )

   Today's effect: Mutable state
   OCaml does have variables like you know them from Java or Python, i.e. mutable varibles. We call these "references" in OCaml

   > Create a reference : 
       let v = ref 0 in ...
       The function ref creates a referece with an initial value 
   > Read the value of a reference:
       let x = !v in ...
       The operator ! (read: bang) dereferences a ref (like * in C)
   > Write the value of a reference:
       v := e 
       The operator := (read: gets) assigns to a ref 
*)

(* IMPERATIVE PROGRAMMING *)

(* 
   Did you know OCaml has for and while loops?
*)

let fact n = 
  let v = ref 1 in 
  for i = 1 to n do 
    v := !v * i 
  done; (* notice the semicolon: This is the sequence operator*)
  !v

(*lol, we'll never use this again*)

(*
   Pros: Could be more performant in some cases: no need to worry about tail-recursion
   Cons: Harder to reason about the code. Can't use induction.
*)

(* Static semantics (type-checking) of references *)
(*
   A new type: "A ref" means "reference with contents of type A"
   Allocation: If e : A, then (ref e) : (A ref)
   Dereference: If v : A ref, then !v : A
   Update: If v : A ref, and a : A, then (v := a) : unit
  
> Updating returns a dummy value of type unit (since we don't care about the return value. We just want the reference to change)
> We use := for its effect of updating the reference
> In general, when a function returns unit, it is because it (most likely) performs effects
*)

(* Sequence operator ";" *)
(*
   > If e1 : unit and e2 : A, then (e1 ; e2) : A
   > It's the same as doing let _ = e1 in e2 
   > We evaluate e1, ignore its result (we only care about its effects), and evaluate e2

   In the factorial example, the for loop is like a function that returns unit. We don't care about unit. We want to update v 
   to compute the factorial, so we throw away unit and compute !v * i  
*)

let v = ref 0

(*
val fact : int -> int = <fun>
val v : int ref = {contents = 0}
utop # !v;;
- : int = 0
utop # v := 5;;
- : unit = ()
utop # !v;;
- : int = 5
*)

let vlist = ref [0;1]

(*
val vlist : int list ref = {contents = [0; 1]}
utop # vlist := [ false ];;
Error: This expression has type bool but an expression was expected of type int
*)

(*
Earlier we had seen that if I defined:
let v = 1;;
let add_v y = v + y;;
add_v 9;;
- : int = 10
let v = 5;;
add_v 9;;
- : int = 10
And so shadowing the value of v did not change it's definition in the function add_v
Now consider doing the same with references:

utop # let v = ref 1;;
val v : int ref = {contents = 1}
utop # let add_v y = !v + y;;
val add_v : int -> int = <fun>
utop # add_v 9;;
- : int = 10
utop # v := 5;;
- : unit = ()
utop # add_v 9;;
- : int = 14
*)

(* recall our really slow fibonacci function *)
let rec fib (n: int) : int = 
  if n = 0 then 0 
  else if n = 1 then 1 
  else fib (n-2) + fib (n-1)

let fib40 () = fib 40 (* every time we call this it computes fib40 slowly *)

let v = ref 1;;
let add_v y = !v + y

type 'a susp = Susp of (unit -> 'a)

let mk_susp (f: unit -> 'a) : 'a susp = 
  let cache = ref None in 
  let go () = match !cache with 
    | Some v -> v 
    | None -> let v = f () in 
        cache := Some v;
  in
  Susp go

let force (Susp f) = f ()


(*let fib40_susp = mk_susp (fun () -> fib 40)*)

(* this doesn't compile for some reason*)

(* lets take a step back to understand how this worked *)

let tick = 
  let c = ref 0 in 
  fun () -> c := !c + 1; !c

(*
utop # tick;;
- : unit -> int = <fun>
utop # tick () ;;
- : int = 1
utop # tick ();;
- : int = 2
*)

(* 
   tick is kind of acting like an object with a singular private field c 
   though if we wanted to make a tick object, we would want to be able to instantiate it with 
   different values of c, so that it could tick differently.
   We can accomplish this by changing this variable into a function that makes tick objects
*)

let mk_tick () = 
  let c = ref 0 in 
  fun () -> c := !c + 1; !c

(*
utop # mk_tick () ();;
- : int = 1
utop # mk_tick () ();;
- : int = 1 We just made a new one!

We can now have separate counters!

utop # let tick1 = mk_tick ();;
utop # let tick2 = mk_tick ();;
utop # tick1 ();;
- : int = 1
utop # tick1 ();;
- : int = 2
utop # tick1 ();;
- : int = 3
utop # tick2 ();;
- : int = 1
*)

(* 
   Going back to the fibonacci susps, we can see how the cache fields were not being shared between fib40 and some fib39 susp 
   They acted like "private fields" for the separate objects being created 
*)

(* 
   Okay, this is great and all, but we just learned how to make an object with a single method. How do we get more?
*)

let (tick, reset) = 
  let c = ref 0 in 
  let t () = c := !c + 1; !c in 
  let r () = c := 0 in
  (t,r)

(* 
Now we've created a single object having two methods. One that ticks, and one that resets the tick. They each have access to the private field c 
utop # tick ();;
- : int = 1
utop # tick ();;
- : int = 2
utop # reset ();;
- : unit = ()
utop # tick ();;
- : int = 1
*)

(* How do we do the same thing we did before to convert all this into a function that makes (tick,reset) pairs? *)

type counter = {
  tick : unit -> int; 
  reset : unit -> unit
}

let mk_tick_reset () = 
  let c = ref 0 in 
  let t () = c := !c + 1; !c in 
  let r () = c := 0 in 
  { tick = t; reset = r}

(*
utop # let tr = mk_tick_reset ();;
val tr : counter = {tick = <fun>; reset = <fun>}
utop # tr.tick();;
- : int = 1
utop # tr.tick();;
- : int = 2
utop # tr.reset();;
- : unit = ()
Looks familiar?
*)

(* We've been seeing c as a private field. This begs the question. Can we create public fields?*)

type custom_start_counter = {
  c : int ref;
  tick : unit -> int;
  reset : unit -> unit;
}

let mk_custom_tick_reset () = 
  let c = ref 0 in 
  let t () = c := !c + 1; !c in 
  let r () = c := 0 in 
  {c; tick = t; reset = r}

(*
utop # let tr = mk_custom_tick_reset ();;
val tr : custom_start_counter =
  {c = {contents = 0}; tick = <fun>; reset = <fun>}
utop # tr.c := 10;;
- : unit = ()
utop # tr.tick ();;
- : int = 11
*)

let use_counter {tick; reset; _} = 
  tick(), tick()

(* 
use_counter tr;;
- : int * int = (2, 1)
whoa, OCaml just evaluated the ticks from left to right. How shitty
*)
