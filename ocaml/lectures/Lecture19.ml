(* Subtyping *)
(*
  Basic subtyping principle: S < T
  S is a subtype of T if we can provide an expression of type S whenever a value of type T is expected
    * This process is called upcasting 
    * Upcasting happens implicity during typechecking:

    Γ ⊢ e : S      S < T
   ----------------------T-SUB
          Γ ⊢ e : T

    * We can *imagine* the compiler is inserting calls to special casting functions up : S -> T 

  Relations for base types:
  1) int < float 
  2) positive < int 
  3) even < int

  Reflexive Transitive Closure:
                  S < T'  T' < T
  --------S-Ref  ----------------S-TRANS
    T < T             S < T
*)

(* How about tuples? *)
(*
  When is S1 * S2 a subtype of T1 * T2?
  Guess: If S1 < T1 and S2 < T2
  I'm so right 

   S1 < T1     S2 < T2
  ---------------------S-PROD
    S1 * T1 < S2 * T2

  Tuple types (aka product types) are co-variant 
  In other words: The subtyping *direction* is the same in the premises and the conclusion 
*)

(* How about functions? *)
(*
  Idea: S1 -> S2 is a subtype of T1 -> T2 
  if we can provide a value of type S1 -> S2
  whenever a value of type T1 -> T2 is required

  Example: Can we provide a function float -> int whenever a function float -> float is required?

  One way to think about solving this:
  HAVE f : float -> int 
  HAVE up: int -> float 

  NEED: ??? : float -> float 

  looking at it like this, we can obviously use function composition, so it is possible! Simply do:
  (fun x -> up (f x)) : float -> float

  Example 2: Can we provide a function float -> float whenever a function int -> float is required?

  HAVE f : float -> float 
  HAVE up: int -> float 

  NEED: ??? : int -> float
  maybe do (fun x -> f (up x)) : int -> float

  A tale of two variances:
   contravariant  covariant
     T1 < S1     S2 < T2
  --------------------------
      S1 -> S2 < T1 -> T2

  * We want to use a function S1 -> S2 where a function of type T1 -> T2 is expected 
  * We imagine the interpreter is inserting the appropriate (safe) upcasts implicitly at runtime
  * In other words, the interpreter is trying ti implemented a function of type T1 -> t2 given 
    a function f of type S1 -> S2
  
  If you were tasked with this, how would you do it?
  fun x -> ??? 
  where,
  x : T1, and you have access to f : S1 -> S2
  as well as the safe upcasts: 
  up1 : T1 -> S1 
  (from contravariance of input)
  up2 : S2 -> T2 
  (from covariance of output)

  x has tye T1, can use up1 to go from T1 -> S1, can use f to go from S1 -> S2, 
  finally can use up2 to get from S2 -> T2. So ultimately, 
  f x : T1 -> T2
  Putting it all together:
  fun x -> up2 (f (up1 x)) : T1 -> T2
*)

(* Taking stock *)
(*
  Basics:
  1) Built-in types, eg. int < float 
  2) Reflexivity: T < T 
  3) TransitivityL T < S and S < R then T < R 

  Products:
  1) Are covariant
  e.g: int * float < float * float 

  Functions:
  1) Are covariant IN THE OUTPUT TYPE
  e.g T -> int < T -> float
  2) Are contravariant IN THE INPUT TYPE
  e.g float -> T < int -> T
  3) Both phenomena can occur at once
  e.g float -> int < int -> float
*)

(* Subtyping for references *)
(*
  References can be both read and written, so we must consider both situations to arrive at a subtyping rule for the type T ref 
*)

let x = ref 5.0 in 
let y = ref (2 + 3) in 
 !x *. 3.14 

(* Is it safe to write !y * 3.14 instead of !x * 3.14 ? *)
(*
  Can we provide a value of type int ref when a value of float ref is required?
  In this case, this is okay. We are reading from both references, in the case of !x * 3.14, this is obviously okay. 
  In the case of !y * 3.14, since we consider int < float, this is also okay.
  We can take y and convert it into a float. This seems to suggest:

       S < T
  ---------------
   S ref < T ref
*)

let x = ref 5.0 in 
let y = ref (2 + 3) in 
  y := 4

(* Is it safe to write x := 4 instead of y := 4 ? *)
(*
   Here, it is reasonable to store an integer in a float reference cell. But this suggests the rule:

       S < T
  ---------------
   T ref < S ref
*)

(*
  A more formal argument
  Since references support both reading and writing, we can observe that:
  T ref is the same as (unit -> T) (for reading) * (T -> unit) (for writing)
  If we want S ref < T ref, that's the same as asking for 
  (unit -> S) * (S -> unit) < (unit -> T) * (T -> unit)
  Which, by covariance of tupes, we can split into:
  unit -> S < unit -> T  (for reading)
  S -> unit < T -> unit  (for writing)

  And this requires both:
   1) S < T (by covariance of function output)
   2) T < S (by contravariance of function input)

   Conclusion:
   References are INVARIANT
   The subtyping rule would have to be 
    S < T   T < S
   ---------------
    S ref < t ref
  Which in our setting requires S = T
*)

(* Practice problems *)
(*
  Base realtions:
  - int < float 
  - even < int 
  - pos < int

  1. even -> float < float -> float ?
     No, we lack the contravariance on input types. It is not the case that float < even

  2. int -> int < float -> float ?
     No, we again fail the contravariance check. It is not the case that float < int for the input types

  3. float * int ref -> int * int ref < int * float ref -> int * int ref ?
     While we do have the necessary contravariance in input types int < float, we do not have int ref = float ref and so this is not subtypable

  4. pos * int * float -> int < pos * int * float -> float ?
     This is correct. since both input types are identical, we only need to check for covariance, which is satisfied by int < float

  5. float * (float -> float) < float * (even -> float) ?
     Need to check that: float -> float < even -> float since in the tuple, obviously float < float 
     on input types, we have the necessary contravariance of even < float. then the outputs match so A-OK! 
     This is subtypable
*)

(* be careful with nested arrows int -> float -> int < blah blah blah*)