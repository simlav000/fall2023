(* We force the user to annotate the types of their functions *)
(*
  This means so far we have no polymorphism D:
  We also have no operators 

  Intuition:
  What are the types of the following OCaml expressions?
  1) 1 + 2

  Obviously this has type : int. We know because the (+) operator can only return ints as outputs 

  2) fun x -> x + 1 

  This also has type : int. Once again, we are taking some x and adding (+). Thus the return type is int, 
  and so is the type of x. 

  3) fun f -> f 0

  this one is a function that takes a function as input, and returns another function that takes an int (zero), but 
  we have no idea what f 0 will be, so we need more information 

  4) fun b -> if b then b else 0 

  here, b has to be a boolean since we can only have "if boolean then...". But we also have "then b", so we need 
  "else _" to be a bool, but _ is 0 which is an int. Thus we require that b is an int and a bool 

  5) fun f x -> if f x then x else 1 

  here f is being applied to x so it must be an arrow type, the then and else branches need to match the "1" so x is 
  an int, and we need f x to be a condition thus f x returns a bool
*)

(* How we think about types *)
(*
  1) Numbers and booleans have obvious types 
  2) Operands to (+) "must be" ints 
  3) Things that are applied "must have" function/arrow types 
  4) if f: a -> b, and we have f x, then x "must have" type a
  5) in "if e1 then e2 else e3", e1 "must have" type bool, and the types of e2 and e3 must match
*)

(* What we do differently *)
(*
  When we think about types, we (as humans) are comfortable leaving something as "unknown" and figuring it out 
  later. The inability to do that was where we got stuck with "fun" previously. 

  So can we make our algorithm figure them out later? Just like us?

  Of course we can!

  * How do deal with unknowns? Variables!
  * "Unification variables": types we haven't figured out yet. 
  * "Constraints": the "musts". Equalities between types. 
    * E.G. a = int or int -> a = b 
  * Typing rules previously: check that types match 
  * Typing rules now: Create constraint that types match!

  !*! Unification variables !*!
  Unification variables are a special kind of type variable. They stand for ONE unknown type. - Not polymorphism!
*)

(*
  New typing judgements:
  Constraint Sets:
    Constrains are of the form t1 = t2. We'll use phi to refer to sets of these. 
  * Need to encode constrains into our typing rules 
  * G |- e : t won't cut it anymore
  * G |- e : t ~> φ 
  Read G |- e : t ~> φ as "Gamma proves e has type t generating constraints φ"
  * Set up rules to keep track of generated constraints 

  -------------------T-int  -------------------T-bool 
   Γ ⊢ n : int ~> {}        Γ ⊢ b : bool ~> {}

      
      x : τ ∈ Γ
  -----------------T-Var
   Γ ⊢ x : τ ~> {}


   Γ ⊢ e1 : τ1 ~> φ1         Γ ⊢ e2 : τ2 ~> φ2        Γ ⊢ e3 : τ3 ~> φ3
  -----------------------------------------------------------------------T-If
   Γ ⊢ if e1 then e2 else e3 : τ ~> {τ1 = bool, τ2 = τ3} ∪ φ1 ∪ φ2 ∪ φ3


   Γ, x : α ⊢ e : τ1 ~> φ     α is a new Unification Variable
  ------------------------------------------------------------T-Fun
                 Γ ⊢ fun x => e : α -> τ ~> φ


   Γ ⊢ e1 : τ1 ~> φ1         Γ ⊢ e2 : τ2 ~> φ2  
  ----------------------------------------------T-App
    Γ ⊢ e1 e2 : α ~> {τ1 = τ2 -> α} ∪ φ1 ∪ φ2

  Conjuring up the new unification variable alpha allows us to get around forcing the user to type-annotate!

  * Our rules accumulae a big set of constraints
  * We never delete constraints. Implementation hint!
  * But we still don't have an actual type for our expression
  We have to solve our constraints!
*)

(* Unifiers *)
(*
  * Solutions to constraints: type for each Unification Variable
  * We can then substitute these types for the variables

  Example:
  Solve:
    * {int -> b = a, b = bool}
      > we're directly being told b has type bool
      > this implies that a has type signature int -> bool
      > Formal syntax: [int -> bool / a, bool / b]

    * {a -> int = bool -> b}
      > for both sides to be equal, we need a to have type bool, and b to have type int
      > Formally: [bool / a, int / b]

  These "mult-substitutions" for types are called unifiers

  How can we actually solve these constraint sets?
*)

(* Hindley-Milner Unification Algorithm *)
(*
  1) Pick an unsolved equality. Based on shape:
  2) τ = τ : throw it away (delete)
  3) τ = α : flip it around (orientation)
  4) τ1 -> τ2 = τ3 -> τ4 : add τ1 = τ3 and τ2 = τ4 to the constraint set instead (decompose)
  5) τ = α : This one is a bit more complex (eliminate)
    1) Apply the substitution [ τ / α ] to all other equations 
    2) Solve the rest of the equations
    3) Add [ τ / α ] to the solution

*)
