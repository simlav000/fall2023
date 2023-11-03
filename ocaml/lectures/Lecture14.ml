(* Fundamentals of Programming Language Theory *)

(*
  What are the suntactically legal expressions? (Grammar)
  > What expressions does the parser accept?

  What are well-typed expressions? (Static semantics)
  > What expressions does the type-checker accept?

  How is an expression executed? (Dynamic semantics)
*)

(*
  Definition: Syntactically legal expressions
  The set of expressions is defined incutively by the following clauses:
    * A number n is an expression
    * The booleans true and false are expressions
    * If e1 and e2 are expressions, then e1 op e2 is an expression
      (where op in {+,=,-,*,<,>,...})
    * if e1 and e2 are expressions, then 
      if e then e1 else e2 is an expression

  Alternative: Backus-Naur From (BNF):
    * Operations op := +|-|*|<|=
    * Expressions e := n | e1 op e2 | true | false | if e then e1 else e2
*)

(* how do we encode expressions *)

type op = Plus | Minus | Times | Lt | Eq

type exp
(* OCaml doesn't like when our type exp doesn't have a constructor, so we can't say something like "type exp = int. That's a type synonym"*)
 = Int of int 
 | Bool of bool 
 | If of exp (* condition *) * exp (* then-branch *) * exp (* else-branch *)
 | Op of exp * op * exp

(* How might we represent something like: "if 0 < 3 then 4 else 5"*)
let exp1 = If (Op(Int 0, Lt, Int 3), Int 4, Int 5)

let parse (input : string) : exp option = failwith "Exercise"

(* How to evaluate an expression? *)
(*
  A better question... How do descripe evlauation of expressions. 
  We want to say:
    "Expression e evaluates to a value v"
  Hold on - what's a value?
    Values v := n | true | false 
*)

type value = 
  | VInt of int 
  | VBool of bool

(* 
  Expression e evaluates to value v
  Definition:
    Evaluation of the expression to a value v is defined inductively by the following clauses:
      * A value v evaluates to itself
      * If expression e evaluates to the value true 
        and expressions e1 evaluates to the value v
        then if e then e1 else e2 evaluates to the value v
      * If expressions e evaluates to the value false
        and expression e2 evaluates to a value v
        then if e then e1 else e2 evaluates to the value v

  Pretty verbose! Let's come up with some more compact notation for this
  Let's write:
      e !! v to mean "Expressions e evaluates to a value v"
  Definition:
    e !! v is defined inductively by the following clauses
    * v !! v
    * If expressions e !! true and e1 !! v then: if e then e1 else e2 !! v
    * If expression e !! false and e2 !! v then: if e then e1 else e2 !! v
  
  Step 2: Make it concise
  premise_1 ... premise_n
  -----------------------
        conclusion
  Read as: If premise_1 and premise_2 and .... and premise_n then conclusion

  Definition:
               e !! true        e1 !! v     e !! true       e2 !! v
  ----------  --------------------------   -------------------------
    v !! v    if e then e1 else e2 !! v    if e then e1 else e2 !! v

  Extending it to Operators
  Definition: e !! v is defined inductively by the following clauses:
  * ...                                      
  * if e1 !! v1 and e2 !! v2 then e1 op e2 !! (v1 op v2)
  Written in our stupid notation:

   e1 !! v1       e2 !! v2
  ------------------------
   e1 op e2 !! (v1 op v2)
*)

(* We can no implement these rules *)
exception RunTimeException of string 

let do_op (op : op) (v1 : value) (v2 : value) = 
  match op, v1, v2 with 
  | Plus, VInt x, VInt y -> VInt (x + y)
  | Plus, VBool x, VBool y -> VBool (x || y)
  | Minus, VInt x, VInt y -> VInt (x - y)
  | Times, VInt x, VInt y -> VInt (x * y)
  | Times, VBool x, VBool y -> VBool (x && y)
  | Lt, VInt x, VInt y -> VBool (x < y)
  | Eq, VInt x, VInt y -> VBool (x == y)
  | _ -> raise (RunTimeException "Don't do that")

let rec eval (e: exp) : value = 
  match e with 
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (cond, t, f) -> 
    begin match eval cond with 
     | VBool true -> eval t
     | VBool false -> eval f
     | VInt x -> if x <> 0 then eval t else eval f
    end
  | Op (e1, op, e2) -> 
    let v1 = eval e1 in 
    let v2 = eval e2 in 
    do_op op v1 v2

let exp2 = If (Int 0, Int 4, Int 5)

(* Advantages of a formal description *)
(*
   * Coverage: For all expressions e, there is an evaluation rule
   * Confluence: If e !! v1 and e !! v2, then v1 = v2
   * Value soundness: If e !! v then v is a value
   * Termination: If e is well-typed, then e !! v
   * Preservation: If e is well-typed and e !! v, then e and v have the same type
*)

(*
   All of the semantics we looked at today are dynamic semantics.
   We used the lends of "Big-Step Semantics". 
   Big-Step Semantics are not the only way to formally specify the behavior of code!
    * Big-Step semantics: what is the final result?
    * Small-Step semantics: what is the next step?
    * Denotatioinal semantics: What does the program mean? You figure out the rest
    * "Predicate Transformer Semantics": Useful for theorem provers
*)