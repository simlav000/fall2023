(*
  Let's extend our language with variables, let-expressions, and functions!
  We saw Backus-Naur form last time:

  Operations op :: = + | - | * | < | =
  Expressions e :: = n | e1 op e2 | true | false
                   | if e then e1 else e2
                   | x | let x = e1 in e2 end <-- new!

  Are the following well-formed expressions?
    * let x = x in x + 1 end
    * let x = let y = x in y + x end in end
    * let x = let x = 2 + 3 in x + 1 end in x end

    * yes 
    * no
    * yes

    Why?
    * in the first example, following the convention that a well-formed expression is one that follows the convention "let x = e1 in e2 end", 
      we can see that if we have e1 = x and e2 = x + 1 then we have the proper form
    * In the second, we can claim that e1 = let y = x in y + x end, but then we say "in end" meaning we are missing an e2. 
    * In the third, we can say that e1 = "let x = 2 + 3 in x + 1 end" which is in e2 = "x" end. This means the first let expression is valid.
      Then, the second let expression has let x = 2 + 3 in x + 1 end which is also a valid let expression meaning both are okay

  How should we evaluate "let x = e1 in e2 end"?
    * Evaluate e1 to a value V
    * Replace all references to x in e2 with V
    * Evaluate e2 normally

  Notation:
  The notation [e1/x]e2 means "replace all occurrences of x in e2 with e1." Read it as "substitute e1 for x in e2"

  Variable names should not matter.
  Another use for substitution is renaming variables
  We should be able to rename the variables in an expression without changing what the expression means 

  Example:
  These are all the same:
    * let x = 5 in (let y = x + 3 in y + y end) end 
    * let x = 5 in (let x = x + 3 in x + x end) end 
    * let v = 5 in (let w = v + 3 in w + w end) end

  If we rename x to y in:
    let x = 1 in x + x end
  We get:
    let y = 1 in [y/x] (x + x) end

  We can define substitution recursively!
  [e1/x]x = e1
  [e1/x]y = y
  [e1/x]n = n (n is an integer)
  [e1/x]b = b (b is bool)
  [e1/x](e2 op e3) = [e1/x]e2 op [e1/x]e3
  [e1/x](if e then e_t else e_f) = if [e1/x]e then [e1/x]e_t else [e1/x]e_f
  [e1/x](let y = ed in eb end) = let y = [e1/x]ed in [e1/x]eb end
*)

type op = Plus | Minus | Times | Lt | Eq
type name = string (* to represent variable names *)

type exp
 = Int of int 
 | Bool of bool 
 | If of exp (* condition *) * exp (* then-branch *) * exp (* else-branch *)
 | Op of exp * op * exp
 (* introducing our new constructs *)
 | Var of name 
 | Let of dec * exp
 and dec = 
  | Dec of name * exp


type value = 
  | VInt of int 
  | VBool of bool

(* how can we implement this recursive substitute function? *)

let rec subst (x : name) (e1 : exp) (e2 : exp) = 
  match e2 with 
  | Int n -> Int n 
  | Bool b -> Bool b 
  | Var y when y = x -> e1 
  | Var y -> Var y 
  | Op (e2, op, e3) -> Op (subst x e1 e2, op, subst x e1 e3)
  | Let (Dec (y, e), b) -> Let (Dec (y, subst x e1 e), subst x e1 b)
  | If (ec, et , ef) -> If (subst x e1 ec, subst x e1 et, subst x e1 ef)

  
let let_ex = Let (Dec ("x", Int 1), Op (Var "x", Plus, Var "x"))
(* encodes: let x = 1 in x + x end *)

let exp_of_value (v : value) : exp = 
  match v with 
  | VInt n -> Int n 
  | VBool b -> Bool b 

(* Evaluation revisited *)
(*
  e !! v  -> e evaluates to v 
  Evaluation rules:
  ------
  v !! v   B-Val

  e !! true        e_t !! v
  -------------------------
  if e then et else ef !! v     B-IfT

  e !! false       e_f !! v
  -------------------------
  if e then e2 else ef !! v    B-IfF

  ed !! v     [v1/x]e2 !! v 
  -------------------------
  let x = e1 in e2 end !! v    B-Let

  Notice there is no rule to evaluate variables!
*)

(* OH NO! What should happen if we try to evaluate the following expression?*)
(*
  let x = 1 in let x = 2 in x end end
  According to our rule, we get that this !! 1
*)

let oh_no = Let (Dec ("x", Int 1), 
              Let (Dec ("x", Int 2), 
                    Var "x"))

(*
  utop # eval oh_no;;
  - : value = VInt 1

  It's a disaster!
  let x = 1 in let y = 2 in x + y end end !! 3
  Wht if we try to rename the first variable to y as well?
    let y = 1 in [y/x](let y = 2 in x + y end) end
  = let y = 1 in let y = 2 in y + y end end !! 4
  Substitution should not change the meaning of our programs!
*)

(* Free and Bound variables *)
(*
  The solution is to talk about free and bound variables
  Definition:
  A reference to a variable x is bound in an expression e if it is within the scope of a definition for x in e
  
  Definition:
  A reference to a variable x is free in an expression e if it is not bound in e

  Example:
  * x    <- x is free!
  * let x = 1 in x end     <- x is bound!
  * let y = y' in y end    <- imagine here that y and y' are the same symbol. y is bound, but y' is free

  Computing free variables
  FV(e) is the set of free variable names occuring in e. 
  FV(x) = {x}
  FV(e1 op e2) = FV(e1) union FV(e2)
  FV(if e then et else ef0 = FV(e) union FV(et) union FV(ef)
  FV(let x = ed in eb end) = FV(ed) union (FV(eb)\{x})

  Let bindings:
  Any free references to x in eb become bound references to x in let x = ed in eb end.
  We say that the let binds or captures them. 

  In our "oh no!" example, we had:
    let x = 1 in let x' = 2 in x' end end (again imagine x and x' are the same symbol)
  x and x' are different variables though!
  When we substituted [1/x](let x = 2 in x end), we replaced an instance of x' with a value intended for x

  Solution: when applying substitutions to let-expressions, we should only substitute for free occurences of the variable!

  The other problem: "Accidental Capture"
  Accidental capture can occur because substitution can bring in any expression, not just values. In the "It's a disaster!" example, we had:
    let y = 1 in [y/x](let y = 2 in x + y end) end 
    let y = 1 in let y' = 2 in y' + y' end end
  the variable y was accidentally captured by the definition of y'

  Solution: to substitute [e1/x](let x = ed in eb end), first rename x to something new

  Definition:
  Substitution which renames bound variables is called capture-avoiding substitution
*)

(* Evaluation of Let (Revisited) *)
(*
  How should we evaluate " let x = e1 in e2 end"?
  * Evaluate e1 to a value V
  * Replace all FREE references to x in e2 with V
  * Evaluate e2 normally!

  Notation:
  The notation [e1/x]e2 means:
    " Replace all FREE occurences of x in e2 with e1, AVOIDING CAPTURE"
  Read it as "substitute e1 for x in e2"
*)

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
  | Var x -> raise (RunTimeException ("Unbound variable: " ^ x))
  | Let (Dec (x,e), b) -> 
    let v1 = eval e in 
    let b' = subst x (exp_of_value v1) b in 
    eval b' 

let exp2 = If (Int 0, Int 4, Int 5)