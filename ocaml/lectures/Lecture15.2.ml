(* Bound variables vs free variables *)
(*
- Bound: Seen together with its binder
  let x = 2 in x, for example 

  Substitution rule:
   e1 !! v1     [v1/x]e2 !! v
  ----------------------------
     let x = e1 in e2 !! v
  
  We have to be careful though. Say we have, for example:

  let x = 1 in let x = 2 in x

  We should only substitute values into free variables. The final x in this example is bound to 2, 
  meaning we should not end up with something like 

  let x = 2 in 1

  As this means we change the meaning of our program if we simply had different variable names.

  1. Prevent substitutions from elimination bound variables:
     [e/x](let y = e1 in e2)
        if x = y then 
          let y = [e/x] e1 in e2 
        Here, we don't substitute anything in e2 because, as per our example, and x that occurs in e2 is already bound!

  Now consider the following problem:

  [y/x](let y = 3 in x + y)   --- and y != x
  let y = [y/x]3 in [y/x](x + y)
  let y = 3 in [y/x]x in [y/x]y
  let y = 3 in y + y 
  let y = 6

  This is a problem because once again if we had renamed one of our variables, we would have:

  [y/x](let z = 3 in x + z)   --- and y != x
  let z = 3 in [y/x]x + z
  let z = 3 in y + z
  let z = 3 in y + 3

  Clearly these are not the same! This is an issue. 


  (* Now let's implement functions in our new frogramming language *)
*)
type name = string 

type exp = 
  | Int of int 
  | Bool of bool 
  | If of exp * exp * exp
  | Op of exp * op * exp 
  | Var of name 
  | Let of dec * exp 
  | Fun of name * exp   (* <- BRAND NEW ! *)
  | App of exp * exp (* <- BRAND NEW 2 ! *)
and dec = 
  | Dec of name * exp 
and op = Plus | Minus | Times | Lt | Equals 

(* we're actually defining functions as they are in OCaml. OCaml functions only take a single input, but may return, 
   as output, another function. This is consistent with our fun being of name * exp, as the exp may be another function*)

(* Now, at first it may not seem obvious, but functions are more than just expressions, they are also values. 
   If I write (fun x -> x), nothing is evaluated in this. This is the same behaviour as simply writing "3". 
   In that sense, a function is a value just like integers or booleans
*)

type value = 
  | Vint of int 
  | Vbool of bool 
  |Vfun of name * exp 

(* Now for all of these we need to define some machinery to obtain the set of all free variables *)

type fvs = name list 
let union (fvs1 : fvs) (fvs2 : fvs) = fvs1 @ fvs2 
let rm_fv (x : name) (fvs : fvs) = 
  let p y = y <> x in 
  List.filter p fvs 

(* now let's define a function that actually returns the set of free variables in some expression *)
let rec fv (e : exp) : fvs = 
  match e with 
  | Int _ -> [] 
  | Bool _ -> [] 
  | Var x -> [x]
  | If (ec, et, ef) -> union (fv ec) (union (fv et) (fv ef))
  | Op (e1, _, e2) -> union (fv e1) (fv e2) 
  | Let (Dec (x,e), b) -> union (fv e) (rm_fv x (fv b))
  | Fun (x,e) -> rm_fv x (fv e)
  | App (e1, e2) -> union (fv e1) (fv e2) 

(* now, to perform substitutions we'll sometimes need to rename some variables. Here's a function that does this *)

let new_name_for (old : name) (e1 : exp) (b : exp) = 
  let names_to_avoid = union (fv e1) (fv b) in
  let safe n = not (List.mem n names_to_avoid) in 
  let rec generate_safe_name n = 
    let new_name = old ^ string_of_int n in 
    if safe new_name then new_name 
    else generate_safe_name (n + 1)
  in generate_safe_name 1

(* This effectively appends a new integer to the name until the name is not present in the free variables of e1 or b *)

(* Now we can define a substitution function! *)

let rec subst (e1 : exp) (x : name) (e2 : exp) = 
  match e2 with 
  | Int n -> Int n 
  | Bool b -> Bool b 
  | Var y when x = y -> e1 
  | Var y -> Var y 
  | Op (e2, op, e3) -> Op (subst e1 x e2, op, subst e1 x e3) 
  | If (ec, et, ef) -> 
    If (
    subst e1 x ec,
    subst e1 x et,
    subst e1 x ef
       )
  | Let (Dec (y, e), b) when x = y -> 
    Let (Dec (y, subst e1 x e), b) (* don't touch b! All instances of x in b are already bound! *)
  | Let (Dec (y, e), b) when List.mem y (fv e1) -> 
    let z = new_name_for y e1 b in (* rename y to avoid accidental capture *)
    let renamed_b = subst (Var z) y b in (* perform renaming *)
    Let (Dec (z, subst e1 x e), subst e1 x renamed_b) 
  | Let (Dec (y, e), b) -> Let (Dec (y, subst e1 x e), subst e1 x b)
  | Fun (y, e) when x = y -> Fun (y, e) 
  | Fun (y, e) when List.mem y (fv e1) -> 
    let z = new_name_for y e1 e in 
    let renamed_e = subst (Var z) y e in 
    Fun (y, subst e1 x renamed_e)
  | Fun (y, e) -> Fun (y, subst e1 x e) 
  | App (ea1, ea2) -> App (subst e1 x ea1, subst e1 x ea2)

(* In our language we may also like to be able to get expressions from values, just to make things easier to work with *)

let exp_of_value (v : value) : exp = 
  match v with 
  | Vint n -> Int n
  | Vbool b -> Bool b
  | Vfun (x,e) -> Fun (x,e) 

(* next step is to implement a way to evaluate expressions! This is the process of turning an expression into 
   a value. A couple design decisions can be made here, and one of the decisions I chose to make is to raise some 
   errors when some undefined behaviour occurs *)

exception TypeError of string

let rec do_op op v1 v2 = raise (TypeError "I don't feel like implementing the operation function")

let rec eval (e : exp) : value = 
  begin match e with 
  | Int n -> Vint n 
  | Bool b -> Vbool b 
  | If (ec, et, ef) -> 
    begin match eval ec with 
    | Vbool true -> eval et 
    | Vbool false -> eval ef 
    | Vint x -> if x <> 0 then eval et else eval ef 
    | Vfun (x, e) -> raise (TypeError "Function cannot be passed as conditional to if-statement")
    end 
  | Op (e1, op, e2) -> 
    let v1 = eval e1 in 
    let v2 = eval e2 in 
    do_op op v1 v2
  | Var x -> raise (TypeError ("Unbound variable: " ^ x))
  | Let (Dec (x,e), b) -> 
    let v1 = eval e in 
    let b' = subst (exp_of_value v1) x b in 
    eval b' 
  | Fun (x,e) -> Vfun (x,e) 
  | App (e1,e2) -> 
    begin match eval e1 with 
    | Vfun (x,e) -> 
      let v = eval e2 in 
      eval (subst (exp_of_value v) x e) 
    | _ -> raise (TypeError "Only functions can be applied")
    end
  end