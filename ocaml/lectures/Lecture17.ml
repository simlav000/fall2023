(* Closures *)
(*
  
*)
type op = Plus | Minus | Times | Lt | Eq

type name = string        

type tp = TInt | TBool | TArrow of tp * tp 

type exp
  = Int of int
  | Bool of bool
  | If of exp (* condition *) * exp (* then-branch *) * exp (* else-branch *)
  | Var of name
  | Let of dec * exp 
  | Fn of name * tp * exp
  | App of exp * exp
and dec =
  | Dec of name * exp

module Env = Map.Make(String)



exception TypeError of string

(*
  e !! v   <- evaluation rules
  
  Context G := . | G, x : t

  G |- e : t    <- type rules (e has type t in the contenxt G) (the environment G proves that e has type t)

  ------------- T-Int   ---------------- T-Bool
  G |- n : int            G |- b : bool  
  
  
   G |-  e1 : bool  G |- e2 : t  G |- e3 : t
  ------------------------------------------- T-If
       G |- if e1 then e2 else e3 : t

   x : t in G 
  ------------ T-Var
   G |- x : t
  
   e1 : t'                  e2 : t
  ---------------------------------  T-Let
        let x = e1 in e2 : t

       G, x : t1 |- e : t
  --------------------------------- T-Fun
    G |- fun x => e : t1 -> t

   G |- e1 : t1 => t    G |- e2 : t'
  ----------------------------------- T-App
             G |- e1 e2 : t
*)

type ctx = tp Env.t

let rec infer (ctx : ctx) (e : exp) : tp =
  match e with 
  | Int _ -> TInt 
  | Bool _ -> TBool 
  | Var x -> 
    begin match Env.find_opt x ctx with 
    | None -> raise (TypeError "unbound variable!")
    | Some t -> t
    end
  | If (e1, e2, e3) ->
    let t1 = infer ctx e1 in 
    begin match t1 with 
    | TBool -> 
      let t = infer ctx e2 in 
      let t' = infer ctx e3 in 
      if t = t' then t 
      else raise (TypeError "Both arms of if statement must have the same type")
    | _ -> raise (TypeError "Condition of if must be a boolean")
    end
  | Let (Dec (x, e1), e2) -> 
    let t' = infer ctx e1 in 
    let ctx' = Env.add x t' ctx in 
    let t = infer ctx' e2 in 
    t 
  | Fn (x, t1, e) -> 
    let t = infer (Env.add x t1 ctx) e in 
    TArrow (t1, t)
  | App (e1, e2) -> 
    begin match infer ctx e1 with 
    | TArrow (t1, t) -> 
      let t' = infer ctx e2 in 
      if t = t' then t 
      else raise (TypeError "Application of function to wrong type of argument")
    | _ -> raise (TypeError "Can't apply non-function")
    end


type value
  = VInt of int
  | VBool of bool
  | VFun of name * exp * env 
and env = value Env.t


type free_vars = name list
let union (fvs1 : free_vars) (fvs2 : free_vars) = fvs1 @ fvs2
let delete (x : name) (fvs : free_vars) =
  let p y = y <> x in
  List.filter p fvs 
                    
(* Compute free variables, exactly as seen on Slide 12 *)
let rec fv (e : exp) : free_vars =
  match e with
  | Int _ | Bool _ -> []
  | Var x -> [x]
  | If (ec, et, ef)     -> union (fv ec) (union (fv et) (fv ef))
  | Op (e1, _, e2)      -> union (fv e1) (fv e2)
  | Let (Dec (x, e), b) -> union (fv e) (delete x (fv b)) 
  | Fn (x, e)           -> delete x (fv e) (* fn x => e binds x in e *)
  | App (e1, e2)        -> union (fv e1) (fv e2) (* recurse normally *)
                                                          
(* When we do renaming, we have to pick a "new" name - that is, one which
won't accidentally capture free variables of e1 or b. This function does
just that, by generating names until one is safe. *)
let new_name_for (old : name) (e1 : exp) (b : exp) =
  let names_to_avoid = union (fv e1) (fv b) in
  let safe n = not (List.mem n names_to_avoid) in
  let rec generate_safe_name n =
    let new_name = old ^ string_of_int n in
    if safe new_name then new_name 
    else generate_safe_name (n+1)
  in generate_safe_name 1

             (* subst e1 x e2 === [e1 / x] e2 *)
let rec subst (e1 : exp) (x : name) (e2 : exp) =
  match e2 with
  | Int n -> Int n
  | Bool b -> Bool b
  | Var y when x = y -> e1
  | Var y            -> Var y
  | If (ec, et, ef) -> If (
      subst e1 x ec,
      subst e1 x et,
      subst e1 x ef
    )
  | Let (Dec (y, e), b) when x = y -> 
      Let (Dec (y, subst e1 x e), b) (* Don't touch b - x wouldn't be free! *)
  | Let (Dec (y, e), b) when List.mem y (fv e1) ->
      let z = new_name_for y e1 b in (* rename y to avoid accidental capture *)
      let renamed_b = subst (Var z) y b in (* rename... *)
      Let (Dec (z, subst e1 x e), subst e1 x renamed_b) (* finish subst *)
  | Let (Dec (y, e), b) ->
      (* x would be free in b, and no risk of accidental capture...
         The plain recursive substitutions will do the right thing. *)
      Let (Dec (y, subst e1 x e), subst e1 x b)
  
   (* Just like Let, Fn is a binding construct. So all these dangerous
   edge cases that we saw with Let will apply to Fn as well. If we added any
   more binding constructs, we'd probably want to make a helper function.
   That's an exercise for you! *)
  | Fn (y, e) when x = y -> Fn (y, e) (* Don't touch b - x would not be free! *)
  | Fn (y, e) when List.mem y (fv e1) ->
      let z = new_name_for y e1 e in
      let renamed_e = subst (Var z) y e in
      Fn (z, subst e1 x renamed_e)
  | Fn (y, e) -> Fn (y, subst e1 x e)
  
  (* For applications, we can recurse normally. *)
  | App (e2, e3) -> App (subst e1 x e2, subst e1 x e3)
      
                             
let exp_of_value (v : value) : exp =
  match v with
  | VInt n -> Int n
  | VBool b -> Bool b 
  | VFun (x, e, env) -> Fn (x, e)
                      
(* let x = 1 in x + x end *)
(* Evaluating this now correctly gives VInt 2. Try it! *)
let let_ex = Let (Dec ("x", Int 1), Op (Var "x", Plus, Var "x"))

(* [y/x](let y = 2 in x + y end *) 
(* Check that we now get the right answer! *)
let disaster_ex = subst (Var "y") "x"
    (Let (Dec ("y", Int 2), Op (Var "x", Plus, Var "y")))
      
exception RuntimeException of string      
      
        (*
  e !! true    e1 !! v          e !! false    e2 !! v
--------------------------     --------------------------
if e then e1 else e2 !! v      if e then e1 else e2 !! v
*)

let rec eval (env : env) (e : exp) : value = match e with
  | Int n -> VInt n
  | Bool b -> VBool b
  | If (cond, t, f) -> begin match eval env cond with
      | VBool true  -> eval env t
      | VBool false -> eval env f
      | _ -> raise (RuntimeException "Condition of if must be a boolean")
    end 
  | Var x -> begin match Env.find_opt x env with
      | None -> raise (RuntimeException ("Unbound variable: " ^ x))
      | Some v -> v
    end
  | Let (Dec (x, e), b) ->
      let v = eval env e in
      let env' = Env.add x v env in
      eval env' b
  | Fn (x, e) -> VFun (x, e, env)
  | App (e1, e2) ->
      begin match eval env e1 with
        | VFun (x, e, env) ->
            let v = eval env e2 in
            let env' = Env.add x v env in
            eval env' e
        | _ -> raise (RuntimeException "Can't apply non-fun!")
      end

let do_eval e = eval Env.empty e 

let oh_no = Let (Dec ("x", Int 1),
                 Let (Dec ("x", Int 2),
                      Var "x"))
            (* 
 if 0 < 3 then 4 else 5
 *)
 
let ex = If (
    (Int 0), 
    Int 4, 
    Int 5
  )

let parse (input : string) : exp option = failwith "Exercise"