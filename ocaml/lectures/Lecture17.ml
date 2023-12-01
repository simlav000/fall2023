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

exception NotImplemented

(* Types in MiniCAML *)
(* We extend types to support variables. *)
type tp =
  | Pair of tp * tp
  | Arrow of tp * tp
  | Int
  | Bool
  | TVar of string 

module TVarMap = Map.Make (String)

(* Different errors that can arise during unification. *)
type unif_error =
  (* Raised when attempting to unify a type variable 'a with a type t
     of which 'a is a subexpression, e.g. t is UArrow (UInt, 'a) *)
  | UnifOccursCheckFails
  (* Raised when the unifier attempts to unify mismatched types,
     e.g. Bool with Int, an Arrow with a Bool,
     or two Arrows with mismatched argument lengths. *)
  | UnifTypeMismatch of tp (* LHS *) * tp (* RHS *)

(* An exception constructor so that we can raise unif_error values. *)
exception UnifError of unif_error

(* Convenience function for raising unif_error values. *)
let unif_error e = raise (UnifError e)

(* Convenience function for raising type mismatch errors *)
let type_mismatch lhs rhs =
  unif_error (UnifTypeMismatch (lhs, rhs))

(* Used for variables, aka "identifiers" *)
type name = string

(* The primitive operations available in MiniCAML *)
type primop = Equals | LessThan | Plus | Minus | Times | Negate | Comma

(* Expressions in MiniCAML *)
type exp =
  | I of int                           (* 0 | 1 | 2 | ... *)
  | B of bool                          (* true | false *)
  | If of exp * exp * exp              (* if e then e1 else e2 *)
  | Primop of primop * exp list        (* e1 <op> e2  or <op> e *)
  | Fn of name * exp                   (* fn x => e *)
  | Rec of name * exp                  (* rec f => e *)
  | Let of name * exp * exp            (* let x = e1 in e2 end *)
  | LetPair of name * name * exp * exp (* let (x1, x2) = e1 in e2 end*)
  | Apply of exp * exp                 (* e (e_1, e_2, ..., e_n) *)
  | Var of name                        (* x *)

(* Some example programs in MiniCAML. *)
(* fun t => let (x, y) = t in (x * x) + (y * y) *) 
let ex1 = Fn ("t", LetPair ("x", "y", Var "t", 
                            Primop (Plus, [Primop (Times, [Var "x"; Var "x"]); 
                                           Primop (Times, [Var "y"; Var "y"])])))

(* fun x => true *)
let ex2 : exp = Fn ("x", B true)

(* let f = (fun t => let (x, y) = t in (x * x) + (y * y))
   in
   f (3, 4) *)
let ex3 : exp =
  Let ("f", ex1, 
       Apply (Var "f", Primop (Comma, [I 3; I 4])))

(* let g = (fun x => true)
   in
   g 0 *)
let ex4 : exp =
  Let ("g", ex2,
       Apply (Var "g", I 0))

(* let f = (fun (x, y) => (x * x) + (y * y))
   in
   f (3)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex5 : exp =
  Let ("f", ex1,
       Apply (Var "f", I 3))

(* let f = (fun (x) => (fun (y) => (x * x) + (y * y)))
   in
   (f (3)) (4)
*)
let ex6 : exp =
  Let ("f",
       Fn ("x",
           Fn ("y",
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       Apply (Apply (Var "f", I 3),
              I 4))

(* let f = (fun (x) => (fun (y) => (x * x) + (y * y)))
   in
   f (3, 4)
   Note: this expression is syntactically valid, but ill-typed!
*)
let ex7 : exp =
  Let ("f",
       Fn ("x",
           Fn ("y",
               Primop (Plus,
                       [Primop (Times, [Var "x"; Var "x"]);
                        Primop (Times, [Var "y"; Var "y"])]))),
       (Apply (Var "f", Primop (Comma, [I 3; I 4]))))

(* PART 1: eval *)

(* Substitution functions similar to HW9 are in scope as the values:
   [val subst : exp * name -> exp -> exp]
   [val subst_list : (exp * name) list -> exp -> exp]
*)

(* Runtime errors that may be raised by eval. *)
type runtime_error =
  | Free_variable of name
  | Bad_primop_args
  | If_non_true_false
  | Apply_non_fn
  | LetPair_non_tuple

exception Stuck of runtime_error


(* Evaluates a primitive operation *)
let eval_op (op : primop) (exps : exp list) : exp option =
  match op, exps with
  | (Equals,   [I i; I i']) -> Some (B (i = i'))
  | (LessThan, [I i; I i']) -> Some (B (i < i'))
  | (Plus,     [I i; I i']) -> Some (I (i + i'))
  | (Minus,    [I i; I i']) -> Some (I (i - i'))
  | (Times,    [I i; I i']) -> Some (I (i * i'))
  | (Negate,   [I i])       -> Some (I (-i))
  | (Comma,    [I i; I i']) -> Some (Primop (Comma, exps))
  | _                       -> None

(* PART 2: unify *)

let rec apply_type_substitution (subst : tp TVarMap.t) (tp : tp) : tp =
  match tp with
  | Int | Bool -> tp
  | Arrow (t,t') ->
      Arrow (apply_type_substitution subst t, apply_type_substitution subst t')
  | Pair (t, t') ->
      Pair (apply_type_substitution subst t, apply_type_substitution subst t')
  | TVar v ->
      match TVarMap.find_opt v subst with
      | None -> tp
      | Some t -> t

let rec string_of_tp (subst : tp TVarMap.t) (tp : tp) : string =
  match tp with
  | Arrow (t, t') -> "(" ^
                     (string_of_tp subst t) ^
                     " -> " ^ 
                     (string_of_tp subst t') ^ ")"
  | Pair (t, t') -> "(" ^
                    (string_of_tp subst t) ^
                    ", " ^ 
                    (string_of_tp subst t') ^ ")"
  | Int -> "int"
  | Bool -> "bool"
  | TVar v ->
      match TVarMap.find_opt v subst with
      | None -> "'" ^ v
      | Some tp -> string_of_tp subst tp

let string_of_type_substitution (type_substitution : tp TVarMap.t) : string =
  "[\n"
  ^ (TVarMap.fold (fun key value acc ->
      acc ^ "  " ^ (string_of_tp type_substitution value) ^ " / '" ^ key ^ ",\n"
    ) type_substitution "")
  ^ "]"

let print_tp tp = print_string @@ string_of_tp TVarMap.empty tp

let print_type_substitution subst = print_string @@ string_of_type_substitution subst

(* PART 3: infer *)

(* Type contexts *)
module Context = Map.Make(String)
type context = tp Context.t
let empty = Context.empty

(* Looks up the topmost x in ctx and return an option. *)
let lookup (x : name) (ctx : context) = Context.find_opt x ctx

(* Adds a new type ascription to a context. *)
let extend ctx (x, tau) = Context.add x tau ctx

(* Adds multiple new type ascriptions to a context. *)
let extend_list (ctx : context) (l : (name * tp) list) =
  List.fold_left extend ctx l

(* Type errors that may be raised by infer *)
type type_error =
  | Free_variable of name

exception TypeError of type_error

let free_variable name = raise (TypeError (Free_variable name))

(* Computes the type of a primitive operation.
   The result is a tuple representing the domain and range of the primop.
*)
let primopType (p : primop) : tp list * tp = match p with
  | Equals   -> ([Int; Int], Bool)
  | LessThan -> ([Int; Int], Bool)
  | Plus     -> ([Int; Int], Int)
  | Minus    -> ([Int; Int], Int)
  | Times    -> ([Int; Int], Int)
  | Negate   -> ([Int], Int)
  | Comma    -> ([Int; Int], Pair (Int, Int))


(* -------------------------------------------------------------*)
(* Other helper functions                                       *)
(* You don't need to look at these to do the assignment, but it *)
(* would be a good idea to understand them.                     *)
(* -------------------------------------------------------------*)

(* Generating fresh (new) variable names *)
type gen_var = {
  fresh : name -> name; (* generates a fresh name based on a given one. *)
  reset : unit -> unit (* resets the internal counter for making names. *)
}

let gen_var : gen_var =
  let counter = ref 0 in
  let fresh x = incr counter; x ^ (string_of_int (!counter)) in
  let reset () = counter := 0 in
  {fresh; reset}

(* Use this function to generate fresh vars. *)
let fresh_var = gen_var.fresh
let reset_ctr = gen_var.reset

(* String representations of expressions. Useful for debugging!
   Note that this expression printer is very primitive, but it should suit
   your needs most of the time.
*)
let nl_sep l = String.concat "\n" l

let bracket str = "(" ^ str ^ ")"

let string_of_op p = match p with
  | Equals   -> " = "
  | LessThan -> " < "
  | Plus     -> " + "
  | Minus    -> " - "
  | Times    -> " * "
  | Negate   -> "-"
  | Comma    -> ", "

let rec string_of_exp indent exp =
  let new_ind = indent ^ "  " in
  let string_of_exp' = string_of_exp indent in
  let string_of_exp'' = string_of_exp new_ind in
  match exp with
  | I n ->
      if n < 0 then bracket (string_of_int n)
      else string_of_int n
  | B b -> if b then "True" else "False"
  | If (p, e1, e2) ->
      nl_sep
        ["if " ^ (string_of_exp'' p) ^ " then";
         new_ind ^ (string_of_exp'' e1);
         indent ^ "else";
         new_ind ^ (string_of_exp'' e2)]
  | Primop (p, el) ->
      bracket @@
      if p = Negate then
        (string_of_op p) ^ (string_of_exp' (List.nth el 0))
      else if p = Comma then
        "(" ^ (string_of_exp' (List.nth el 0)) ^
        (string_of_op p) ^
        (string_of_exp' (List.nth el 1)) ^ ")"
      else
        (string_of_exp' (List.nth el 0)) ^
        (string_of_op p) ^
        (string_of_exp' (List.nth el 1))
  | Fn (name, exp) -> 
      bracket @@
      nl_sep
        ["fun (" ^ name ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Rec (name, exp) ->
      bracket @@
      nl_sep
        ["rec (" ^ name ^ ") =>";
         new_ind ^ (string_of_exp'' exp)]
  | Let (name, e1, e2) ->
      nl_sep
        ["let " ^ name ^ " = " ^ (string_of_exp' e1) ^ " in";
         new_ind ^ (string_of_exp'' e2)]
  | Apply (f, e) -> 
      (string_of_exp' f) ^ " " ^ (string_of_exp' e)
  | LetPair (x1, x2, e1, e2) ->
      nl_sep
        ["let (" ^ x1 ^ ", " ^ x2 ^") = " ^ (string_of_exp' e1) ^ " in";
         new_ind ^ (string_of_exp' e2)]
  | Var name -> name

let print_exp exp = print_string (string_of_exp "" exp)