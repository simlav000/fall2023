(* Closures again *)
(*
  The motivation behind closures is that substitution is hella slow. 
  Instead of checking to see if we have to rename variables, substitute, check for renaming again, 
  and so on and so on, we'll define a hash map to be our "environment", and we'll simply keep track 
  of values and variable names in said map. This way, any time we encounter situations like:

  let x = 1 in let x = 2 in x, 

  instead of having to worry about accidentally binding the already bound variable x to 1 and returning the 
  incorrect result, we can analyze this expression from left to right, mapping our environment "x" to 1, then, 
  once we encounter the second binding for "x", simply update the value stored in the hash map from 1 to 2. In 
  this manner, lookup is constant time, and this error is avoided. 
*)

type name = string 

module Env = Map.Make(String)

type exp = 
  | Int of int 
  | Bool of bool 
  | If of exp * exp * exp
  | Var of name 
  | Let of dec * exp 
  | Fun of name * exp
  | App of exp * exp
and dec = 
  | Dec of name * exp 

type value = 
  | Vint of int 
  | Vbool of bool 
  | Vfun of name * exp * env (* <- first time reading this assume it is "App of exp * exp" *)
and env = value Env.t

(* let's redefine our language using this new concept. For this we'll use the Map module *)

(* With this, we'll have to redefine substitution from scratch. Also, for convenience, 
   we're throwing operators in the trash. We also won't need any of the free_variable set bullshit *)

let exp_of_value (v : value) : exp = (* this is still nice to have *)
  match v with 
  | Vint n -> Int n
  | Vbool b -> Bool b
  | Vfun (x,e,env) -> Fun (x,e) 

exception RunTimeError of string

(* when evaluating previously, if we matched on some variable Var x, we threw and error as we did not expect 
   eval to actually deal with such an expression. This was because we had the subst function that would substitute 
   the correct "simpler" expression of Int n or Bool b into it. Now, in the absence of subst, we may actually need 
   to evaluate variables. This is not a problem given eval now takes as input an environment hash map. We can simply 
   lookup the value stored at the given variable name. One such built-in Map function to do this is find_opt. We 
   want the find_opt function as opposed to the find function as find raises KeyErrors in the event a key cannot be 
   found, and we wish to throw our own errors as this is MY language *)

let rec eval (env : env) (e: exp) : value = 
  match e with 
  | Int n -> Vint n 
  | Bool b -> Vbool b 
  | If (ec, et, ef) -> 
    begin match eval env ec with 
    | Vbool true -> eval env et 
    | Vbool false -> eval env ef 
    | _ -> raise (RunTimeError "kys")
    end 
  | Var x -> 
    begin match Env.find_opt x env with 
    | None -> raise (RunTimeError ("unbound variable: " ^ x))
    | Some v -> v 
    end
  | Let (Dec (x,e), b) -> 
    let v = eval env e in           (* we still want to evaluate e in the same environment as our let *)
    let env' = Env.add x v env in   (* but we need to add x and its value to our environment *)
    eval env' b                     (* in order to evaluate b *)
  (* the fact that "adding" an element to a map creates an whole new map is actually a good thing here, because it 
     doesn't override "env", meaning the previous scope still exists and can still be used, but we now have a new environment 
     where "x" is in scope. We have the freedom to use either *)
  | Fun (x,e) -> Vfun (x,e,env)
  | App (e1, e2) -> 
    begin match eval env e1 with 
    | Vfun (x,e, captured_env) -> 
      let env' = Env.add x (eval env e2) captured_env in
      eval env' e
    | _ -> raise (RunTimeError "kys")
    end

(* now when using this eval function, generally we'll want to start with an empty environment and let the 
   values bind themselves as per the definition of our function. For this reason, we'll define: *)

let do_eval e = eval Env.empty e

(* now here's a problem with what we just did:
     | App (e1, e2) -> 
    begin match eval env e1 with 
    | Vfun (x,e) -> 
      let env' = Env.add x (eval env e2) env in
      eval env' e
    | _ -> raise (RunTimeError "kys")
    end
    
    Imagine someone tries to evaluate:
    (let x = 1 in fun y -> x) 0
    
    This is an application of the function y -> 1 on 0. We expect this to return 1. In our 
    implementation of App, this will return "RunTimeError "kys"". Why? Because "x" is never added to the 
    application's environment. The environment defined in the function above is that one that exists at the time 
    that the function is APPLIED. not at the time when the function is CREATED. This is a problem. Generally, when 
    we write functions, we have NO IDEA when it will be applied. This is especially true in the case of a library. 
    In order for App to be defined properly, we need to add "x" to the environment which existed from the function's 
    inception. This is why we retroactively went back to the type definition of App, and said it contains an env. *)