(* 5.1 Binding, Scope *)

(*
  So far, we have seen variables in let-expressions or functions. An important point about these local variables or binders
  were that:
  1) They onlyexist within a scope
  2) Their names did not matter
  Consider the following example:
*)

let test () = 
  let pi = 3.14 in                                  (*1*)
  let area = (fun r -> pi *. r *. r) in             (*2*) 
  let a2 = area (2.0) in                            (*3*) 
  let pi = 6.0 in                                   (*4*)
  let a3 = area 2.0 in                               
print_string ("Area a2 = " ^ string_of_float a2 ^ "\n");  (*Area a2 = 12.56*)
print_string ("Area a3 = " ^ string_of_float a3 ^ "\n");  (*Area a3 = 12.56*)

(* 
  The binder or local variable pi in line (*1*) is bound to 3.14. If we are trying to establish a new binding for pu in line (*4*), then 
  this only overshadows the previous binding, but it is important to note that it does no effect or change other bindings such as area or a2.
  In fact, OCaml will give you a warning:
    File "Midterm2Review(References).ml", line 15, characters 6-8:
    15 |   let pi = 6.0 in
              ^^
    Warning 26 [unused-var]: unused variable pi.
  Since we do not use the second binding for pu in the subsequent code, the variable pi with the new 6.0 is unused. The result of the expression 
  will be 12.56 for both a2 and a3 as they both use the previous value of pi = 3.14. The fact that we shadowed the previous binding of pu in line (*4*)
  did not have any effect
*)

(* Reference Cells *)

(*
  To support mutable storate, we will extend our language with reference cells. A reference cell may be thought of as a container in which a data 
  value of a specified type may be stored. During the execution of a program, the contents of a cell may be read or replaced by any other value of 
  the appropriate type. Since reference cells are updated and ready by issuing "commands", progamming with cells is also called imperative programming.

  Changing the contents of a reference cell introduces a temporal aspect. We often will spreak of previous and future values of a reference cell when 
  discussing the behaviour of a program. This is in sharp contrast to the effect-free fragment we have encounteres so far. A binding for a variable
  does not change when we evaluate withing the scope of that variable. The content of a reference cell may well change when we evaluate another expression.

  A reference cell is created or allocated by te constructor "ref". ref 0 will create a reference cell with content 0, i.e. it is a reference cell 
  in which we store integers. Hence the type of ref 0 will be int ref. Reference cells are like all other values. They may be bound to variable names,
  passed as arguments to functions, returned as results of functions, even be stored within other reference cells.

  Here are some examples:
*)

(*
let r = ref 0;
let s = ref 0; 
*)

(*
  In the first line, we create a new reference cell and initialize it with content 0. The name of the reference cell is "r". In the second line, we 
  create a new reference cell with content 0 and bind it to the name s. It is worth pointing out that s and r are not referring to the same cell!
  In fact, we can compare s and r, by s = r which will evaluate to false! In OCaml, there are two comparisons we can in fact make:
    * r == s: Compares whether the cell r, i.e. r's address in memory, is the same cell as s, i.e. s's address in memory.
    * r = s: Compare the contents of both cells
  We can read the content of a reference cell using the construct ! This means:
    * !r == !s
  !r yields the current content of the reference cell r, in this example it will return 0. We can also directly pattern match on the content of a call 
  using the following pattern:
*)

(*
  let {contents = x} = r;
    > val x : int = 0
  let {contents = y} = s;
    > val y : int = 0

  In a function:
*)
  
  let double_reference (r : int ref) : int ref = 
    match r with
    | {contents = x} -> ref (x + x)

(*
  This function takes as input a reference, and returns a NEW reference whose contents are twice that of the input reference's contents.
*)

(*
  Here x and y are bound to the values contained in the cell r and s respectively. To change the content of a reference cell, we use the assignment 
  operator := and it typically is written as an infix operator. The furst argument must be a reference cell of type τ ref, and the second argument 
  must be an expression of type τ. The assignment r := 5 will replace the content of the cell r with the value 5. Note that the old content of the 
  cell r has been destroyed and does not exist anymore!
  Consider the following piece of code:

  let r = ref 0
  let s = ref 0
  let a = r = s       -> a = true  (same contents: 0)
  let a = r == s      -> a = false (different address)
  let _ = r := 3      -> !r = 3
  let x = ! s + ! r   -> x = 0 + 3 = 3
  let t = r           -> t now points to the same cell as r (having contents 3)
  let w = r = s       -> w = false (this is comparing contents. !r = 3 and !s = 0)
  let v = t = s       -> v = false (this is comparing contents. !t = 3 and !s = 0)
  let b = s == t      -> b = false (these references point to different addresses) 
  let c = r == t      -> c = true  (these two references are the same! They have the same address)
  let _ = t := 5      -> !t = 5 SO !r = 5 (since r and t are the same reference!)
  let y = ! s + ! r   -> y = 0 + 5 = 5
  let z = ! t + ! r   -> z = 5 + 5 = 10
*)

(*
  Often it is convenient to sequentially compose expressions. This can be done using the semicolon ;
  The expression:
  > exp1 ; exp2
  Is shorthand for:
  > let _ = exp1 in exp2
  It essentially means we first evaluate exp1 for its effect, and then evaluate exp2. Reqriting the introductry example with references gives us:
*)

let test_updates () = 
  let pi = ref 3.14 in 
  let area = (fun r -> !pi *. r *. r) in 
  let a2 = area 2.0 in 
  let _ = (pi := 6.0) in 
  let a3 = area 2.0 in 
print_string (" Area a2 = " ^ string_of_float a2 ^ "\n"); (* a2 = 12.56 *)
print_string (" Area a3 = " ^ string_of_float a3 ^ "\n"); (* a3 = 24.00 *)

(*
  Note that now a2 will still bind to 12.56, while when we compute the value of a3 the result will now be 24. Udating the reference cell pi will 
  have an effect on the previously defined function area. Since an assignment destroys the previous values held in a reference cell, it is also 
  sometimes called a destructive update.
*)

(* Mutable Data Structures *)

(*
  So far we have only considered immutable data structures such as lists or trees, i.e. data structures whose structure cannot be changed without 
  rebiulding a modified copy of the structure. Immutable data structures are persistent. I.e. operations performed on them does not destroy the 
  original structure. This often makes our implementations easier to understand and reason about. However, sometimes we do not want to rebuild a 
  data structure. A classic example is maintaining a dictionary. It is clearly wasteful if we would need to carry around a large dictionary
  and when we want to update it, we copy the whole thing. What we would like to do is an "in-place update" operation. For this we must have 
  ephemeral (opposite of persistent) data structures. We can achieve this by using references.
*)

type 'a rlist = Empty | RCons of 'a * 'a rlist ref 

(* then we can define a circular list as follows *)

let l1 = ref (RCons(4, ref Empty))
let l2 = ref (RCons(5, l1))

(* utop: l1 := !l2;; *)

(* how do l1 and l2 look now? *)
(*
  Before the assignment:
  l2 -> |5 -|-> |4 -|-> |Empty |
                  ^l1
  After the assignment l1 := !l2;;
  l1:  |-----------|
       |---> |5 -|-|
  (think: l1 is a cell containing 5 pointing back to itself)
  l2:       |--------|
    ->|5 -|-|->|5 -|-|
  (think: l2 is a cell containing a 5 which points to another cell containing a 5 which points to itself)
*)

(* here is a destructive append function *)
type 'a rlist = Empty | RCons of 'a * 'a rlist ref 

let rec rapp r1 r2 =
  match r1 with 
  | {contents = Empty} -> r1 := !r2 
  | {contents = RCons (h,t)} -> rapp t r2

(* Note that as a side effect, the reference list r1 is updated. This change can only be observed by reading r1 after we have appended r2*)

(* consider this destructive reverse function *)

let rec rev l =
  let r = ref Empty in 
  match l with 
  | {contents = Empty} -> l := !r 
  | {contents = RCons(h,t)} -> (r := RCons (h, ref !r); rev t)

(* consider this destructive reverse function *)

let rev l =
  let r = ref Empty in
  let rec rev' l =
    match l with
    | {contents = Empty} -> l := !r
    | {contents = RCons (h, t)} ->
        (r := RCons (h, ref (!r)); rev' t)
  in
  rev' l

(*
  The idea is that we first create a temporary reference to an empty list. While we recursively traverse the input list l, we will pop the elements 
  from l and push them onto the temporary list r. When we are done, we let reassign l := !r so it points to the reversed list. If, instead of 
  returning a unit (), we wanted to return the reversed list, as is often the case in imperative programming, we change the last line to 
  (rev' l; l)
*)

(* Closures, References and Objects *)

(* We can also write a counter which is incremented by the function tick and reset by the function reset *)

let counter = ref 0 in 
let tick () = counter := !counter + 1; !counter in 
let reset () = counter := 0 in 
  (tick, reset)

(*
   This declaration introduces two functions "tick : unit -> int" and "reset : unit -> unit". Their definitions share a private variable 
   "counter" that is bound to a reference cell containing the current value of their shared counter.The tick operation increments the 
   counter and returns its new value, while the reset operations sets it back to zero. The types already suggest that implicit state 
   is involved. We then package the two functions together in a tuple.

   Suppose we wish to have several different instances of a counter and different pairs of functions tick and reset. We can achieve this by 
   defining a counter generator. We first declare a "record" counter_object which contains two functions (i.e. methods). You can think of this 
   as the interface or signature of the object. We then define the methods in the functoin newCounter which when called will give us a new object 
   with methods tick and reset.
*)

type counter_object = 
{
  tick: unit -> int;
  reset: unit -> unit;
  start: int
}

let newCounter (start : int) = 
  let counter = ref start in 
  {
    tick = (fun () -> counter := !counter + 1; !counter);
    reset = (fun () -> counter := start)
  }

(*
  We've packaged the two operations into a record containing two functions that share the same private state. There is an obvious analogy 
  with class-based object-oriented programming. The function newCounter may be thought of as the constructor for a class of counter objects.
  Each object has a private instance variable counter that is shared between the methods tick and reset.
  Here is how to use the counters:
  
  # let c1 = newCounter () ;;
  # let c2 = newCounter () ;;
  # c1.tick;;
  - : unit -> int = <fun>
  # c1.tick;;
  - : int = 1
  # c1.tick;;
  - : int = 2
  # c2.tick;;
  - : int = 1

  Notice that c1 and c2 are distinct counters!
*)
  
(* Other Common Mutable Data Structures *)

(*
  We already have seen records as an example of a mutable data-structure in OCaml. Records are useful for creating a data entry, for example.
  Here we create a data entry for a student by defining a record type "student" with fields "name", "id", "birthdate", and "city". We also 
  include a field "balance" which describes the amount the student is owing to the university. We define the field balance as "mutable" which 
  ensures that we can update it. For example, we want to set it to 0 once the outstanding balance has been paid.
*)

type student = 
{
  name: string;
  id: int;
  birthdate: int * int * int;
  city: string;
  mutable balance: float;
}

(* We can no create the first student data entry: *)
(* 
  1 # let s1 = { name = " James McGill "; id = 206234444; birthdate = (6 , 10 , 1744) ; city = " Montreal "; balance = 1650.0 };;

  To access and update the student's balance we simply write:
  s1.balance <- 0.0;;

  We can only update a field in a record if it is declared to be mutable
*)

(* we can also do something very similar with references *)

type student = 
  {
    name: string;
    id: int;
    balance: float ref;
    set_balance: float -> unit; 
  }
  
let create_student (name: string) (id: int) (balance: float) = 
  let balance_ref = ref balance in
  {
    name;
    id;
    balance = balance_ref;
    set_balance = (fun new_balance -> balance_ref := new_balance); 
  }

