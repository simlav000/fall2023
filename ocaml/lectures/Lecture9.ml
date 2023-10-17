(* REPL *)
(* 
   typing #show List in the toplevel prints out a series of functions defined on List
   these include (but are definitely not limited to):
      * map
      * fold-right
      * fold-left
      * exists
      * for_all
*)

(* LAZY FROGRAMMING *)

(* Eager evaluation *)
(*
   AKA: Call-by value
   Arguments are evaluated before function application
   This is what we're used to. f e1 e2 is the same as:
    let x = e1 in 
    let y = e2 in
    f x y
   Concrete example: let x = 3 + 2 in x * 2
   Here, 3 + 2 is evaluated first before x is bound to the value 5. Then, we can perform x * 2 to return 
   a final answer.
   
   Suppose we have a function called Horrible : int -> int
   defined as:
   let x = Horrible 302 in 5
   Imagine now that calling this function makes the code run for several minutes
   We can see that whatever the result of Horrible 302 is, we don't even use it. We just 
   let x = 5. It would be nice if we could save some time and not call functions when it 
   isn't necessary. 

   Another example:
   What does this program do when it runs?
*)

let f = fun () -> print_string "line1\n" in 
let () =          print_string "line2\n" in 
f () 

(*
   Answer:
   line2 
   line1
*)
(* 
   Why?
   Even though print_string "line1\n" appears first, we can't evaluate f before its arguments 
   are initialized. Thus we have to run () first, which will cause line2 to print, then, line1
   
   Upshot:
    * This means we can "delay" the evaluation of something by surrounding it with 
      (fun () -> ....)
    * With this we can call the function only when we need it 
    * We have technically already used this with "fail" continuations 
*)

(*
  Don't fall for this trap though:
  
  let x () = Horrible 302 in x () + x ()
  
  What does this do?
    * Associate x -> fun () -> Horrible 302
    * call x () to get 777
    * call x () to get 777
    * Evaluate 777 + 777 and return 1554
    -> Horrible 302 is duplicated, and evaluated twice!
*)


(* Call-by-Need *)
(*
   A language using call-by-need evaluation handles this 
   let x = Horrible 302 in x + x
    1. Associate x -> Horrible 302 
      * that is, to a representation of the expression itself 
    2. Evaluate x + x: first evaluate the necessary variable x 
      * Evaluate Horrible 302 to 777
      * Reassociate x -> 777 
    3. Use the rebound x to evaluate x + x 
    4. Obtain result 1554 
    -> Horrible 302 is only evaluated once! 
    We haven't (yet) seen the tools to do this in OCaml
*)

(* Side effects *)
(* 
   Suppose we're working in a lazy language.
    * map: ('a -> 'b) -> 'a list -> 'b list    is the familiar function 
    * take: int -> 'a list -> 'a list          takes from the start of a list (takes the first n elements of the list)
    * what does "take 2 (map print_int [1;2;3;4;5])" do?
    - Maybe it only prints the first two elements since we are lazy on only look at the first 2 
    - Maybe it prints the whole list once as we "map print_int [1;2;3;4;5]" and then further print [1;2] 
    ! The correct answer is the first. Only 1, 2 are printed, since the rest of the list is simply not needed 

    Certain languages that opt for lazy programming must then try to address issues of functions doing something 
    other than simply taking arguments and returning results.
    For this course we are going to ignore that and write functions that do nothing else. Just take in inputs and give outputs.
*)

(* Eager vs. Lazy *)
(*
   Eager:
    + Easier to reason about evaluation order 
    + Clear when evaluation happens 
    + Clear what evaluation happens
    - May evaluate expressions that are never needed 

   Lazy:
    - Harder to reason about evaluation order 
    - Side effects must be managed very differently 
    + Constructs like if, then, else are _functions_, not special 
    + Can represent interactive data, e.g. network interactions 
    + Can represent infinite data, e.g. the "stream" of all prime numbers
*)

(* INFINITE DATA *)

(*recall:*)

type 'a list = 
  | [] 
  | (::) of 'a * 'a list 

(*
   * The constructors contain data 
   * But if we try to put infinite data, we can never finish computing it
  Instead of invinite data, we put _observations_ 
*)

type 'a stream = 
  | Next of 'a * (unit -> 'a stream)

(* we can observe on-demand: *)

let tl (Next (_,f)) = f ()