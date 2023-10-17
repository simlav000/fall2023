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

type 'a susp = Susp of (unit -> 'a)

let mk_susp (f: unit -> 'a) : 'a susp = Susp f 

let force (Susp f: 'a susp) : 'a = f ()

type 'a stream = 
  | Next of 'a * 'a stream susp

let rec ones () = Next (1, mk_susp (fun () -> ones ()))

(* Head function: Observe head of stream. I.e. get first element *)
let hd (str: 'a stream) : 'a = 
  match str with 
  | Next (x, _) -> x

(* Tail function: Observe tail of stream. I.e. get rest of stream *)
let tl (str: 'a stream) : 'a stream = 
  match str with 
  | Next (_, susp) -> force susp

let rec take (n: int) (str: 'a stream) : 'a list = 
  match n, str with 
  | n, _ when n <= 0 -> []
  | n, Next (x, susp) -> x :: take (n-1) (force susp)

let get_ones n = take n (ones ())

let rec mk_nats (n: int) = Next (n, mk_susp (fun () -> mk_nats (n+1)))
let nats = mk_nats 0

(* want to be able to use some version of "map", maybe "mapi" (map-index) that takes 
   an int (index) and the element, and does some work
   mapi: ((int * 'a ) -> 'b) -> 'a list -> 'b list 
   But we can do something clever. We already have a map function which can transform a 'a -> 'b
   We can maybe extract some 'a to be int * 'a such that each element is attached to its index
   Then this mapi function is just map
   Instead of lists let's work with streams
   We already have an inifinite stream of natural numbers. This can represent all possible indices.
   What we really want is a way to take two steams: The 'a stream we care about and the stream of naturals, 
   and zip them together pair-wise. *)

let rec zip (xs: 'a stream) (ys: 'b stream) : ('a * 'b) stream = 
  Next ((hd xs, hd ys), mk_susp (fun () -> zip (tl xs) (tl ys)))

(* Now, I can take two streams, not knowing how many elements of each stream the user may need, 
   and produce a list of both streams paired element-wise. For use in grouping the elements of 
   one stream with some index, I always know I can provide an index, no matter how many the user 
   may need (unless the reach a stack overflow due to "take" not being tail-recusrive) 
   
   I can now write something like 
   let ix_ones = zip nats (ones ())
   and be able to call this function with however many indexed ones that I need
   >> take 10 ix_ones;;
   -: (int * int) list = [(0,1); (1,1); (2,1); ...]

   In a lazy language, we could do this with lists. Maybe the lists stop eventually, but we can write 
   and index function that never has to check how long the input list is. it just takes an infinite list 
   of natural numbers, and just zips them together until the input list runs out. 
   *)

(* BLACK MAGIC: FIBONACCI STREAM *)

let rec zip_with (f : 'a -> 'b -> 'c) (xs: 'a stream) (ys: 'b stream) : 'c stream =
  Next (f (hd xs) (hd ys), mk_susp (fun () -> zip_with f (tl xs) (tl ys)))

let zip_made_with_zip_with xs ys = zip_with (fun x y -> (x,y)) xs ys

let rec fibs () = Next (0, mk_susp fibs1)
and    fibs1 () = Next (1, mk_susp (fun () -> zip_with (+) (fibs ()) (fibs1 ())))