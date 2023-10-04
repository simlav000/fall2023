(* how functions get evaluated *)
fun a b c -> a

(* this is an anonymous function having 3 inputs, and one output. The function has no name meaning a b c are inputs *)
(*
   If we call this function with two inputs, like a b, the function will return another function awaiting a third input
   Upon calling this returned function with some input, THAT function will finally return a.
     
*)

(* CPS questions *)

(* 
   say we have an n x m grid, and we start in the top left corner. 
   We want to know how many ways we can get to the bottom right by only going down and right
   assuming there's a blockade in the grid at the point x y
   Let's think of some base cases:
    1) if the grid is 1x1
*)

let solve m n x y =
  let rec help n m = 
    if (n = 1 && m = 1) then 1
    else if (n = 0 || m = 0) then 0
    else if (n = x && m = y) then 0
    else help (n-1) m + help n (m-1)
  in
  help n m

(* TRANSLATING TO CPS *)
(* STEP 1: COPY PASTE EVERYTHING*)

let solve m n x y =
  let rec help n m = 
    if (n = 1 && m = 1) then 1
    else if (n = 0 || m = 0) then 0
    else if (n = x && m = y) then 0
    else help (n-1) m + help n (m-1)
  in
  help n m

(* STEP 2: ADD A CONTINUATION FUNCTION TO THE RECURSIVE PART *)

let solve m n x y =
  let rec help n m return = 
    if (n = 1 && m = 1) then 1
    else if (n = 0 || m = 0) then 0
    else if (n = x && m = y) then 0
    else help (n-1) m + help n (m-1)
  in
  help n m

(* STEP 3: USE CONTINUATION TO RETURN ANY VALUES *)

let solve m n x y =
  let rec help n m return = 
    if (n = 1 && m = 1) then return 1
    else if (n = 0 || m = 0) then return 0
    else if (n = x && m = y) then return 0
    else help (n-1) m + help n (m-1)
  in
  help n m (fun x -> x) (* <- THIS IS JUST THE IDENTITY FUNCTION, SINCE WE WANT TO RETURN WHATEVER "help n m" RETURNS *)

(* STEP 4: FIGURE OUT WHAT CONTINUATION HAS TO BE PASSED WHEN DOING ACTUAL RECURSIVE CALLS *)

let solve m n x y =
  let rec help n m return = 
    if (n = 1 && m = 1) then return 1
    else if (n = 0 || m = 0) then return 0
    else if (n = x && m = y) then return 0
    else help (n-1) m (fun r -> ???) + help n (m-1) (fun l -> ???)
    (* IN CONTINUATION PASSING STYLE, WE CAN'T BE ADDING TWO RESULTS OF RECURSIVE CALLS. THIS 
       WON'T BE TAIL RECURSIVE. THUS THE "+ help n (m-1) (fun l -> ???)" PART HAS TO GO INTO THE 
       "(fun r -> ???)" PART
    *)
  in
  help n m (fun x -> x)

let solve m n x y =
  let rec help n m return = 
    if (n = 1 && m = 1) then return 1
    else if (n = 0 || m = 0) then return 0
    else if (n = x && m = y) then return 0
    else help (n-1) m (fun r -> help n (m-1) (fun l -> return (r + l))) 
    (*                     ^                      ^ AND THIS IS THE RESULT OF CALLING "help n (m-1)""
                           |THIS IS THE RESULT OF CALLING "help (n-1) m"
      SO WE JUST MAKE THE FINAL CONTINUATION BE A FUNCTION THAT ADDS THESE RESULTS TOGETHER *)


(* let's now do a trace on a simple input *)
(* 
  Say we have a 2x2 grid with a blockade at (-1,-1) (i.e. no blockade)
  solve 2 2 -1 -1 (fun x -> x) <- this matches with the recursive call case 
  help (1) 2 (fun r -> help 2 (1) (fun l -> (fun x -> x) (l + r)))
  ^^^^^^^^^^ -> this bit will return 1 as it matches with the first case 
  >> (fun (1) -> help 2 (1) (fun l -> (fun x -> x) (l + r)))
                 ^^^^^^^^^ -> now this will also return 1
  >> (fun (1) -> (fun (1) -> (fun x -> x) (l + r)))
  And maybe it would have been nicer to write al this as 
  >> (fun r -> (fun l -> (fun x -> x) (l + r))) 1 1
  >> (fun (1) -> (fun (1) -> (fun x -> x) (1 + 1)))                                      
*)