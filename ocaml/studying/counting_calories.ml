(* 
   Let's assume we're able to read files, and make a list containing integers until a newline character is reached 
   such that if our file contains:
    1000
    2000
    3000

    4000

    5000
    6000
    ...
   Then the following lists are bound: [1000; 2000; 3000], [4000], [5000;6000] ...
   If we want to count the sum of each of these lists and return the maximum count at the end, we can use 
   a function that can operate on an arbitrary list length.
*)

let rec sum_list (l: int list) : int = 
  let rec help l acc =
    match l with 
    | [] -> acc 
    | e :: l -> help l (acc + e)
  in help l 0

(*
   Now all I need is a function that can process the file and build these lists, call sum_tuple, and store the maximum value to be returned 
   when there are no more lists to process
*)

let parse_lists (biglist: int list list): int list =
  (* turn a list of lists into a list of sums of the elements of the inner list *)
  let rec help biglist totals = 
    match biglist with 
    | [] -> totals
    | sl :: l -> help biglist (sum_list sl :: totals)
  in help biglist []

let find_max list = 
  let rec remember list max =
   match list with 
    | [] -> max
    | e :: list -> remember list (if e > max then e else max)
  in remember list 0

let count_calories biglist =
  find_max (parse_lists biglist)
