(* Define a custom exception for when no subset is found *)
exception NoSubsetFound

(* Define a custom function to convert a list of tuples to a string *)
let rec list_of_tuples_to_string lst =
  match lst with
  | [] -> "[]"
  | (x, y) :: [] -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"
  | (x, y) :: rest -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ "); " ^ list_of_tuples_to_string rest

(* Implement the subset_sum function *)
let subset_sum (target: (int * int)) (tuples : (int * int) list) : (int * int) list =
  let rec subset_sum' (t1, t2) tuples result = 
    match tuples with 
    | [] -> if target == (0, 0) then result else raise NoSubsetFound
    | (x1, x2) :: tuples' ->
        try
          (* Print the arguments for each recursive call *)
          print_endline ("Calling subset_sum' with target: (" ^ string_of_int t1 ^ ", " ^ string_of_int t2 ^ ") and tuples: " ^ list_of_tuples_to_string tuples);
          subset_sum' target tuples' result
        with
        | NoSubsetFound -> 
          let new_t1 = t1 - x1 in
          let new_t2 = t2 - x2 in
          (* Print the subset and new arguments when NoSubsetFound is raised *)
          print_endline ("Found subset (" ^ string_of_int x1 ^ ", " ^ string_of_int x2 ^ ")");
          print_endline ("New target: (" ^ string_of_int new_t1 ^ ", " ^ string_of_int new_t2 ^ ")");
          subset_sum' (new_t1, new_t2) tuples' ((x1, x2) :: result)
  in
  let result = subset_sum' target tuples [] in
  print_endline ("Result: " ^ list_of_tuples_to_string result);
  result

(* Test cases *)
let subset_sum_option_tests : (((int * int) * (int * int) list) * (int * int) list) list = [
  (((10, 5), [(2, 2); (8, 3)]), [(2, 2); (8, 3)]);
  (((10, 5), [(2, 3); (8, 3)]), []);
]

(* Run the test cases and print the results *)
let () =
  List.iter (fun ((target, tuples), expected_result) ->
    let result = subset_sum target tuples in
    print_endline ("Expected: " ^ list_of_tuples_to_string expected_result);
    print_endline ("Actual: " ^ list_of_tuples_to_string result);
    print_endline ""
  ) subset_sum_option_tests
