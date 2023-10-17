(* TRYING TO UNDERSTAND THIS FEATURE *)
(*
  utop # let add x y = x + y;;             DEFINING A FUNCTION TO SIMPLY ADD TWO ARGUMENTS
  val add : int -> int -> int = <fun>
  ─( 12:50:40 )─< command 22 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
  utop # add 2 3;;                         WE CAN GIVE IT TWO ARGUMENTS, AS EXPECTED, AND IT RETURNS THEIR SUM
  - : int = 5
  ─( 12:53:18 )─< command 23 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
  utop # add 2;;                           WE CAN ALSO GIVE IT A SINGLE ARGUMENT, AND IT RETURNS A FUNCTION READY
  - : int -> int = <fun>                   TO TAKE IN THE SECOND ONE, EAGER TO COMPUTE THE SUM
  ─( 12:53:23 )─< command 24 >─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────{ counter: 0 }─
  utop # (add 2) 3;;                       HERE THE PARENTHESES DEFINE A PARTIALLY EVALUATED FUNCTION, WE THEN PASS 
  - : int = 5                              THE SECOND ARGUMENT AND THEIR SUM IS COMPUTED
*)

