(* file: main.ml

   CSCI 1103 Computer Science 1 Honors

   Some simple functions working with 1D-arrays.

   This can be run from the REPL but a complete file path is required
   for the open_in function.
*)
(* readLines : string -> string list *)
let readLines file =
  let inch = open_in file in
  let rec repeat lines =
    try
      let line = input_line inch
      in
      repeat (line :: lines)
    with
      End_of_file -> close_in inch;
      lines
  in
  List.rev (repeat [])
