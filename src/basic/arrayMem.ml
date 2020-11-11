(* file: arrayMem.ml

   CSCI 1103 Computer Science 1 Honors

   A simple function working with 1D-arrays. Uses the built-in
   Exit exception.  Run it in a REPL.

   arrayMem : 'a -> 'a list -> bool *)
let arrayMem x a =
  try
    for i = 0 to Array.length a - 1 do
      match x = a.(i) with
      | true  -> raise Exit
      | false -> ()
    done ;
    false
  with | Exit -> true
