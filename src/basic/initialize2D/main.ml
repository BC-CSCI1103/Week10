(* file: main.ml

   CSCI 1103 Computer Science 1 Honors

   Some simple functions working with 1D-arrays.

   + These functions can be run from a REPL.

   + These initializations are done automatically by
     Array.make and Array.make_matrix.
*)
let b = Array.make 10 5

(* initialize : 'a array -> 'a -> unit *)
let initialize a value =
  for i = 0 to Array.length a - 1 do
    a.(i) <- value
  done

(* initialize : ('a array) array -> 'a -> unit *)
let initialize a value =
  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a.(i) - 1 do
      a.(i).(j) <- value
    done
  done
