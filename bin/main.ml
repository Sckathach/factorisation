open Lib

(**
 /\_/\
( o.o )
 > ^ <

To protect the code from bugs
*)

let () = print_string "\n\n"
let () = print_int (Cogs.f 2)
let () = print_string "\n"

let a = [(3, 4); (2, 1); (9, 0)]

let () = print_string (Polynomial.to_string a)