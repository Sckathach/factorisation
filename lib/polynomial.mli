(** Exception: BadType. That's a fantastic expression ! *)
exception BadType of string

(** Type polynomial: 2X+1 -> [(2, 1); (1, 0)] *)
type polynomial = (int * int) list

(** Add two polynomials *)
val add : polynomial -> polynomial -> polynomial
(** wow so beautiful ! *)

(** Convert a polynomial into a string : X³ + 3X² + 1 -> [(1, 3); (3, 2); (1, 0)] *)
val to_string : polynomial -> string

(** Returns P in Z/nZ[X] *)
val modulo : int -> polynomial -> polynomial