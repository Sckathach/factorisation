(** Fast exponentiation: pow n a b *)
val pow : int -> int -> int -> int

(** Division using Fermat's theorem: div n a b
Raises DivByZero *)
val div : int -> int -> int -> int

(** Inverse in Z/pZ using Fermat's theorem: inverse p a
Raises DivByZero *)
val inverse : int -> int -> int
