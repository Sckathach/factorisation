exception BadType of string

(*
    [(-1., 3); (1., 1); (2., 0)]
    -> -X^3 + X + 2
*)

type degree = int
type coefficient = float
type monomial = coefficient * degree
type polynomial = monomial list

let pol_zero = [(0., 0)]
let pol_unit = [(1., 0)]

let string_of_monomial ((c, d) : monomial) : string =
    string_of_float c ^ "X^" ^ string_of_int d

let rec string_of_polynomial = function
    [] -> string_of_polynomial pol_zero
    | x :: [] -> string_of_monomial x
    | x :: q -> (string_of_monomial x) ^ " + " ^ (string_of_polynomial q)

let rec add (p : polynomial) (q : polynomial) : polynomial = match (p, q) with
    [], _ -> q
    | _, [] -> p
    | (cp, dp) :: tp, (cq, dq) :: tq ->
        if dp = dq then
            begin
                if (Floats.is_zero (cp +. cq)) then
                    add tp tq
                else
                    (cp +. cq, dp) :: (add tp tq)
            end
        else
            if dp > dq then
                (cp, dp) :: (add tp q)
            else
                (cq, dq) :: (add p tq)


(* type polynomial = (int * int) list *)
(* let pol = [(0, 3) ; (-1, 2); (0, 1) ; (2, 0)] *)

(* let rec add (p : polynomial) (q : polynomial) : polynomial = match p, q with *)
(* [], _ -> q *)
(* | _, [] -> p *)
(* | (a, b) :: x, (c, d) :: y -> *)
(*    if b = d then *)
(*        let s = a + c in *)
(*            if s = 0 then *)
(*                add x y *)
(*            else *)
(*                (s, b) :: add x y *)
(*    else if b > d then *)
(*        (a, b) :: add x q *)
(*    else *)
(*        (c, d) :: add p y *)
(*  *)
(* let to_string (p : polynomial) : string = *)
(*    let rec aux = function *)
(*        [] -> "" *)
(*        | (a, 0) :: q -> (Printf.sprintf "+ %d " a) ^ (aux q) *)
(*        | (a, 1) :: q -> (Printf.sprintf "+ %dX " a) ^ (aux q) *)
(*        | (a, b) :: q -> (Printf.sprintf "+ %dX^%d " a b) ^ (aux q) *)
(*    in *)
(*        let s = aux p in *)
(*            if String.length s >= 2 then *)
(*                String.sub s 2 (String.length s - 2) *)
(*            else *)
(*                "" *)
(*  *)
(* let modulo (n : int) (p : polynomial) : polynomial = *)
(*    let rec aux acc = function *)
(*        [] -> List.rev acc *)
(*        | (x, y) :: q -> aux ((x mod n, y) :: acc) q *)
(*    in aux [] p *)
(*  *)
let mult_const (p : polynomial) (x : float) : polynomial =
    if Floats.is_zero x then pol_zero else
    List.map (function (y, z) -> (x *. y, z)) p

let sub p q = add p (mult_const q (-1.))

let rec mult_xk (p : polynomial) (k : int) : polynomial = match p with
    [] -> []
    | (c, d) :: t -> (c, d+k) :: (mult_kx t k)

let mult_monomial (p : polynomial) ((c, d) : monomial) : polynomial = mult_xk (mult_const p c) d

let rec mult (p : polynomial) (q : polynomial) : polynomial = match p with
    [] -> []
    | h :: t -> add (mult_monomial q h) (mult t q)

let rec exp (p : polynomial) (n : int) : polynomial = function
    0 -> pol_unit
    | n -> mult p (exp p (n-1))

