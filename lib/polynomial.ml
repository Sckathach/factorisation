exception BadType of string

type expr =
    Const of float
    | X
    | Sum of expr * expr
    | Mult of expr * expr
    | Pow of expr * int

let pol_test = [(-1., 3); (1., 1); (2., 0)]
(* -X^3 + X + 2 *)

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

let to_string = string_of_polynomial

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

let mult_const (p : polynomial) (x : float) : polynomial =
    if Floats.is_zero x then pol_zero else
    List.map (function (y, z) -> (x *. y, z)) p

let sub p q = add p (mult_const q (-1.))

let rec mult_xk (p : polynomial) (k : int) : polynomial = match p with
    [] -> []
    | (c, d) :: t -> (c, d+k) :: (mult_xk t k)

let mult_monomial (p : polynomial) ((c, d) : monomial) : polynomial = mult_xk (mult_const p c) d

let rec mult (p : polynomial) (q : polynomial) : polynomial = match p with
    [] -> []
    | h :: t -> add (mult_monomial q h) (mult t q)

let rec pow (p : polynomial) (n : int) : polynomial = match n with
    0 -> pol_unit
    | n -> mult p (pow p (n-1))

let rec pol_of_expr = function
    Const(x) -> [(x, 0)]
    | X -> [(1., 1)]
    | Sum(left, right) -> add (pol_of_expr left) (pol_of_expr right)
    | Mult(left, right) -> mult (pol_of_expr left) (pol_of_expr right)
    | Pow(left, n) -> pow (pol_of_expr left) n

let eval (p : polynomial) (x : float) : float =
    let rec aux p x acc = match p with
        [] -> acc
        | (c, d) :: [] -> (acc +. c) *. (Float.pow x (float_of_int d))
        | (c, d1) :: t ->
            begin
                match t with
                    (_, d2) :: _ -> aux t x ((acc +. c) *. (Float.pow x (float_of_int (d1 - d2))))
                    | _ -> failwith "ERROR : Horner"
            end
    in
        aux p x 0.

let rec differentiate (p : polynomial) : polynomial = match p with
    [] -> []
    | (c, 0) :: [] -> []
    | (c, d) :: t -> (c *. (float_of_int d), d-1) :: (differentiate t)

let rec primitive (p : polynomial) : polynomial = match p with
    [] -> []
    | (c, d) :: t -> (c /. (float_of_int d +. 1.), d + 1) :: (primitive t)

let rec integrate (p : polynomial) (a : float) (b : float) : float =
    let q = primitive p in (eval q b) -. (eval q a)