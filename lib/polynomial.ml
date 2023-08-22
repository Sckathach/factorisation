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

let trapezes p a b n =
    let dx = (b -. a) /. (float_of_int n) in
    let sum = ref (eval p a +. eval p b) in
    for i = 1 to n-1 do
        let j = float_of_int i in
        sum := 2. *. (eval p (dx *. j)) +. !sum
    done;
    (b -. a) /. (2. *. (float_of_int n)) *. !sum

let lagrange_factor x = [(1., 1); (-.x, 0)]

let rec lagrange_f_i pts xi = match pts with
    [] -> pol_unit
    | (x, y) :: t ->
        begin
            if x = xi then
                lagrange_f_i t xi
            else
                mult (lagrange_factor x) (lagrange_f_i t xi)
        end

let lagrange_p_i pts (xi, yi) =
    let fi = lagrange_f_i pts xi in
    mult_const fi (yi /. (eval fi xi))

let interpolationLagrange pts =
    let rec aux = function
        [] -> pol_zero
        | h :: t -> add (lagrange_p_i pts h) (aux t)
    in aux pts

let interpolationVandermonde pts =
    let n = List.length pts in
    let x = Matrix.make_vect n 0. in
    let y = Matrix.make_matrix n 1 in
    let rec aux l m = match l with
        [] -> ()
        | (xi, yi) :: t ->
            begin
                x.(m) <- xi;
                y.(m).(0) <- yi;
                aux t (m+1)
            end in
    aux pts 0;
    let a = Matrix.make_matrix n n in
    Matrix.fill_matrix a (function (i, j) -> if j = 0 then 1. else a.(i).(j-1) *. x.(i));
    let x = Matrix.solve_system a y in
    let rec coeff k =
        if k >= 0 then
            (x.(k).(0), k) :: (coeff (k-1))
        else
            []
    in
        coeff (n-1)

let rec newton p x = function
    0 -> x
    | n -> newton p (x -. (eval p x) /. (eval (differentiate p) x)) (n-1)

let degree = function
    [] -> -1
    | (c, d) :: _ ->
        begin
            if d <> 0 then
                d
            else
                if Floats.is_zero c then
                    -1
                else
                    0
        end

let rec div a b =
    if (degree a < degree b) then
        (pol_zero, a)
    else
        begin
            match a, b with
                (ca, da) :: _, (cb, db) :: _ ->
                    begin
                        let q = [(ca/.cb, da-db)] in
                        let r = sub a (mult b q) in
                        let (qrec, rrec) = div r b in
                            ((add q qrec), rrec)
                    end
        end

let rec gcd a b =
    if (degree b) <= 0 then
        a
    else
        let (_, r) = div a b in
        gcd b r

let euclidean aa bb =
    let rec aux a b lastV =
        if (degree b) <= 0 then
            (a, pol_zero, lastV)
        else
            let (q, r) = div a b in
            let (p, u, v) = aux b r lastV in
            (b, v, sub u (mult v q))
    in aux aa bb pol_zero

let rec map f = function
    [] -> []
    | h :: t -> (f h) :: (map f t)

let rec nb_sign_change s x = match s with
    [] -> 0
    | h :: [] -> 0
    | h1 :: h2 :: t ->
        (
            let h1x = eval h1 x in
            let h2x = eval h2 x in
            if (h1x < 0. && h2x > 0.) || (h1x > 0. && h2x < 0.) then
                1
            else
                0
        ) + (nb_sign_change (h2 :: t) x)

let rec roots s inf sup =
    let sinf = nb_sign_change s inf in
    let ssup = nb_sign_change s sup in
    if (sinf = ssup) then
        []
    else
        let mid = (inf +. sup) /. 2. in
        if (Floats.is_zero (sup -. inf)) then
            [mid]
        else
            (roots s inf mid) @ (roots s mid sup)