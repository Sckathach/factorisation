open Lib

let p = [(1., 3); (2., 1); (1., 0)]
let q = [(1., 2); (-4., 0)]

let array_of_pol p =
    let n = Polynomial.degree p in
    let a = Array.make (n+1) 0. in
    let rec aux = function
        [] -> ()
        | (x, y) :: q -> a.(n-y) <- x; aux q
    in
        aux p;
        a

let degree p = Array.length p - 1

let sylvester p q =
    let n = degree p in
    let m = degree q in
    let s = Matrix.make_matrix (n+m) (n+m) in
    for i = 0 to (m-1) do
        for j = i to (i+n) do
            s.(i).(j) <- p.(j-i)
        done
    done;
    for i = m to (m+n-1) do
        for j = (i-m) to i do
            s.(i).(j) <- q.(j-i+m)
        done
    done;
    s

let binomial n p =
    if p > n || n < 0 || p < 0 then
        0
    else
        let rec aux acc a b =
        if b <= p then
            aux ((acc * a) / b) (a - 1) (b + 1)
        else
            acc
    in aux 1 n 1;;

let pol_x_h p h =
    let n = degree p in
    let a = Array.make (n+1) 0. in
    for i = 0 to n do
        for k = i to n do
            a.(n-i) <- a.(n-i) +. p.(n-k) *. (float_of_int (binomial k i)) *. (Float.pow h (float_of_int (k-i)))
        done
    done;
    a

(* let resultant_p_p_h p h = *)
(*    let s = sylvester p (pol_x_h p h) in *)
