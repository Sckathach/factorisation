exception DivByZero of string

let rec pow p a = function
    0 -> 1
    | 1 -> a mod p
    | n -> let b = pow p a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a) mod p

let inverse p = function
    0 -> raise (DivByZero "FUNCTION: inverse")
    | a -> pow p a (p-2)

let div p a = function
    0 -> raise (DivByZero "FUNCTION: div")
    | b -> a * (inverse p b) mod p
