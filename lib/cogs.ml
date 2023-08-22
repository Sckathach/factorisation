let even n = n mod 2 = 0

let rec pow a = function
    0 -> 1
    | 1 -> a
    | n -> let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)

let rec gcd n m =
    if m = 0 then
        n
    else
        gcd m (n mod m)

let ind_euler x =
    let rec aux a n =
        if a >= n then
            0
        else
            (
                if (gcd a n = 1) then
                    1
                else
                    0
            )
                + aux (a+1) n
    in aux 0 x

let rec range_backward = function
    0 -> []
    | n -> n :: (range_backward (n-1))

let range x =
    let rec aux n acc =
        if n = 0 then
            acc
        else
            aux (n-1) (n::acc)
    in aux x []