exception BadType of string

type polynomial = (int * int) list

let rec add (p : polynomial) (q : polynomial) : polynomial = match p, q with
[], _ -> q
| _, [] -> p
| (a, b) :: x, (c, d) :: y ->
    if b = d then
        let s = a + c in
            if s = 0 then
                add x y
            else
                (s, b) :: add x y
    else if b > d then
        (a, b) :: add x q
    else
        (c, d) :: add p y

let to_string (p : polynomial) : string =
    let rec aux = function
        [] -> ""
        | (a, 0) :: q -> (Printf.sprintf "+ %d " a) ^ (aux q)
        | (a, 1) :: q -> (Printf.sprintf "+ %dX " a) ^ (aux q)
        | (a, b) :: q -> (Printf.sprintf "+ %dX^%d " a b) ^ (aux q)
    in
        let s = aux p in
            if String.length s >= 2 then
                String.sub s 2 (String.length s - 2)
            else
                ""