open Printf

let test_message x y m i =
    if x = y then
        (sprintf "TEST %i : Pass" i, true)
    else
        (sprintf "TEST %i : Fail %s" i m, false)

let test x y i = test_message x y " " i