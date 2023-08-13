let tests = [
    (Cogs.tests, "Cogs");
    (Galois.tests, "Galois");
    (Matrix.tests, "Matrix");
    (Polynomial.tests, "Polynomial")
]

let rec test_category c b i = function
    [] ->
        if b then
            begin
                print_endline ("-> Finished " ^ c ^ " : OK");
                true
            end
        else
            begin
                print_endline ("-> Finished " ^ c ^ " : FAIL");
                false
            end
    | x :: q ->
        begin
            match x i with
                y, z -> print_endline y; test_category c (z && b) (i + 1) q
        end

let () =
    let rec run b = function
        [] -> b
        | (x, y) :: q ->
            begin
                print_endline ("-> Testing " ^ y ^ "...");
                if (test_category y true 0 x) then
                    run b q
                else
                    run false q
            end
    in
        print_endline "--- STARTING TESTS ---";
        if (run true tests) then
            print_endline "--- SUCCESS ---"
        else
            print_endline "--- FAIL ---"