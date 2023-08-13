open Floats

exception Dimensions_mismatch
exception Singular_matrix

let make_vect = Array.make
let length_vect = Array.length

let make_matrix n p =
    let m = make_vect n [||] in
    for i = 0 to (n-1) do
        m.(i) <- make_vect p 0.;
    done;
    m

let nb_lines m = length_vect m
let nb_columns m = length_vect m.(0)

let fill_line m i f =
    let p = nb_columns m in
    for j = 0 to (p-1) do
        m.(i).(j) <- f j;
    done

let fill_matrix m f =
    let n = nb_lines m in
    for i = 0 to (n-1) do
        fill_line m i (function j -> f (i, j));
    done

let identity n =
    let m = make_matrix n n in
    fill_matrix m (function i,j -> if (i = j) then 1. else 0.);
    m

let copy_matrix m =
    let c = make_matrix (nb_lines m) (nb_columns m) in
    fill_matrix c (function i,j -> m.(i).(j));
    c

let integers n =
    let m = make_matrix 1 n in
    fill_matrix m (function _,j -> float_of_int (j+1));
    m

let string_of_matrix m =
    let str = ref "" in
    let n = nb_lines m in
    let p = nb_columns m in
    for i = 0 to (n-1) do
        for j = 0 to (p-1) do
            str := !str ^ " " ^ string_of_float m.(i).(j);
        done;
        str := !str ^ "\n"
    done;
    !str

let print_matrix m =
    print_string (string_of_matrix m)

let matrix_of_list l =
    let m = make_matrix 1 (List.length l) in
    let rec fill i l = match l with
        [] -> ()
        | h :: t -> m.(0).(i) <- h ; fill (i+1) t in
    fill 0 l;
    m

let list_of_vector v =
    let rec aux acc = function
        -1 -> acc
        | j -> aux (v.(j)::acc) (j-1) in
    aux [] (length_vect v - 1)

let list_of_matrix m =
    let rec aux acc m = function
        -1 -> acc
        | i -> aux ((list_of_vector m.(i))::acc) m (i-1)
    in aux [] m (nb_lines m - 1)

let transpose m =
    let n = nb_lines m in
    let p = nb_columns m in
    let t = make_matrix p n in
    fill_matrix t (function i,j -> m.(j).(i));
    t

let add_matrix a b =
    let la = nb_lines a in
    let lb = nb_lines b in
    let ca = nb_columns a in
    let cb = nb_columns b in
    if (la = lb && ca = cb) then
        let c = make_matrix la ca in
            fill_matrix c (function i,j -> a.(i).(j) +. b.(i).(j));
            c
    else
        raise Dimensions_mismatch

let equal a b =
    let c = ref true in
    let la = nb_lines a in
    let lb = nb_lines b in
    let ca = nb_columns a in
    let cb = nb_columns b in
    if (la = lb && ca = cb) then
        begin
            for i = 0 to (la-1) do
                for j = 0 to (ca-1) do
                    c := !c && (Floats.equal a.(i).(j) b.(i).(j))
                done
            done;
            !c
        end
    else
        false

let trace m =
    let n = nb_lines m in
    let p = nb_columns m in
    if n <> p then
        raise Dimensions_mismatch
    else
        let s = ref 0. in
        for i = 0 to (n-1) do
            s := !s +. m.(i).(i)
        done;
        !s

let mult_matrix_const m k =
    let n = nb_lines m in
    let p = nb_columns m in
    let c = make_matrix n p in
    fill_matrix c (function i,j -> k *. m.(i).(j));
    c

let mult_matrix a b =
    let la = nb_lines a in
    let lb = nb_lines b in
    let ca = nb_columns a in
    let cb = nb_columns b in
    if ca = lb then
        let c = make_matrix la cb in
        let fill_function (i, j) =
            (
                let total = ref 0. in
                for k = 0 to (ca-1) do
                    total := !total +. a.(i).(k) *. b.(k).(j)
                done;
                !total;
            ) in
        fill_matrix c fill_function;
        c;
    else
        raise Dimensions_mismatch

let rec exp_matrix m n =
    if n = 0 then
        identity (nb_columns m)
    else
        mult_matrix m (exp_matrix m (n-1))

let add_linear_combination m (lg, wg) (ld, wd) =
    fill_line m lg (function j -> m.(lg).(j) *. wg +. m.(ld).(j) *. wd)

let pivot m inv (a, b) =
    let n = nb_lines m in
    let pivot_value = m.(a).(b) in
    for i = (a+1) to (n-1) do
        let r = -. m.(i).(b) in
        add_linear_combination inv (i, pivot_value) (a, r);
        add_linear_combination m (i, pivot_value) (a, r);
    done

let find_pivot m i =
    let k = ref i in
    let p = nb_lines m in
    while (!k < p && m.(!k).(i) = 0.) do
        k := !k + 1;
    done;
    if !k = p then
        raise Singular_matrix;
    !k

let swap_items m (l1, c1) (l2, c2) =
    let tmp = m.(l1).(c1) in
    m.(l1).(c1) <- m.(l2).(c2);
    m.(l2).(c2) <- tmp

let swap_lines m i k =
    for j = 0 to (nb_columns m - 1) do
        swap_items m (i, j) (k, j);
    done

let iteration_pivot m inv i =
    let k = find_pivot m i in
    if i <> k then
        begin
            swap_lines m i k;
            swap_lines inv i k;
        end;
    pivot m inv (i, i)

let reduce_diagonal m inv =
    let n = nb_lines m in
    for i = 0 to (n-1) do
        let c = m.(i).(i) in
        fill_line m i (function j -> m.(i).(j) /. c);
        fill_line inv i (function j -> inv.(i).(j) /. c);
    done

let rec apply_function f x = function
    0 -> x
    | n -> apply_function f (f x) (n-1)

let rotation m =
    let n = nb_lines m in
    let p = nb_columns m in
    let t = make_matrix n p in
    fill_matrix t (function i,j -> m.(n-i-1).(p-j-1));
    t

let invert_matrix m =
    let n = nb_lines m in
    let reduce_and_rotate (a, b) =
        (
            for k = 0 to (n-2) do
                iteration_pivot a b k;
            done;
            (rotation a, rotation b)
        ) in
    let (m, inv) = apply_function reduce_and_rotate (copy_matrix m, identity n) 2 in
    reduce_diagonal m inv;
    inv

let solve_system a y =
    mult_matrix (invert_matrix a) y