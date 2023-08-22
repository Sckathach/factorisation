exception Dimensions_mismatch
exception Singular_matrix

val make_vect : int -> 'a -> 'a array
val length_vect : 'a array -> int
val make_matrix : int -> int -> float array array
val nb_lines : 'a array -> int
val nb_columns : 'a array array -> int
val fill_line : 'a array array -> int -> (int -> 'a) -> unit
val fill_matrix : 'a array array -> (int * int -> 'a) -> unit
val identity : int -> float array array
val copy_matrix : float array array -> float array array
val integers : int -> float array array
val string_of_matrix : float array array -> string
val print_matrix : float array array -> unit
val matrix_of_list : float list -> float array array
val list_of_vector : 'a array -> 'a list
val list_of_matrix : 'a array array -> 'a list list
val transpose : float array array -> float array array
val add_matrix : float array array -> float array array -> float array array
val equal : float array array -> float array array -> bool
val trace : float array array -> float
val mult_matrix_const : float array array -> float -> float array array
val mult_matrix : float array array -> float array array -> float array array
val exp_matrix : float array array -> int -> float array array
val add_linear_combination : float array array -> int * float -> int * float -> unit
val pivot : float array array -> float array array -> int * int -> unit
val find_pivot : float array array -> int -> int
val swap_items : 'a array array -> int * int -> int * int -> unit
val swap_lines : 'a array array -> int -> int -> unit
val iteration_pivot : float array array -> float array array -> int -> unit
val reduce_diagonal : float array array -> float array array -> unit
val apply_function : ('a -> 'a) -> 'a -> int -> 'a
val rotation : float array array -> float array array
val invert_matrix : float array array -> float array array
val solve_system : float array array -> float array array -> float array array