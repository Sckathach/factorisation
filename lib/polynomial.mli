module type Polynomial = sig

    (**
        Exception: BadType
            That's a fantastic expression !
    *)
    exception BadType of string

    (**
        Type: polynomial
            2X+1 -> [(2, 1); (1, 0)]
    *)
    type polynomial = (int * int) list

    (**
        Function: add
            @param p q
            @return p + q
            @raises BadType
    *)
    val add : polynomial -> polynomial -> polynomial
    (** wow so beautiful ! *)

    (**
        Function: to_string
            @param p
            @return a string
            @raises BadType
    *)
    val to_string : polynomial -> string
end
