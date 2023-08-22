exception BadType of string
type expr =
    Const of float
  | X
  | Sum of expr * expr
  | Mult of expr * expr
  | Pow of expr * int
type degree = int
type coefficient = float
type monomial = coefficient * degree
type polynomial = monomial list
val string_of_monomial : monomial -> string
val string_of_polynomial : monomial list -> string
val to_string : monomial list -> string
val add : polynomial -> polynomial -> polynomial
val mult_const : polynomial -> coefficient -> polynomial
val sub : polynomial -> polynomial -> polynomial
val mult_xk : polynomial -> degree -> polynomial
val mult_monomial : polynomial -> monomial -> polynomial
val mult : polynomial -> polynomial -> polynomial
val pow : polynomial -> degree -> polynomial
val pol_of_expr : expr -> polynomial
val eval : polynomial -> coefficient -> coefficient
val differentiate : polynomial -> polynomial
val primitive : polynomial -> polynomial
val integrate : polynomial -> coefficient -> coefficient -> coefficient
val trapezes : polynomial -> coefficient -> coefficient -> degree -> coefficient
val lagrange_factor : coefficient -> (coefficient * degree) list
val lagrange_f_i : (coefficient * 'a) list -> coefficient -> polynomial
val lagrange_p_i : (coefficient * 'a) list -> coefficient * coefficient -> polynomial
val interpolationLagrange : (coefficient * coefficient) list -> polynomial
val interpolationVandermonde : (coefficient * coefficient) list -> (coefficient * degree) list
val newton : polynomial -> coefficient -> degree -> coefficient
val degree : (coefficient * degree) list -> degree
val div : polynomial -> polynomial -> polynomial * polynomial
val gcd : polynomial -> polynomial -> polynomial
val euclidean : polynomial -> polynomial -> polynomial * polynomial * polynomial
val map : ('a -> 'b) -> 'a list -> 'b list
val nb_sign_change : polynomial list -> coefficient -> degree
val roots : polynomial list -> coefficient -> coefficient -> coefficient list