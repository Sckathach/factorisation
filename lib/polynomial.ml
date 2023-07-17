module type Polynomial = sig
    exception BadType of string
    val g : int -> int
end

module Polynomial : Polynomial = struct
    exception BadType of string
    let g x = x + 1
end