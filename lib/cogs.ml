module type Cogs = sig
    exception BadType of string
    val f : int -> int
end

module Cogs : Cogs = struct
    exception BadType of string
    let g x = x + 2
    let f x = g (x + 2)
end