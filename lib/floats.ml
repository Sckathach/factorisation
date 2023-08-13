let epsilon = 0.001

let dist x y = Float.abs (x -. y)
let equal x y = (dist x y) < epsilon
let is_zero x = equal x 0.

