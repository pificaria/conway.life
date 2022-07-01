open Func

type ('a, 'b) t      = 'a * 'b
let pair a b         = (a, b)
let bimap f g (a, b) = (f a, g b)
let lmap  f          = bimap f id
let rmap  g          = bimap id g
