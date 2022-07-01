let id a           = a
let flip f a b     = f b a
let (@.) f g a     = f (g a)
let curry f (a, b) = f a b
let uncurry f a b  = f (a, b)
