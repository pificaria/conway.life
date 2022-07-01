include List

type 'a t = 'a list

let join     = concat
let bind f a = join (map f a)
let ap f a   = bind (fun f -> map f a) f
let return a = [a]
