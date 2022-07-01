open Func

module S (I : Ix.S) : sig
  type 'a t
  val bounds : 'a t -> I.t * I.t
  val map    : ('a -> 'b) -> 'a t -> 'b t
  val get    : I.t -> 'a t -> 'a
  val init   : I.t * I.t -> (I.t -> 'a) -> 'a t
end = struct
  type 'a t = (I.t * I.t) * 'a array
  let bounds (r, _) = r
  let map f (r, a)  = (r, Array.map f a)
  let get i (r, a) = Array.get a (I.index r i)
  let init r f =
    let module I = Ix.Make (I) in
    let n = I.rangeSize r in
    (r, Array.init n (f @. I.fromIndex r))
end
