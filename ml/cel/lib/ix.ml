open Func

module type S = sig
  type t 
  val range     : t * t -> t list
  val inRange   : t * t -> t -> bool
  val index     : t * t -> t -> int
  val fromIndex : t * t -> int -> t
end

module Make (I : S) : sig
  include S
  val rangeSize : I.t * I.t -> int
end with type t = I.t = struct
  include I
  let rangeSize ((_, n) as r) = 1 + I.index r n
end
    
module Pair (I : S) (J : S) : S with type t = I.t * J.t = struct
  type t = I.t * J.t
  let range ((m1, m2), (n1, n2)) = Clist.ap (Clist.map Pair.pair (I.range (m1, n1))) (J.range (m2, n2))
  let inRange ((m1, m2), (n1, n2)) (i, j) = I.inRange (m1, n1) i && J.inRange (m2, n2) j
  let index ((m1, m2), (n1, n2)) (i, j) =
    let module J = Make (J) in
    let m = (m1, n1)
    and n = (m2, n2) in
    (I.index m i * J.rangeSize n) + J.index n j
  let fromIndex ((m1, m2), (n1, n2)) i =
    let module J = Make (J) in
    let m = (m1, n1)
    and n = (m2, n2) in
    let r = J.rangeSize n in
    Pair.bimap (I.fromIndex m) (J.fromIndex n) (i / r, i mod r)
end
