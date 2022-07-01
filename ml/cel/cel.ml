open Lib

(* Some indexers. *)
module IntIx = Ix.Make (
  struct
    type t = int
    let range (m, n) =
      let rec go i = if i > n then [] else i :: go (i + 1) in
      go m
    let inRange (m, n) i   = i >= m && i <= n
    let index (m, n) i     = i - m
    let fromIndex (m, n) i = i + m 
  end)

module IntPairIx : Ix.S with type t = int * int = Ix.Pair (IntIx) (IntIx)
    
(* Comonad definitions. *)
module type Comonad = sig
  type 'a t
  val extract   : 'a t -> 'a
  val duplicate : 'a t -> 'a t t
  val extend    : ('a t -> 'b) -> 'a t -> 'b t
end

module type ComonadStore = sig
  type 'a t
  type s
  include Comonad with type 'a t := 'a t
  val pos  : 'a t -> s
  val peek : s -> 'a t -> 'a
end

(* Our actual store. *)
module PArray (I : Ix.S) : sig
  type 'a t
  type s = I.t
  val fromArray : I.t -> 'a IxArray.S (I).t -> 'a t
  val toArray   : 'a t -> 'a IxArray.S (I).t 
  include ComonadStore with type 'a t := 'a t with type s := I.t
end = struct
  module A  = IxArray.S (I)
  type 'a t = I.t * 'a A.t
  type s    = I.t 
  let extract (i, a)  = A.get i a
  let extend f (i, a) = (i, A.init (A.bounds a) (fun i -> f (i, a)))
  let duplicate a     = extend id a
  let pos (i, _)      = i
  let peek i (_, a)   = A.get i a
  let fromArray i a   = (i, a)
  let toArray (_, a)  = a
end

(* Automata *)
type ('m, 'a) action = 'a -> 'm -> 'a
type ('m, 's) rule   = ('m -> 's) -> 's

module Automata (W : ComonadStore) : sig
  val step : ('m, W.s) action -> ('m, 's) rule -> 's W.t -> 's
end = struct
  let step phi r w = r (fun x -> W.peek (phi (W.pos w) x) w)
end

(* Life *)
type plane = int * int

module Life (B : sig val m : int val n : int val d : float end) = struct
  module W = PArray (IntPairIx)
  module A = IxArray.S (IntPairIx)
  module C = Automata (W)
  open B
  let norm x n = if x >= 0 then x mod n else (n + (x mod n)) mod n
  let torusQ (i, j) (a, b) = (norm (i + a) (m + 1), norm (j + b) (n + 1))
  let life phi =
    let rec s = phi (0, 0)
    and nhood = [(1, -1); (1, 0); (1, 1); (0, -1); (0, 1); (-1, -1); (-1, 0); (-1, 1)]
    and psi b = if b then 1 else 0 in
    let n = List.fold_left (fun a b -> a + psi (phi b)) 0 nhood in
    (s && (n = 2 || n = 3)) || (not s && n = 3)
  let random () =
    Random.init (int_of_float (Unix.time ()));
    W.fromArray (0, 0) (A.init ((0, 0), (m, n)) (fun _ -> Random.bool ()))
  let print a =
    let rec phi b = if b then print_string "· " else print_string "  "
    and psi ((_, j) as p) =
      if j = 0 then print_endline "";
      phi (W.peek p a) in
    ignore (List.map psi (IntPairIx.range ((0, 0), (m, n))))
  let clear () =
    print_string "\x1b[2J\x1b[0;0f";
    print_string "\x1b[0;0f"
  let usleep () = ignore (Unix.select [] [] [] d)
  let loop a g =
    let phi = W.extend (C.step torusQ life) in
    let rec go a g =
      clear ();
      Printf.printf "%d-th generation\n" g;
      print a;
      usleep ();
      go (phi a) (g + 1) in
    go a g
end

let () =
  let module L = Life (struct let m = 50 let n = 80 let d = 0.3 end) in
  L.loop (L.random ()) 0
