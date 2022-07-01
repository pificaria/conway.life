(* Conway's game of life. *)

open Unix

(* Utils *)
let int_of_bool b = if b then 1 else 0
let usleep f = ignore (Unix.select [] [] [] f)

(* Torus *)
module Torus : sig
  type 'a t = 'a array array
  val norm   : int -> int -> int
  val ele    : 'a t -> int -> int -> 'a
  val nhood  : 'a t -> int -> int -> 'a list
  val bounds : 'a t -> int * int
  val withRule : ('a list -> 'a) -> 'a t -> 'a t
end = struct
  type 'a t = 'a array array
  let norm a n  = if a >= 0 then a mod n else (n + (a mod n)) mod n
  let bounds t = (Array.length t, Array.length t.(0))
  let ele t i j =
    let (m, n) = bounds t in
    t.(norm i m).(norm j n)
  let nhood t i j = [ele t i j;
                     ele t i (j - 1);
                     ele t i (j + 1);
                     ele t (i + 1) j;
                     ele t (i - 1) j;
                     ele t (i + 1) (j + 1);
                     ele t (i + 1) (j - 1);
                     ele t (i - 1) (j + 1);
                     ele t (i - 1) (j - 1)]
  let withRule f t =
    let (m, n) = bounds t in
    Array.init m (fun i -> Array.init n (fun j -> f (nhood t i j)))
end

(* Life *)
module Life : sig
  type t
  val rule   : bool list -> bool
  val clear  : unit -> unit
  val print  : t -> unit
  val random : int -> int -> t
  val loop   : t -> int -> float -> unit
end = struct
  type t = bool Torus.t
  let rule l =
    match l with
    | []      -> failwith "Empty list"
    | x :: xs -> (
        let n = List.fold_left (fun a b -> a + int_of_bool b) 0 xs in
        (x && (n = 2 || n = 3)) || (not x && n = 3))
  let clear () =
    print_string "\x1b[2J\x1b[0;0f";
    print_string "\x1b[0;0f"
  let print t =
    let rec psi b = if b then print_string "· " else print_string "  "
    and eta t = Array.iter psi t; print_endline "" in
    Array.iter eta t
  let random m n =
    Random.init (int_of_float (Unix.time ()));
    Array.init m (fun _ -> Array.init n (fun _ -> Random.bool ()))
  let loop t n d =
    let rec phi t n =
      clear ();
      Printf.printf "%d-th generation\n" n;
      print t;
      usleep d;
      phi (Torus.withRule rule t) (n + 1) in
    phi t n
end

(* Main *)
let () =
  let m = 50
  and n = 80
  and d = 0.3 in
  Life.loop (Life.random m n) 0 d
