open Containers

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let rec map3 f xs ys zs = match xs, ys, zs with
  | x::xs, y::ys, z::zs -> (f x y z)::map3 f xs ys zs
  | _, _, _ -> []

let random_n n seed = Random.int n seed

let random_1_n n seed = (Random.int n seed) + 1 (* BUG? should limit be n+1 *)

let set_range x min max =
  if x < min then min else if x > max then max else x

let min x y = if x < y then x else y
