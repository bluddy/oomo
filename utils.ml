
let rec map3 f xs ys zs = match xs, ys, zs with
  | x::xs, y::ys, z::zs -> (f x y z)::map3 f xs ys zs
  | _, _, _ -> []

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)
