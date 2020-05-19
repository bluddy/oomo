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

let id_fn x = x

let array_min ?(f=id_fn) a =
  Array.foldi (fun min_i i x -> if f x < f a.(min_i) then i else min_i) 0 a

let array_max ?(f=id_fn) a =
  Array.foldi (fun max_i i x -> if f x > f a.(max_i) then i else max_i) 0 a

exception Not_implemented

(* fold over an array and update in place *)
let fold_update f ~init arr =
  let rec loop i acc =
    if i >= Array.length arr then acc
    else begin
      let acc', v = f i acc arr.(i) in
      arr.(i) <- v;
      loop (i+1) acc'
    end
  in
  loop 0 init

  
