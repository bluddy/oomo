module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let rec map3 f xs ys zs = match xs, ys, zs with
  | x::xs, y::ys, z::zs -> (f x y z)::map3 f xs ys zs
  | _, _, _ -> []

let array_foldi f ~init ~arr =
  Array.fold_left (fun (i, acc) x ->
    let acc' = f i acc x in
    (i+1, acc'))
    (0, init)
    arr
  |> snd

let random_n n = Random.int n

let random_1_n n = Random.int n + 1 (* BUG? should limit be n+1 *)
