open Types
open Game_types

let rnd_get_new_seed () =
  let t = Unix.time () in
  int_of_float @@ t *. 1000.

let game_start g =
  (* Take care of game seed *)
  g

let game_update_production v =
  0



