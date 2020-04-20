open Types
open Game_types

let rnd_get_new_seed () =
  let t = Unix.time () in
  int_of_float @@ t *. 1000.

let game_start g =
  if g.seed = 0 then begin
    let g' = {g with seed = rnd_get_new_seed ()} in
    Printf.printf "Game: seed was 0, got new seed 0x%x\n" (g.seed);
    g'
    end
  else
    g

let game_update_production v =
  0



