open Containers
open Utils
open Types
open Game_types

let tech_reduce_50percent_per_10pts = [|
  100; 93; 87; 81; 76; 71; 66; 62; 58; 54;
  50; 47; 44; 41; 38; 35; 33; 31; 29; 27;
  25; 23; 22; 20; 19; 18; 16; 15; 14; 13;
  13; 12; 11; 10; 9; 9; 8; 8; 7; 7;
  6; 6; 5; 5; 5; 4; 4; 4; 4; 3;
  3
|]

let tech_reduce_25percent_per_10pts = [|
  100; 97; 94; 92; 89; 87; 84; 82; 79; 77;
  75; 73; 71; 69; 67; 65; 63; 61; 60; 58;
  56; 55; 53; 52; 50; 49; 47; 46; 45; 43;
  42; 41; 40; 39; 38; 37; 36; 35; 34; 33;
  32; 31; 30; 29; 28; 27; 27; 26; 25; 24;
  24
|]

let get_tech_reduce_50 percent (* 1..100 *) =
  let percent = if percent > 50 then 50 else percent in
  tech_reduce_50percent_per_10pts.(percent - 1)

let player_has_tech g field tech player =
  let srd = get_srd g player in
  let research = research_completed_of_field srd field in
  IntSet.mem tech research

let get_base_cost_mod_armor g player percent =
  let idx =
    Shiptech.armor_foldi ~init:0
      (fun i acc ship_armor ->
        if player_has_tech g Tech_field_construction ship_armor.tech_i player
        then i else acc)
  in
  let idx = if idx > 0 then idx - 1 else 0 in
  let mult = get_tech_reduce_50 percent in
  let armor = Shiptech.tbl_armor.(idx) in
  let cost =
    ((Shiptech.get_armor_hull armor Ship_hull_large).cost +
    (Shiptech.hull_get Ship_hull_large).cost * mult) / 1500
  in
  cost

let get_base_cost_mod_weap g tech_i percent =
  let mult = get_tech_reduce_50 percent * 9 in
  (Shiptech.tbl_weap.(tech_i).cost * mult) / 1000

let get_base_cost_mod_shield g tech_i percent =
  let mult = get_tech_reduce_50 percent in
  let shield = Shiptech.tbl_shield.(tech_i) in
  let hull_data = Shiptech.get_shield_hull shield Ship_hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10


