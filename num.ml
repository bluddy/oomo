open Containers
open Utils
open Types


let num_deterministic = true
let num_bt_turn_max = 50
let num_bt_wait_no_reload = false
let num_bt_precap_tohit = false
let num_bt_no_tohit_acc = false
let num_bt_oracle_fix = false
let num_stargate_cost = 3000
let num_stargate_maint = 100
let num_weapon_list_max = 30
let num_limit_ships = 32000
let num_limit_ships_all = 32000
let num_max_pop = 300
let num_max_factories = 2500
let num_max_inbound = 300
let num_atmos_cost = 200
let num_soil_cost = 150
let num_adv_soil_cost = 300
let num_adv_scan_range = 110
let num_pop_hp = 200
let num_fact_hp = 50
let num_max_bomb_dmg = 100000
let num_max_bio_dmg = 10000
let num_max_trans_dmg = 32000
let num_max_ship_maint = 32000
let num_max_tribute_bc = 32000
let num_event_roll = 512
let num_accident_chk_factories = false
let num_council_years = 25
let num_news_orion = false
let num_aud_ask_break_nap = false
let num_aud_bounty_give = false
let num_monster_rest_att = false
let num_orbital_weap_any = false
let num_orbital_weap_4 = false
let num_orbital_torpedo = false
let num_orbital_comp_fix = false
let num_combat_trans_fix = false
let num_stargate_redir_fix = false
let num_trans_redir_fix = false
let num_retreat_redir_fix = false
let num_first_tech_rp_fix = false
let num_waste_calc_fix = false
let num_waste_adjust_fix = false
let num_doom_stack_fix = true
let num_eco_slider_slack = 7
let num_reset_tform_to_max = true
let num_soil_rounding_fix = false
let num_hidden_child_labor = true

let num_tbl_hull_w = [| 1; 5; 25; 125 |]

let num_base_hp = [| 50; 75; 100; 125; 150; 175; 200 |]
let num_pshield_cost = [| 0; 500; 1000; 1500; 2000 |]

(* difficulty multipler: per difficulty *)
let num_tech_costmuld = [| 20; 25; 30; 35; 40 |]

let get_tech_costmuld difficulty =
  num_tech_costmuld.(difficulty_to_enum difficulty)

let num_tech_costmula = [| 20; 20; 20; 20; 20 |]

(* per race, per field *)
let num_tech_costmulr = [|
    [| 100; 100; 60; 80; 80; 100   |];
    [| 100; 125; 100; 100; 100; 60 |];
    [| 80; 125; 125; 125; 125; 125 |];
    [| 100; 100; 100; 60; 100; 100 |];
    [| 80; 80; 80; 80; 80; 80 |];
    [| 100; 100; 125; 100; 60; 100 |];
    [| 100; 60; 100; 100; 125; 100 |];
    [| 125; 80; 100; 100; 100; 80  |];
    [| 60; 100; 100; 125; 100; 100 |];
    [| 80; 100; 100; 100; 100; 100 |]
|]

let get_tech_costmulr race field =
  let race = race_to_enum race in
  let field = tech_field_to_enum field in
  num_tech_costmulr.(race).(field)


(* Different fields and 50 numbers for techs *)
let numt_cm = [| 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0 |]
let numt_cn = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
let numt_ff = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
let numt_pl = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0 |]
let numt_pr = [| 0; 0; 0; 4; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]
let numt_we = [| 0; 0; 0; 0; 4; 0; 0; 0; 4; 0; 0; 4; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 4; 0; 0; 4; 0; 0; 0; 0; 0; 0 |]


let numt_cn_silicoid =
  [| 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0; 0; 0 |]
let numt_pl_silicoid = [| 0; 0; 0; 1; 0; 1; 1; 0; 0; 1; 0; 0; 1; 1; 0; 1; 1; 0; 1; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 4; 0; 1; 0; 0; 0; 0; 0; 0; 0; 4; 0; 0; 0; 0; 0; 0; 0; 0 |]

(* per species *)
let num_ng_tech = [|
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn_silicoid; numt_ff; numt_pl_silicoid; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
  [| numt_cm; numt_cn; numt_ff; numt_pl; numt_pr; numt_we |];
|]

(* per race, per trait1 *)
let num_tbl_trait1 = [|
    [| 4; 4; 4; 4; 4; 4; 4; 5; 5; 3 |];
    [| 1; 1; 1; 1; 1; 1; 1; 2; 2; 0 |];
    [| 0; 0; 0; 0; 0; 0; 0; 2; 2; 3 |];
    [| 2; 2; 2; 2; 2; 2; 2; 3; 3; 1 |];
    [| 5; 5; 5; 5; 5; 5; 5; 4; 3; 3 |];
    [| 4; 4; 4; 4; 4; 4; 4; 5; 5; 3 |];
    [| 0; 0; 0; 0; 0; 0; 0; 2; 2; 1 |];
    [| 2; 2; 2; 2; 2; 2; 2; 3; 3; 1 |];
    [| 3; 3; 3; 3; 3; 3; 3; 2; 2; 0 |];
    [| 2; 2; 2; 2; 2; 2; 2; 1; 1; 0 |]
|]

(* per race; per trait2 *)
let num_tbl_trait2 = [|
    [| 0; 0; 0; 0; 1; 2; 3; 4; 4; 0; 3; 5 |];
    [| 0; 1; 1; 1; 1; 3; 2; 2; 3; 3; 4; 5 |];
    [| 0; 1; 2; 2; 2; 2; 3; 3; 3; 4; 4; 5 |];
    [| 0; 3; 1; 5; 5; 3; 4; 2; 2; 2; 2; 2 |];
    [| 0; 0; 3; 1; 2; 3; 3; 3; 3; 4; 4; 5 |];
    [| 0; 1; 1; 1; 2; 2; 2; 3; 4; 2; 3; 5 |];
    [| 0; 1; 1; 2; 2; 3; 3; 4; 5; 5; 5; 5 |];
    [| 0; 5; 5; 5; 5; 2; 2; 3; 3; 3; 4; 5 |];
    [| 0; 1; 2; 2; 3; 3; 3; 4; 4; 4; 4; 5 |];
    [| 0; 0; 3; 3; 1; 2; 0; 3; 3; 4; 4; 5 |]
|]

let num_tbl_tech_autoadj = [| 0; 25; 50; 75 |]
