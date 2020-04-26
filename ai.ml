(* open Types *)
open Containers
open Utils
open Types
open Game_types


let new_game_init g player home = 0

let new_game_tech g player = 0

type ai_turn_p1 = int

type ai_turn_p2 = int

let best_speed g player = 0

let turn_p1_send_scout g turn_p1 player = 0

let turn_p1_front_find_planet g aiturn player x y = 0

let turn_p1_front g aiturn player = 0

let turn_p1_have_colony_ship g aiturn player = 0

let turn_fleet_send g aiturn player from dest = 0

let turn_p1_send_colony_ships g aiturn player = 0

let turn_p1_planet_w g aiturn player = 0

let turn_p1_send_attack g aiturn player = 0

let turn_p1_send_defense g aiturn player = 0

let turn_p1_send_idle g aiturn player = 0

let turn_p1_trans_en g aiturn player = 0

let turn_p1_trans_own g aiturn player = 0

let turn_p1_trans_own g aiturn player = 0

let turn_p1_build_defending_ships g player = 0

let turn_p1_tax g player = 0

let turn_p1 g player = 0

let design_scrap g player ship = 0

let design_ship_get_loog g player hull = 0

let design_ship_get_itemm g num chance = 0

let design_update_engines_space design = 0

let design_ship_base g aiturn player = 0

let design_ship_sub2 g aiturn player = 0

let design_ship_weapon g aitur player slot numshots_ignore c1 c2 = 0

let design_ship_weapons g aiturn player = 0

let design_ship g aiturn player = 0

let turn_p2_do g player = 0

let turn_p2 g player = 0

let turn_p3_sub1 g player = 0

let turn_p3 g player = 0

let production_boost g player prod = 0

(* The classic AI has the same tech costs as the human has on the easiest level.
   The cost for humans is: game_num_tech_costmuld[difficulty], and for classical
   AIs game_num_tech_costmula[g.difficulty]. *)
let tech_cost g _player =
  Num.get_tech_costmula g.difficulty

let base_cost g player cost = 0

let battle_ai_ai_get_weights g player tbl = 0

let battle_ai_ai_resolve_do battle = 0

let battle_ai_ai_resolve battle = 0

let battle_missile_dmg battle missile = 0

let battle_incoming_missiles_dmg battle item = 0

let battle_dmgmax battle item = 0

let battle_dmggive battle itemi1 itemi2 a2 = 0

let battle_rival battle itemi a2 = 0

let battle_missile_none_fired_by battle itemi = 0

let battle_stasis_target battle = 0

let battle_ai_missile_evade battle = 0

let get_possible_distance_increase battle target = 0

let battle_ai_best_range battle target = 0

let eval_pos_for_pulsar_use battle sx sy = 0

let battle_ai_target1_sub3 battle sx sy target bestrange = 0

let battle_ai_target1_sub4 battle = 0

let battle_ai_target1_sub5 battle = 0

let battle_ai_target1 battle target = 0

let battle_ai_turn battle = 0

let battle_ai_retreat battle = 0

let tech_next g player field techs num =
  if random_1_n 6 < difficulty_to_enum g.difficulty then
    techs.(num-1)
  else
    techs.(random_n num)

let bomb g player planet pop_inbound = 0

let ground g def_player att_player planet pop_killed owner_changed = 0

let add_reserve g planet = 0

let crank_tech g planet = 0

let crank_ship g planet = 0

let vote_w g player cand = 0

let vote_like g player cand = 0

let vote_hate g player c1 c2 = 0

let vote election player = 0

let diplo_wage_war_fleet_w game p1 p2 = 0

let diplo_wage_war_do g p1 p2 = 0

let diplo_wage_war_prod_w g p1 p2 = 0

let diplo_wage_war g p1 p2 = 0

let diplo_p1_sub1 g = 0

let turn_diplo_p1_get_ai_trade_tech g p1 p2 field tech = 0

let turn_diplo_p1 g = 0

let turn_diplo_p2_sub1 g p1 p2 = 0

let turn_diplo_p2_sub2 g p1 p2 = 0

let turn_diplo_p2_sub3 g p1 p2 = 0

let turn_diplo_p2 g = 0

let aud_check_mood audience a0 a2 = 0

let aud_start_human audience = 0

let aud_treaty_nap audience = 0

let aud_treaty_alliance audience = 0

let aud_treaty_peace audience = 0

let aud_treay_declare_war audience = 0

let aud_treaty_break_alliance audience = 0

let aud_trade audience = 0

let aud_sweeten audience bcptr field tech = 0

let aud_threaten audience = 0

let aud_tribute_bc audience selected bc = 0

let aud_tribute_tech audience selected field tech = 0

let aud_tech_scale audience = 0

let aud_get_dtype audience dtype a2 = 0

let aud_later audience = 0

