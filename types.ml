
module Player = struct
  type t = int
end

type tech_field =
  | Tech_field_computer | Tech_field_construction | Tech_field_force_field
  | Tech_field_planetology | Tech_field_propulsion | Tech_field_weapon
  [@@deriving enum]

let tech_field_num = 6

type race =
  | Human | Mrrshan | Silicoid
  | Sakkra | Psilon | Alkari
  | Klackon | Bulrathi | Meklar
  | Darlok

type banner =
  | Blue | Green | Purple | Red | White | Yellow

type galaxy_size =
  | Small | Medium | Large | Huge

type difficulty =
  | Simple | Easy | Average | Hard | Impossible

type treaty =
  | No_treaty | Non_aggression | Alliance | War | Final_war

type spymode =
  | Spy_hide | Espionage | Sabotage

type trait1 =
  | Xenophobic | Ruthless | Aggressive
  | Erratic | Honorable | Pacifistic

type trait2 =
  | Diplomat | Militarist | Expansionist
  | Technologist | Industrialist | Ecologist

type game_event =
  | Event_none | Event_plague | Event_quake
  | Event_nova | Event_accident | Event_assassin
  | Event_virus | Event_comet | Event_pirates
  | Event_derelic | Event_rebellion | Event_crystal
  | Event_amoeba | Event_enviro | Event_rich
  | Event_support | Event_poor

type monster_id =
  | Crystal_monster | Amoeba_monster | Guardian_monster

type game_end_type =
  | End_none | End_lost_exile | End_won_good
  | End_final_war | End_won_tyrant | End_lost_funeral | End_quit

type governor_eco_mode =
  | Grow_before_def | Grow_before_last
  | Never_grow
  | Do_not_decrease | Do_not_touch
  | Buildup_mil | Buildup_full | Auto_tech

type techsource =
  | TS_research | TS_spy | TS_found | TS_AI_spy | TS_trade

let techsource_choose = TS_AI_spy

let ship_name_num = 12
let num_shipdesigns = 6
let buildship_stargate = num_shipdesigns

let tech_spy_max = 6
let tech_next_max = 12

let fleet_enroute_ai_max = 208
let fleet_enroute_max = 260

let transport_max = 100

let planets_max = 108
