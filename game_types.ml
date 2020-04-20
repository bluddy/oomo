open Types
open Shipdesign

type fleet_enroute = {
  owner: Player.t;
  x: int;
  y: int;
  dest: int; (* planet index *)
  speed: int;
  retreat: bool;
  visible: bool list; (* PLAYER_NUM *)
  ships: int list; (* NUM_SHIPDESIGNS *)
}

type transport = {
  owner: Player.t;
  x: int;
  y: int;
  dest: int; (* planet index *)
  speed: int;
  visible: bool list; (* PLAYER_NUM *)
  pop: int;
}

type fleet_orbit = {
  visible: bool list; (* PLAYER_NUM *)
  ships: int list; (* NUM_SHIPDESIGNS *)
}

type techdata = {
  percent: int list; (* TECH_FIELD_NUM, tech level % *)
  slider: int list; (* TECH_FIELD_NUM *)
  slider_lock: bool list; (* TECH_FIELD_NUM *)
  investment: int list; (* TECH_FIELD_NUM *)
  project: int list; (* TECH_FIELD_NUM *)
  cost: int list; (* TECH_FIELD_NUM *)
  completed: int list; (* TECH_FIELD_NUM *) (* num completed projects *)
}

let tech_tier_num = 10
let tech_per_field = 60
let tech_max_level = 100

type ship_research_pership = {
  design: shipdesign;
  mutable have_reserve_fuel: bool;
  year: int;
  shipcount: int;
}

type ship_research = {
  research_list: int array; (* tech_field_num * tech_tier_num * 3 *)
  research_complete: int array; (* tech_field_num * tech_per_field *)
  pership: ship_research_pership array;
}

type empire_tech_orbit_perplayer = {
  contact: bool;
  contact_broken: bool;
  relation1: int;
  relation2: int;
  diplo_type: int;
  diplo_val: int;
  diplo_p1: int;
  diplo_p2: int;
  trust: int;
  broken_treaty: treaty;
  blunder: int;
  tribute_field: tech_field;
  tribute_tech: int;
  mood_treaty: int;
  mood_trade: int;
  mood_tech: int;
  mood_peace: int;
  treaty: treaty;
  trade_bc: int;
  trade_percent: int;
  spymode_next: spymode;
  au_want_trade: int;
  au_want_field: tech_field;
  au_want_tech: int;
  au_tech_trade_num: int;
  au_tech_trade_field: tech_field list; (* TECH_SPY_MAX *)
  au_tech_trade_tech: int list; (* TECH_SPY_MAX *)
  offer_field: tech_field;
  offer_tech: int;
  offer_bc: int;
  au_ally_attacker: Player.t;
  au_ask_break_treaty: Player.t;
  attack_bounty: Player.t;
  bounty_collect: Player.t;
  attack_gift_field: tech_field;
  attach_gift_tech: int;
  attach_gift_bc: int;
  hatred: int;
  have_met: int; (* 0,1,2 *)
  trade_established_bc: int;
  spying: int; (* tenths *)
  spyfund:int;
  spymode:spymode;
  spies: int;
  spyreportfield: int; (* TECH_FIELD_NUM *)
  spyreportyear: int;
}

type empire_tech_orbit = {
  race: race;
  banner: banner;
  trait1: trait1;
  trait2: trait2;
  ai_p3_countdown: int;
  ai_p2_countdown: int;
  perplayer: empire_tech_orbit_perplayer;
  have_planet_shield: int; (* 0,5,10,15,20 *)
  planet_shield_cost: int;
  security:int; (* tenths *)
  total_trade_bc: int;
  ship_maint_bc: int;
  bases_maint_bc: int;
  spying_maint_bc: int;
  percent_prod_total_to_actual: int;
  total_maint_bc: int;
  total_research_bc: int;
  total_production_bc: int;
  reserve_bc: int;
  tax: int;
  base_shield: int;
  base_comp: int;
  base_weapon: int;
  have_sub_space_int: bool;
  antidote: int;
  have_colony_for: Planet.planet_type;
  have_eco_restoration_n: int; (* 2,3,5,10,20 *)
  have_terraform_n: int; (* 0,10,...120 *)
  terraform_cost_per_inc: int; (* 5..2 *)
  have_adv_soil_enrich: bool;
  have_atmos_terra: bool;
  have_soil_enrich: bool;
  inc_pop_cost: int; (* cost of adding 1 pop *)
  scanner_range: int; (* 3,5,7,9 *)
  have_ia_scanner: bool;
  have_adv_scanner: bool;
  have_hyperspace_comm: bool;
  have_stargates: bool;
  colonist_oper_factories: int; (* 2.. *)
  factory_cost: int; (* 10..2 *)
  factory_adj_cost: int;
  ind_waste_scale: int; (* 0, 2, ... 10 *)
  fuel_range: int; (* 3..10, 30 *)
  have_combat_transporter: bool;
  tech: techdata;
  have_engine: int; (* 1.. *)
  shipdesigns_num: int;
  orbit: fleet_orbit list; (* PLANETS_MAX *)
  shipi_colony: int;
  shipi_bomber: int;
}

let newtech_max = 15

type monster = {
  exists: int; (* 0..3 *)
  x: int;
  y: int;
  killer: Player.t; (* MOO1: 0 or id+1 *)
  dest: int;
  counter: int;
  nuked: int;
}

type newtech = {
  field: tech_field;
  tech: int;
  source: techsource;
  v06: int;
  stolen_from: Player.t;
  frame: bool;
  other1: Player.t;
  other2: Player.t;
}

type nexttech = {
  num: int;
  tech: int list; (* TECH_NEXT_MAX *)
}

let newtech_v06_orion = planets_max

type newtechs = {
  num: int;
  d: newtech list; (* newtech_max *)
  next: nexttech list; (* tech_field_max *)
}

let game_event_tbl_num = 20
let help_shown_num = 16

type events_player = {
  home: int; (* planet index *)
  coup: bool;
  newtech: newtechs;
  new_ships: int list; (* num_shipdesigns *)
  build_finished_num: int;
  gov_eco_mode: governor_eco_mode;
  gov_no_stargates: bool;
  voted: Player.t;
  best_ecorestore: int;
  best_wastereduce: int;
  best_roboctrl: int;
  best_terraform: int;
  help_shown: bool array; (* help_shown_num *)
  msg_filter: bool array; (* finished_num *)
}

type events_pair = {
  spies_caught: int; (* catcher,spy *)
  spied_num: int; (* victim, spy *)
  spied_spy: int; (* victim, spy *)
  stolen_field: tech_field; (* victim, spy *)
  stolen_tech: int; (* victim, spy *)
  sabotage_is_bases: bool; (* victim, spy *)
  sabotage_planet: int; (* victim, spy *)
  sabotage_num: int; (* victim, spy *)
  sabotage_spy: Player.t; (* victim, spy *)
  ceasefire: Player.t; (* human, ai *)
}

type events = {
  perplayer: events_player list;
  perpair: events_pair array array;
  year: int;
  gdone: bool list; (* game_event_tbl_num *)
  diplo_msg_subtype: int; (* -1..13 *)
  have_plague: int; (* 0..3 *)
  plague_player: Player.t;
  plague_planet_i: int;
  plague_val: int;
  have_quake: bool;
  quake_player: Player.t;
  quake_planet_i: int;
  have_nova: int; (* 0..3 *)
  nova_player: Player.t;
  nova_planet_i: int;
  nova_years: int;
  nova_val: int;
  have_accident: int; (* 0..2 *)
  accident_player: Player.t;
  accident_planet_i: int;
  have_assassin: bool;
  assassin_player: Player.t;
  assassin_player2: Player.t;
  have_virus: bool;
  virus_player: Player.t;
  virus_field: tech_field;
  have_comet: int; (* 0..3 *)
  comet_player: Player.t;
  comet_planet_i: int;
  comet_years: int;
  comet_hp: int;
  comet_dmg: int;
  have_pirates: int; (* 0..3 *)
  pirates_planet_i: int;
  pirates_hp: int;
  have_derelict: bool;
  derelict_player: Player.t;
  crystal: monster;
  amoeba: monster;
  have_enviro: bool;
  enviro_planet_i: int;
  have_rich: bool;
  rich_planet_i: int;
  have_support: bool;
  support_player: Player.t;
  have_poor: bool;
  poor_planet_i: int;
  have_orion_conquer: int; (* 0, pi+1 *)
  planet_orion_i: int;
  have_guardian: bool;
  report_stars: int;
}

type seen = {
  owner: Player.t;
  pop: int;
  bases: int;
  factories: int;
}

let emperor_name_len = 15
let nebula_max = 4
let year_base = 2299


type per_player = {
  is_ai: bool;
  alive: bool;
  refuse: bool;
  planet_focus_i: int;
  emperor_name: string;
  seen: seen array; (* planets_max *)
  current_design: shipdesign;
  gaux: Game_aux.t;
  eto: empire_tech_orbit;
  srd: ship_research;
}

type locinfo = {
  x0: int; x1: int; y0: int; y1: int;
}

type nebula_info = {
  ntype: int; (* 0..9 *)
  x: int;
  y: int;
  locinfo: locinfo array; (* 4 *)
}

type t = {
    perplayer: per_player array;
    enroute_num: int;
    transport_num: int;
    year: int; (* init to 1 *)
    players: int;
    ai_id: Ai.t;
    active_player: Player.t;
    difficulty: difficulty;
    galaxy_size: galaxy_size;
    galaxy_w: int; (* 6 8 a c *)
    galaxy_h: int; (* 4 6 7 9 *)
    galaxy_stars: int; (* w*h *)
    galaxy_maxx: int;
    galaxy_maxy: int;
    seed: int; (* current seed *)
    galaxy_seed: int; (* seed of galaxy *)
    gend: game_end_type;
    winner: Player.t;
    guardian_killer: Player.t;
    election_held: bool;
    nebula_num: int; (* 0..4 *)
    nebula_info: nebula_info array; (* NEBULA_MAX *)
    planet: Planet.t array; (* planets_max *)
    enroute: fleet_enroute array; (* fleet_enroute_max *)
    transport: transport array; (* transport_max *)
    evn: events;
}
