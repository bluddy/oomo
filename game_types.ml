open Containers
open Utils
open Types
open Shipdesign

type ai_t = Classic | ClassicPlus | Muxer | Stub

let game_ai_default = ClassicPlus

type fleet_enroute = {
  owner: Player.t;
  x: int;
  y: int;
  dest: int; (* planet index *)
  speed: int;
  retreat: bool;
  visible: bool list; (* PLAYER_NUM *)
  ships: int array; (* NUM_SHIPDESIGNS *)
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
  visible: bool array; (* PLAYER_NUM *)
  ships: int array; (* NUM_SHIPDESIGNS *)
}

type techdata = {
  percent: int; (* tech level % *)
  investment: int;
  project: Tech.t; (* current project *)
  cost: int;
  research_list: Tech.t list array; (* tech_tier_num * 3 *)
  research_completed: TechSet.t; (* different from C: we use a set *)
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

type eto_other_player = {
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
  au_tech_trade_field: tech_field array; (* TECH_SPY_MAX *)
  au_tech_trade_tech: int array; (* TECH_SPY_MAX *)
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

(* Data derived from techs *)
type eto_tech_util = {
  have_colony_for: Planet.planet_type;
  have_adv_soil_enrich: bool;
  have_atmos_terra: bool;
  have_soil_enrich: bool;
  inc_pop_cost: int; (* cost of adding 1 pop *)
  have_terraform_n: int; (* 0,10,...120 *)
  terraform_cost_per_inc: int; (* 5..2 *)
  have_combat_transporter: bool;
  have_eco_restoration_n: int; (* 2,3,5,10,20 *)
  scanner_range: int; (* 3,5,7,9 *)
  have_stargates: bool;
  have_hyperspace_comm: bool;
  have_ia_scanner: bool;
  have_adv_scanner: bool;
  colonist_oper_factories: int; (* 2.. *)
  factory_cost: int; (* 10..2 *)
  factory_adj_cost: int;
  ind_waste_scale: int; (* 0, 2, ... 10 *)
  fuel_range: int; (* 3..10, 30 *)
  have_planet_shield: int; (* 0,5,10,15,20 *)
  planet_shield_cost: int;
  have_engine: int; (* 1.. *)
  have_sub_space_int: bool;
  antidote: int;
}

type eto_money = {
  total_trade_bc: int;
  ship_maint_bc: int;
  bases_maint_bc: int;
  spying_maint_bc: int;
  mutable total_research_bc: int;
  total_production_bc: int;
  reserve_bc: int;
  total_maint_bc: int;
  percent_prod_total_to_actual: int;
}

type empire_tech_orbit = {
  race: race;
  banner: banner;
  trait1: trait1;
  trait2: trait2;
  ai_p3_countdown: int;
  ai_p2_countdown: int;
  others: eto_other_player array;
  security:int; (* tenths *)
  tax: int;
  base_shield: Shiptech.shield;
  base_comp: Shiptech.comp;
  base_weapon: Shiptech.weapon;
  tech: techdata array; (* NUM_FIELDS *)
  tech_sliders: slider array; (* NUM_FIELDS *)
  shipdesigns_num: int;
  orbit: fleet_orbit array; (* PLANETS_MAX *)
  shipi_colony: int;
  shipi_bomber: int;
  mutable research_pership: ship_research_pership array; (* moved from srd *)
  mutable techu: eto_tech_util;
  mutable money: eto_money;
}

let update_research_pership eto f =
  eto.research_pership <- f eto.research_pership

let fold_research_pership eto ~init f = Array.fold_left f init eto.research_pership

let get_techdata eto field = eto.tech.(tech_field_to_enum field)
let iter_techdata eto f =
  Array.iteri (fun i x ->
    f (tech_field_of_enum i |> Option.get_exn) x)
  eto.tech
let fold_techdata eto f ~init = Array.foldi (fun acc i x ->
    f acc (tech_field_of_enum i |> Option.get_exn) x) init eto.tech
let map_techdata eto f = Array.mapi (fun i x ->
    f (tech_field_of_enum i |> Option.get_exn) x) eto.tech
let get_techslider eto field = eto.tech_sliders.(tech_field_to_enum field)
let update_techslider eto field f =
  let i = tech_field_to_enum field in
  eto.tech_sliders.(i) <- f (eto.tech_sliders.(i))
let add_techslider ?lower ?upper eto field i =
  update_techslider eto field (fun slider ->
    add_slider ?lower ?upper slider i)
let update_techsliders eto f =
  iter_techdata eto (fun i _ ->
    update_techslider eto i (fun slider -> f i slider))

let update_techdata eto field f =
  let tech = eto.tech in
  let tech_n = tech_field_to_enum field in
  tech.(tech_n) <- f (tech.(tech_n))

let get_research_completed eto field =
  (get_techdata eto field).research_completed


(* side effect: update srd perfield *)
let update_research_completed eto field f =
  let idx = tech_field_to_enum field in
  let perfield = eto.tech.(idx) in
  let research_completed = f perfield.research_completed in
  eto.tech.(idx) <- { perfield with research_completed }

let research_list_of_field eto field =
  eto.tech.(tech_field_to_enum field).research_list

let get_eto_contact_idxs eto =
  (* Return list of contact idxs *)
    Array.fold_left (fun (i, acc) x ->
      if x.contact then (i+1, (Player.of_int i)::acc)
      else (i+1, acc))
    (0, [])
    eto.others
    |> snd

let get_techdata eto field = eto.tech.(tech_field_to_enum field)

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
  tech: Tech.t;
  source: techsource;
  v06: int;   (* multi-use. Planet index, race index *)
  stolen_from: Player.t;
  frame: bool;
  other1: Player.t;
  other2: Player.t;
}

let newtech_v06_orion = planets_max

let game_event_tbl_num = 20
let help_shown_num = 16

type events_perplayer = {
  home: int; (* planet index *)
  coup: bool;
  newtech_num: int;
  newtechs: newtech Vector.vector; (* newtech_max (15) *)
  nexttechs: Tech.t array array; (* tech_field_num (6) * tech_next_max (12) *)
  new_ships: int array; (* num_shipdesigns *)
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

let get_nexttech events_pp field =
  events_pp.nexttechs.(tech_field_to_enum field)

let update_nexttechs events_pp field f =
  let nts = events_pp.nexttechs in
  let i = tech_field_to_enum field in
  nts.(i) <- f (nts.(i))

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

type comet = { have: int; years: int; hp: int; dmg: int; }

type events = {
  perplayer: events_perplayer array;
  perpair: events_pair array array;
  year: int;
  gdone: bool list; (* game_event_tbl_num *)
  diplo_msg_subtype: int; (* -1..13 *)
  plague: (Player.t * Planet.Idx.t * int * int) option; (* have: 0..3, val: int *)
  quake: (Player.t * Planet.Idx.t) option;
  nova: (Player.t * Planet.Idx.t * int * int) option; (* years, val *)
  accident: (Player.t * Planet.Idx.t * int) option; (* 0.. 2 *)
  assassin: (Player.t * Player.t) option;
  virus: (Player.t * tech_field) option;
  comet: (Player.t * Planet.Idx.t * comet) option; (* have, years, hp, dmg *)
  pirates: (Planet.t * int * int) option; (* 0..3, hp *)
  derelict: Player.t option;
  crystal: monster option;
  amoeba: monster option;
  enviro: Planet.Idx.t option;
  rich: Planet.Idx.t option;
  support: Player.t option;
  poor: Planet.Idx.t option;
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
  mutable eto: empire_tech_orbit;
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
    ai_id: ai_t;
    active_player: Player.t;
    difficulty: difficulty;
    galaxy_size: galaxy_size;
    galaxy_w: int; (* 6 8 a c *)
    galaxy_h: int; (* 4 6 7 9 *)
    galaxy_stars: int; (* w*h *)
    galaxy_maxx: int;
    galaxy_maxy: int;
    seed: Random.State.t; (* was int *)
    galaxy_seed: int; (* seed of galaxy *)
    gend: game_end_type;
    winner: Player.t;
    guardian_killer: Player.t;
    election_held: bool;
    nebula_num: int; (* 0..4 *)
    nebula_info: nebula_info array; (* NEBULA_MAX *)
    planets: Planet.t array; (* planets_max *)
    enroute: fleet_enroute array; (* fleet_enroute_max: ships moving *)
    transport: transport array; (* transport_max *)
    events: events;
}

let fold_perplayer g f ~init = Array.foldi (fun acc i x -> f acc (Player.of_int i) x) init g.perplayer
let iter_perplayer g f = Array.iteri (fun i x -> f (Player.of_int i) x) g.perplayer
let get_eto g player = g.perplayer.(Player.to_int player).eto

let get_events_perplayer g player = g.events.perplayer.(Player.to_int player)
let get_t_perplayer g player = g.perplayer.(Player.to_int player)
let is_ai g player = (get_t_perplayer g player).is_ai
let is_human g player = not @@ is_ai g player
let iter_players g f =
  for i=0 to g.players - 1 do
    f (Player.of_int i)
  done
let iter_fields f =
  for i=0 to tech_field_num - 1 do
    f (tech_field_of_int i)
  done
let iter_planets g f = Array.iteri
  (fun i planet -> f (Planet.Idx.of_int i) planet)
  g.planets

