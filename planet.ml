open Containers
open Types

type planet_type =
  | Not_habitable | Radiated | Toxic
  | Inferno | Dead | Tundra
  | Barren | Minimal | Desert
  | Steppe | Arid | Ocean
  | Jungle | Terran | Gaia

type growth =
  | Hostile | NormalGrowth | Fertile | Gaia

type special =
  | Ultra_poor | Poor | NormalSpecial
  | Artifacts | Rich | Ultra_rich
  | Tech4x

type planet_slider =
  | Ship_slider | Def_slider | Ind_slider | Eco_slider | Tech_slider
  [@@deriving enum]

let slider_num = 5

type unrest =
  | No_unrest | Unrest | HMM2 | Rebellion | Resolved_unrest

type star_type =
  | Yellow | Red | Green
  | White | Blue | Neutron

type rocks =
  | No_rocks | Some_rocks | Many_rocks

type finished =
  | Fact_fin | Pop_max_fin | Soilatmos_finw
  | Stargate_fin | Shield_fin | Ship_fin
  | Terraf_fin | Governor_fin

type extras =
  | Governor_extras | Gov_spend_rest_ship
  | Gov_spend_rest_ind | Gov_boost_build
  | Gov_boost_prod

type inrange = NotInrange | InRange | InrangeReserveFuel

type t = {
  name: string;
  x: int;
  y: int;
  star_type: star_type;
  look: int; (* 0-6 *)
  frame: int; (* 0-49 *)
  rocks: rocks;
  growth: growth;
  special: special;
  ptype: planet_type;
  max_pop1: int; (* base size *)
  max_pop2: int; (* size adjusted by soil enrichment tech *)
  max_pop3: int; (* max pop *)
  battlebg: int; (* 0-4; 0: nebula *)
  infogfx: int; (* index to planets.lbx *)
  bc_to_ecoproj: int;
  bc_to_ship: int;
  bc_to_factory: int;
  reserve: int;
  waste: int;
  owner: Player.t;
  prev_owner: Player.t;
  claim: Player.t;
  pop: int;
  pop_prev: int;
  factories: int;
  mutable prod_after_maint: int;
  mutable total_prod: int;
  sliders: slider array; (* PLANET_SLIDER_NUM *)
  buildship: int; (* 0..Num_shipdesigns-1 or BUILDSHIP_STARGATE *)
  reloc: int; (* planet i to relocate produced ships or
                 planet's own index if no relocation *)
  missile_bases: int;
  target_bases: int;
  bc_to_bases: int;
  bc_upgrade_bases: int;
  have_stargate: bool;
  shield: int; (* 0, 10, 15, 20 *)
  bc_to_shield: int;
  trans_num: int;
  trans_dest: int;
  pop_tenths: int;
  explored: bool list; (* PLAYER_NUM *)
  within_srange: bool list; (* PLAYER_NUM *)
  within_fuel_range: inrange list; (* PLAYER_NUM *)
  pop_oper_fact: int;
  bc_to_refit: int;
  rebels: int;
  unrest: unrest;
  unrest_reported: bool;
  unrefuel: bool list; (* PLAYER NUM *)
  finished: bool list; (* FINIHED_NUM *)
  extras: bool list; (* PLANET_EXTRAS_NUM *)
  (* remaining vars only during game_turn_process *)
  inbound: int list; (* PLAYER_NUM *)
  total_inbound: int list; (* PLAYER_NUM *)
  artifact_looter: Player.t;
}

let update_slider planet slider f =
  let i = planet_slider_to_enum slider in
  planet.sliders.(i) <- f planet.sliders.(i)

let get_slider planet slider =
  let i = planet_slider_to_enum slider in
  planet.sliders.(i)

module type IntType_t = sig
  type t
  val to_int: t -> int
  val of_int: int -> t
  val compare: t -> t -> int
  val eq: t -> t -> bool
  val (=): t -> t -> bool
end

module Idx : IntType_t = struct
  type t = int
  let to_int x : int = x
  let of_int x : t = x
  let compare x y = (to_int x) - (to_int y)
  let eq x y = to_int x = to_int y
  let (=) = eq
  let (<>) x y = not @@ eq x y
end
