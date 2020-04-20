open Types
open Shiptech

let ship_name_len = 12
let ship_look_per_hull = 6
let ship_look_per_banner = 24
let weapon_slot_num = 4
let special_slot_num = 3

type weapon_info = {
  wtype: weapon;
  wnum: int;
}

type shipdesign = {
  name: string;
  cost: int;
  space: int;
  hull: ship_hull;
  look: int;
  weapon_info: weapon_info array;
  engine: ship_engine;
  engines: int;
  special: ship_special array; (* special_slot_num *)
  shield: ship_shield;
  jammer: ship_jammer;
  comp: ship_comp;
  armor: ship_armor;
  man: int; (* maneuverability *)
  hp: int;
}

let mk_shipdesign name cost space hull look wpnt_l wpnn_l engine engines
  special_l shield jammer comp armor man hp =
    let weapon_info =
      List.map2 (fun wtype wnum -> {wtype; wnum}) wpnt_l wpnn_l
      |> Array.of_list
    in
    let special = Array.of_list special_l in
    {name; cost; space; hull; look; weapon_info; engine; engines;
    special; shield; jammer; comp; armor; man; hp}

let startship_num = 5

let starfleet_ships = [| 2; 0; 0; 0; 1; 0 |]

let tbl_startship = [|
    (* SCOUT *)
      mk_shipdesign "" 10 0 Ship_hull_small 0
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 10
      [ Ship_special_reserve_fuel_tanks; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
    (* FIGHTER *)
      mk_shipdesign "" 15 0 Ship_hull_small 1
      [ Weapon_laser; Weapon_none; Weapon_none; Weapon_none ]
      [ 1; 0; 0; 0 ]
      Engine_retros 30
      [ Ship_special_none; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
    (* DESTROYER *)
      mk_shipdesign "" 66 0 Ship_hull_medium 6
      [ Weapon_nuclear_missile_2; Weapon_laser; Weapon_none; Weapon_none ]
      [ 1; 3; 0; 0 ]
      Engine_retros 115
      [ Ship_special_none; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 18
    ;
    (* BOMBER *)
      mk_shipdesign "" 86 0 Ship_hull_medium 7
      [ Weapon_nuclear_bomb; Weapon_laser; Weapon_none; Weapon_none ]
      [ 2; 2; 0; 0 ]
      Engine_retros 90
      [ Ship_special_none; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 18
    ;
    (* COLONY SHIP *)
      mk_shipdesign "" 591 0 Ship_hull_large 12
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 205
      [ Ship_special_standard_colony_base; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 100
    ;
    (* (unused) *)
      mk_shipdesign "" 10 0 Ship_hull_small 0
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 10
      [ Ship_special_none; Ship_special_none; Ship_special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
|]

