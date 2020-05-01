open Types

let ship_name_len = 12
let ship_look_per_hull = 6
let ship_look_per_banner = 24
let weapon_slot_num = 4
let special_slot_num = 3

type weapon_info = {
  wtype: Shiptech.weapon;
  wnum: int;
}

type shipdesign = {
  name: string;
  cost: int;
  space: int;
  hull: Shiptech.hull;
  look: int;
  weapon_info: weapon_info list;
  engine: Shiptech.engine;
  engines: int;
  special: Shiptech.special list; (* special_slot_num *)
  shield: Shiptech.shield;
  jammer: Shiptech.jammer;
  comp: Shiptech.comp;
  armor: Shiptech.armor;
  man: int; (* maneuverability *)
  hp: int;
}

let mk_shipdesign name cost space hull look wpnt_l wpnn_l engine engines
  special_l shield jammer comp armor man hp =
    let weapon_info =
      List.map2 (fun wtype wnum -> {wtype; wnum}) wpnt_l wpnn_l
    in
    let special = special_l in
    {name; cost; space; hull; look; weapon_info; engine; engines;
    special; shield; jammer; comp; armor; man; hp}

let startship_num = 5

let starfleet_ships = [| 2; 0; 0; 0; 1; 0 |]

let tbl_startship = [|
    (* SCOUT *)
      mk_shipdesign "" 10 0 Hull_small 0
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 10
      [ Special_reserve_fuel_tanks; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
    (* FIGHTER *)
      mk_shipdesign "" 15 0 Hull_small 1
      [ Weapon_laser; Weapon_none; Weapon_none; Weapon_none ]
      [ 1; 0; 0; 0 ]
      Engine_retros 30
      [ Special_none; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
    (* DESTROYER *)
      mk_shipdesign "" 66 0 Hull_medium 6
      [ Weapon_nuclear_missile_2; Weapon_laser; Weapon_none; Weapon_none ]
      [ 1; 3; 0; 0 ]
      Engine_retros 115
      [ Special_none; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 18
    ;
    (* BOMBER *)
      mk_shipdesign "" 86 0 Hull_medium 7
      [ Weapon_nuclear_bomb; Weapon_laser; Weapon_none; Weapon_none ]
      [ 2; 2; 0; 0 ]
      Engine_retros 90
      [ Special_none; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 18
    ;
    (* COLONY SHIP *)
      mk_shipdesign "" 591 0 Hull_large 12
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 205
      [ Special_standard_colony_base; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 100
    ;
    (* (unused) *)
      mk_shipdesign "" 10 0 Hull_small 0
      [ Weapon_none; Weapon_none; Weapon_none; Weapon_none ]
      [ 0; 0; 0; 0 ]
      Engine_retros 10
      [ Special_none; Special_none; Special_none ]
      Shield_none Jammer_none Comp_none Armor_titanium
      0 3
    ;
|]

