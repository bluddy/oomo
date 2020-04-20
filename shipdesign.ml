open Types
open Shiptech

let ship_name_len = 12
let ship_look_per_hull = 6
let ship_look_per_banner = 24
let weapon_slot_num = 4
let special_slot_num = 3

type per_weaponslot = {
  wtype: weapon;
  wnum: int;
}

type shipdesign = {
  name: string;
  cost: int;
  space: int;
  hull: ship_hull;
  look: int;
  perslot: per_weaponslot array;
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
