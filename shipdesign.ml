open Types

let ship_name_len = 12
let ship_look_per_hull = 6
let ship_look_per_banner = 24
let weapon_slot_num = 4
let special_slot_num = 3

type t = {
  name: string;
  cost: int;
  space: int;
  hull: ship_hull;
  look: int;
  wpnt: weapon array; (* weapon_slot_num *)
  wpnn: int array;
}
