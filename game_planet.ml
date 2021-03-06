open Containers
open Types
open Planet

let tenths_to_str num =
  let s = Printf.sprintf "%d" (num / 10) in
  let mod_n = num mod 10 in
  if num < 100 && mod_n > 0 then
    s ^ (Printf.sprintf ".%d" mod_n)
  else
    s

let increment_slider planet slider =
  update_slider planet slider (fun s ->
    add_slider ~lower:1 ~upper:100 s 1);
  let v = get_slider planet slider in
  Game_misc.adjust_slider_group  planet.sliders (planet_slider_to_enum slider) v

let set_slider planet slider value =
  update_slider planet slider (fun _ -> value);
  Game_misc.adjust_slider_group  planet.sliders (planet_slider_to_enum slider) value



