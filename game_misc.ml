open Containers
open Types
open Game_types
open Shiptech

(*
let game_update_have_reserve_fuel g =
  Array.iter (fun perplayer ->
    let num = perplayer.eto.shipdesigns_num in
    Array.iteri (fun i sr ->
      let fuel =
        i < num && Array.exists
          (function Ship_special_reserve_fuel_tanks -> true | _ -> false)
          sr.design.special
        in
      sr.have_reserve_fuel <- fuel
    ) perplayer.srd.pership;
  ) g.perplayer
*)

let adjust_slider_group slider_arr slider_idx value =
  (* Find how much we have left to play with *)
  let left, first_unlocked, last_unlocked =
    Array.foldi (fun (left, first_unlocked, last_unlocked) i x ->
      if x.locked then
        (left - x.value, first_unlocked, last_unlocked)
      else
        let first_unlocked = match first_unlocked with
          | None -> Some i
          | x -> x
        in
        left, first_unlocked, i)
      (100, None, 0)
      slider_arr
  in
  let value = min value left in
  slider_arr.(slider_idx) <- {slider_arr.(slider_idx) with value};
  (* Reduce some unlocked slider to left *)
  let left = left - value in
  let left, update_data =
    Array.foldi (fun ((left, idx) as acc) i x ->
      if i <> slider_idx && not x.locked then
        if x.value <= left then
          left - x.value, idx
        else (* dump all thats left *)
          0, (Some (left, i))
      else
        acc)
      (left, None)
      slider_arr
  in
  begin match update_data with
  | Some (value, i) ->
      slider_arr.(i) <- {slider_arr.(i) with value}
  | _ -> ()
  end;
  (* If we never found a place to use left,
   * dump it in an unlocked slider *)
  match first_unlocked with
  | Some first_unlocked when left > 0 ->
      let j =
        if slider_idx <> last_unlocked then last_unlocked
        else first_unlocked
      in
      let value = slider_arr.(j).value + left in
      slider_arr.(j) <- {slider_arr.(j) with value}
  | _ -> ()







  
        



