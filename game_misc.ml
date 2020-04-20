open Types
open Game_types
open Shiptech

let game_update_have_reserve_fuel g =
  Array.iter (fun perplayer ->
    let num = perplayer.eto.shipdesigns_num in
    Array.iteri (fun i sr ->
      let fuel =
        i < num && Array.exists
          (function Special_reserve_fuel_tanks -> true | _ -> false)
          sr.design.special
        in
      sr.have_reserve_fuel <- fuel
    ) perplayer.srd.pership;
  ) g.perplayer

