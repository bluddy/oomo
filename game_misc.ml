open Containers
open Types
open Game_types
open Shiptech

let update_have_reserve_fuel g =
  iter_players g (fun player ->
    let eto = get_eto g player in
    update_research_pership eto (fun research_pership ->
      Array.map (fun sr ->
        let have_reserve_fuel = List.exists
            (function Special_reserve_fuel_tanks -> true | _ -> false)
            sr.design.special
        in
        {sr with have_reserve_fuel})
      research_pership)
  )

let update_maint_costs g =
  iter_players g (fun player ->
    let eto = get_eto g player in
    let ship_tbl = Array.make num_shipdesigns 0 in

    (* count all orbiting ships of each type *)
    Array.iter (fun planet_orbit ->
      Array.iteri (fun i num_ships ->
        ship_tbl.(i) <- ship_tbl.(i) + num_ships
      ) planet_orbit.ships)
    eto.orbit;

    (* add all enroute ships of this player *)
    Array.iter (fun (fleet:fleet_enroute) ->
      if Player.(fleet.owner = player) then begin
        Array.iteri (fun i n_ships -> ship_tbl.(i) <- ship_tbl.(i) + n_ships)
          fleet.ships
      end)
      g.enroute;

    (* Mutate shipcounts *)
    update_research_pership eto (fun rp_arr ->
      Array.map2 (fun rp shipcount -> {rp with shipcount})
        rp_arr ship_tbl);

    (* Compute total ship cost *)
    let total_cost =
      fold_research_pership eto ~init:0 (fun acc rp ->
        acc + rp.shipcount * rp.design.cost)
    in
    let total_cost = total_cost / 50
      |> min Num.num_max_ship_maint
    in
    let missile_bases, stargates =
      Array.fold_left (fun ((bases, sgates) as acc) (planet:Planet.t) ->
        if Player.(planet.owner = player) then
          let sgates =
            if planet.have_stargate then sgates + 1 else sgates
          in
          let bases = bases + planet.missile_bases in
          bases, sgates
        else acc)
      (0, 0)
      g.planets
    in
    let sg_cost = stargates * Num.num_stargate_maint in
    let ship_maint_bc = total_cost + sg_cost |>
      min Num.num_max_ship_maint
    in
    let bases_maint_bc = (missile_bases * Game_tech.get_base_cost g player) / 50 in

    update_eto g player (fun eto ->
      let costs =
        {eto.costs with ship_maint_bc; bases_maint_bc}
      in
      {eto with costs})
  )

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
  (* If we never found a place to use left, dump it in an unlocked slider *)
  match first_unlocked with
  | Some first_unlocked when left > 0 ->
      let j =
        if slider_idx <> last_unlocked then last_unlocked
        else first_unlocked
      in
      let value = slider_arr.(j).value + left in
      slider_arr.(j) <- {slider_arr.(j) with value}
  | _ -> ()












