open Containers
open Types
open Game_types
open Shiptech
open Utils

(* Forward declaration *)
let get_base_cost =
  ref (fun _ _ -> assert false : t -> Player.t -> int)

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

(* Mutates ship costs and ship counts *)
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
    let bases_maint_bc = (missile_bases * !get_base_cost g player) / 50
    in
    eto.money <- {eto.money with ship_maint_bc; bases_maint_bc})

let update_production g =
  update_maint_costs g;

  (* Calculate per-planet production *)
  let update_planet_total_prod (p:Planet.t) =
    if Player.(p.owner = none) then ()
    else begin
      let eto = get_eto g p.owner in
      (* WASBUG changed to account for leaving transports *)
      let popx, rebels =
        (* Check if we transferred. See send_transport *)
        if p.trans_num > 0 then
          (p.pop - p.trans_num |> max 1, 
            p.rebels - (p.trans_num / 2 + 1) |> max 0)
        else
          p.pop, p.rebels
      in
      let v =
        (* how many factories can be operated *)
        (popx - rebels) * eto.techu.colonist_oper_factories
        |> max 0
      in
      let factories = p.factories |> min v in
      let techdata = get_techdata eto Tech_field_planetology in
      let extra = (popx * techdata.percent * 3 + 50) / 100 in
      let extra = match eto.race with
        | Klackon -> extra * 2 | _ -> extra
      in
      let v = factories + extra in
      (* AI is given a chance to cheat with production *)
      let v = 
        if is_ai g p.owner then Ai.production_boost g p.owner v
        else v
      in
      (* Reserve is at most = to production value *)
      let reserve = p.reserve |> min v in
      let v = v + reserve in
      let v = (match p.unrest with
        | Planet.Rebellion -> 0 | _ -> v)
        |> max 0
      in
      p.total_prod <- v
    end
  in
  iter_planets g update_planet_total_prod;

  (* Calculate total production per player *)
  let total_prod = Array.make g.players 0 in
  let calc_total_prod_perplayer (p:Planet.t) =
    let i = Player.to_int p.owner in
    total_prod.(i) <- total_prod.(i) + p.total_prod
  in
  iter_planets g calc_total_prod_perplayer;

  (* Calculate production - maintenance per player *)
  let update_player_production player =
    let eto = get_eto g player in
    let m = eto.money in
    let total_production_bc = total_prod.(Player.to_int player) in
    let spysum = Array.fold_left (fun acc x -> acc + x.spying) 0 eto.others in
    let spying_maint_bc = total_production_bc * spysum / 1000 in
    let bonus = match eto.race with Human -> 25 | _ -> 0 in
    let total_trade_bc =
      Array.fold_left (fun acc x ->
        acc + ((x.trade_bc * (x.trade_percent + bonus)) / 100))
      0 eto.others
    in
    let actual_prod =
      (total_production_bc * (1000 - eto.security - spysum)) / 1000
      + m.total_trade_bc - m.ship_maint_bc - m.bases_maint_bc
      - (total_production_bc * eto.tax) / 1000
      |> max 1
    in
    let total_maint_bc = total_production_bc - actual_prod in
    let percent_prod_total_to_actual =
      if total_production_bc > 0 then actual_prod * 100 / total_production_bc else 0
    in
    eto.money <- {eto.money with
          total_production_bc; spying_maint_bc; total_trade_bc;
          total_maint_bc; percent_prod_total_to_actual; }
    in
    iter_players g update_player_production;

    let update_planet_prod_after_maint (p:Planet.t) =
      let eto = get_eto g p.owner in
      let prod_after_maint =
        p.total_prod * eto.money.percent_prod_total_to_actual / 100
          |> max 0
      in
      p.prod_after_maint <- prod_after_maint
    in
    iter_planets g update_planet_prod_after_maint
      

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












