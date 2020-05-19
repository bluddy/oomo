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
  let update_planet_total_prod _ (p:Planet.t) =
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
  let calc_total_prod_perplayer _ (p:Planet.t) =
    let i = Player.to_int p.owner in
    total_prod.(i) <- total_prod.(i) + p.total_prod
  in
  iter_planets g calc_total_prod_perplayer;

  (* Calculate production - maintenance per player *)
  let update_player_production player =
    let eto = get_eto g player in
    let m = eto.money in
    let total_prod = total_prod.(Player.to_int player) in
    let spysum = Array.fold_left (fun acc x -> acc + x.spying) 0 eto.others in
    let spying_maint_bc = total_prod * spysum / 1000 in
    let bonus = match eto.race with Human -> 25 | _ -> 0 in
    let total_trade_bc =
      Array.fold_left (fun acc x ->
        acc + ((x.trade_bc * (x.trade_percent + bonus)) / 100))
      0 eto.others
    in
    let actual_prod =
      (total_prod * (1000 - eto.security - spysum)) / 1000
      + m.total_trade_bc - m.ship_maint_bc - m.bases_maint_bc
      - (total_prod * eto.tax) / 1000
      |> max 1
    in
    let total_maint_bc = total_prod - actual_prod in
    let percent_prod_total_to_actual =
      if total_prod > 0 then actual_prod * 100 / total_prod else 0
    in
    eto.money <- {eto.money with
          total_production_bc=total_prod; spying_maint_bc; total_trade_bc;
          total_maint_bc; percent_prod_total_to_actual; }
    in
    iter_players g update_player_production;

    let update_planet_prod_after_maint _ (p:Planet.t) =
      let eto = get_eto g p.owner in
      let prod_after_maint =
        p.total_prod * eto.money.percent_prod_total_to_actual / 100
          |> max 0
      in
      p.prod_after_maint <- prod_after_maint
    in
    iter_planets g update_planet_prod_after_maint

let get_tech_prod prod slider_v race special =
  let open Planet in
  let v = prod * slider_v / 100 in
  let v = match race with Psilon -> v + (v/2) | _ -> v in
  let v = match special with
    | Artifacts -> v * 2
    | Tech4x -> v * 4
    | _ -> v
  in
  min v 0x7fff

let update_total_research g =
  let has_research g p =
    match g.events.plague, g.events.nova with
      | _, Some (_, planet, _, _) when Planet.Idx.(planet = p) -> false
      | Some (_, planet, _, _), _ when Planet.Idx.(planet = p) -> false
      | _ -> true
  in
  let open Planet in
  iter_planets g (fun i p ->
    if Player.(p.owner <> none) &&
       has_research g i then begin
         let eto = get_eto g p.owner in
         let new_r = get_tech_prod p.prod_after_maint
           (get_slider p Tech_slider) eto.race p.special
         in
         eto.money.total_research_bc <- eto.money.total_research_bc + new_r
       end
  )

let update_eco_on_waste g player force_adjust =
  (* Set the optimal slider point for eliminating waste
   * force_adjust: even if not needed
   *)
  let eto = get_eto g player in
  match eto.race with
  | Silicoid -> ()
  | _ ->
      let open Planet in
      iter_planets g (fun i p ->
        if Player.(p.owner = player) then begin
          (* Can only operate a certain number of factories *)
          let fact = p.factories in
          let v = eto.techu.colonist_oper_factories * p.pop in
          let fact = min v fact in

          let waste = fact * eto.techu.ind_waste_scale / 10 in
          let waste = (waste + p.waste) / eto.techu.have_eco_restoration_n in

          let prod = p.prod_after_maint in
          let prod = if prod <= 0 then 1000 else prod in
          let v = (waste * 100 + prod - 1) / prod
              |> set_range 0 100
          in
          (* Check if we should adjust slider *)
          let s_eco_v = Planet.(get_slider p Eco_slider) in
          let sliders =
            if s_eco_v < v || force_adjust then begin
              let sum = Array.fold_left (+) 0 p.sliders.v in
              let left = sum - v |> max 0 in
              let sum = sum - s_eco_v in
              if sum > 0 then begin
                let left2 =
                  fold_update (fun i left2 slider ->
                    match planet_slider_of_int i with
                    | Eco_slider -> left2, slider
                    | _ ->
                        let t = slider * left / sum in
                        left2 - t, t)
                  ~init:left
                  p.sliders.v
                in
                let max_i = array_max p.sliders.v in
                Planet.update_slider p (planet_slider_of_int max_i) ((+) left2)
              end
            end
          in
          update_slider p Ship_slider (max 0);
          update_slider p Def_slider (max 0);
          update_slider p Ind_slider (max 0);
          update_slider p Eco_slider (fun x -> set_range x 0 100);
          let left = 100 -
            get_slider p Ship_slider -
            get_slider p Def_slider -
            get_slider p Ind_slider -
            get_slider p Eco_slider
            |> max 0
          in
          update_slider p Tech_slider (fun _ -> left)
        end
      )



let adjust_slider_group sliders slider_idx value =
  (* Find how much we have left to play with *)
  let left, first_unlocked, last_unlocked =
    Array.foldi (fun (left, first_unlocked, last_unlocked) i x ->
      if sliders.locks.(i) then
        (left - x, first_unlocked, last_unlocked)
      else
        let first_unlocked = match first_unlocked with
          | None -> Some i
          | x -> x
        in
        left, first_unlocked, i)
      (100, None, 0)
      sliders.v
  in
  let value = min value left in
  sliders.v.(slider_idx) <- value;

  (* Reduce some unlocked slider to left *)
  let left = left - value in
  let left, update_data =
    Array.foldi (fun ((left, idx) as acc) i x ->
      if i <> slider_idx && not sliders.locks.(i) then
        if x <= left then
          left - x, idx
        else (* dump all thats left *)
          0, (Some (left, i))
      else
        acc)
      (left, None)
      sliders.v
  in
  begin match update_data with
  | Some (value, i) -> sliders.v.(i) <- value
  | _ -> ()
  end;
  (* If we never found a place to use left, dump it in an unlocked slider *)
  match first_unlocked with
  | Some first_unlocked when left > 0 ->
      let j =
        if slider_idx <> last_unlocked then last_unlocked
        else first_unlocked
      in
      let value = sliders.v.(j) + left in
      sliders.v.(j) <- value
  | _ -> ()












