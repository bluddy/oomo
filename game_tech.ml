open Containers
open Utils
open Types
open Game_types

let tech_reduce_50percent_per_10pts = [|
  100; 93; 87; 81; 76; 71; 66; 62; 58; 54;
  50; 47; 44; 41; 38; 35; 33; 31; 29; 27;
  25; 23; 22; 20; 19; 18; 16; 15; 14; 13;
  13; 12; 11; 10; 9; 9; 8; 8; 7; 7;
  6; 6; 5; 5; 5; 4; 4; 4; 4; 3;
  3
|]

let tech_reduce_25percent_per_10pts = [|
  100; 97; 94; 92; 89; 87; 84; 82; 79; 77;
  75; 73; 71; 69; 67; 65; 63; 61; 60; 58;
  56; 55; 53; 52; 50; 49; 47; 46; 45; 43;
  42; 41; 40; 39; 38; 37; 36; 35; 34; 33;
  32; 31; 30; 29; 28; 27; 27; 26; 25; 24;
  24
|]

let get_tech_reduce_50 percent (* 1..100 *) =
  let percent = if percent > 50 then 50 else percent in
  tech_reduce_50percent_per_10pts.(percent - 1)

let player_has_tech g field tech player =
  let eto = get_eto g player in
  let research = get_research_completed eto field in
  TechSet.mem tech research

let get_best_armor g player tech =
  let open Shiptech in
  fold_armor (fun acc i armor ->
    if Tech.(armor.tech <= tech) &&
       player_has_tech g Tech_field_construction armor.tech player
    then i else acc)
  ~init:Armor_titanium

let get_base_cost_mod_armor g player percent =
  let open Shiptech in
  let idx = get_best_armor g player Tech.max in
  let idx = match idx with
    | Armor_titanium -> Armor_titanium
    | _ -> prev_armor idx (* BUG? *)
  in
  let mult = get_tech_reduce_50 percent in
  let armor = get_armor idx in
  let cost1 = (get_armor_hull armor Hull_large).cost in
  let cost2 = (get_hull Hull_large).cost in
  (cost1 + cost2) * mult / 1500

let get_base_cost_mod_weap g weapon_tech percent =
  let mult = get_tech_reduce_50 percent * 9 in
  ((Shiptech.get_weapon weapon_tech).cost * mult) / 1000

let get_base_cost_mod_shield g shield_tech percent =
  let mult = get_tech_reduce_50 percent in
  let shield = Shiptech.get_shield shield_tech in
  let hull_data = Shiptech.get_shield_hull shield Hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

let get_base_cost_mod_comp g comp_tech percent =
  let mult = get_tech_reduce_50 percent in
  let comp = Shiptech.get_comp comp_tech in
  let hull_data = Shiptech.get_comp_hull comp Hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

let get_best_jammer g player tech =
  let open Shiptech in
  fold_jammer (fun acc i jammer ->
    if Tech.(jammer.tech <= tech) &&
       player_has_tech g Tech_field_computer jammer.tech player
    then i else acc)
  ~init:Jammer_none

let get_base_cost_mod_jammer g player percent =
  let idx = get_best_jammer g player Tech.max in
  let mult = get_tech_reduce_50 percent in
  let jammer = Shiptech.get_jammer idx in
  let hull_data = Shiptech.get_jammer_hull jammer Hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

  (* Add a newtech event *)
  (* mutates newtech *)
let add_newtech g player field tech source a8 stolen_from frame =
  let ev_player = get_events_perplayer g player in
  if Vector.length ev_player.newtechs < newtech_max then begin
    let frame = match source with
      | Techsource_spy -> frame
      | _ -> false
    in
    let other1, other2, frame =
      if frame then
        (* If we're framing, find 2 other races the victrim is at contact with
        * (not the originator) and place them in other1, other2.
        * For some reason, we *have* to have 2 others, or it doesn't work. BUG? *)
        let eto = get_eto g stolen_from in
        match get_eto_contact_idxs eto with
        | x::y::_ when not Player.(x = player) && not @@ Player.(y = player) -> x, y, frame
        | _ -> Player.none, Player.none, false
      else
        Player.none, Player.none, false
    in
    let add =
      {field; tech; source; v06=a8; stolen_from; other1; other2; frame}
    in
    Vector.push ev_player.newtechs add
  end

(* Find techs to add *)
let get_next_techs g player field =
  let eto = get_eto g player in
  let rcomplete = get_research_completed eto field in
  let len = get_tech_num rcomplete in
  let tmax = Tech.to_int @@ get_max_tech rcomplete in
  let maxtier = match len with
    | 1 -> 1
    | _ ->
        let t = (tmax - 1) / 5 + 2 in
        if t < 10 then t else 10
  in
  (* iterate over research list and add relevant techs *)
  let research_list = research_list_of_field eto field in
  let _, techs =
    Array_slice.fold (fun (num, acc) l ->
      List.fold_left (fun (num, acc) tech ->
        if (not @@ TechSet.mem tech rcomplete) && num < tech_next_max then
             (num + 1, tech::acc)
        else num, acc)
      (num, acc)
      l
    )
    (0, [])
    (Array_slice.make research_list 0 ~len:(maxtier - 1))
  in
  let techs = techs |> List.rev |> Array.of_list in
  match Array.length techs with
  | 0 ->
      let _, techs =
        let tmax =
          if tmax <= 50 then 55
          else
            let tmax = tmax + 5 in
            if tmax > 100 then 100 else tmax
        in
        OSeq.fold_while (fun (num, acc) tech ->
          if tech > tmax || num >= tech_next_max then (num, acc), `Stop
          else
            let tech = Tech.of_int tech in
            let num, acc =
              if not @@ TechSet.mem tech rcomplete then
                (num+1, tech::acc)
              else
                num, acc
            in
            (num, acc), `Continue)
          (0, [])
          (OSeq.iterate 55 ((+) 5))
      in
      techs |> List.rev |> Array.of_list

  | _ -> techs

(* RP = research point *)
let get_next_rp g player field tech =
  let tech_i = Tech.to_int tech in
  let cost = tech_i * tech_i in
  let eto = get_eto g player in
  let cost = cost * Num.get_tech_costmulr eto.race field in
  let cost =
    if is_ai g player then
      let cost = cost * Ai.tech_cost g player in
      cost / 100
    else
      (cost * Num.get_tech_costmuld g.difficulty / 1000) * 10
  in
  cost

(* Create event for player to get tech
 * mutates techdata, nexttech *)
let start_next g player field tech =
  let eto = get_eto g player in
  let techdata = get_techdata eto field in
  let investment =
    if Tech.(not (techdata.project = none)) then 0
    else techdata.investment
  in
  let project = tech in
  let cost = get_next_rp g player field tech in
  update_techdata eto field (fun td -> {td with investment; project; cost});
  let events_pp = get_events_perplayer g player in
  update_nexttechs events_pp field (fun _ -> Array.empty)

(* start_next: mutates techdata, nexttech *)
let ai_tech_next g player field =
  let techs = get_next_techs g player field in
  if Array.length techs > 0 then begin
    let tech = Ai.tech_next g player field techs in
    start_next g player field tech
  end

  (* mutates techdata, newtech, nexttech *)
let get_new g player field tech source a8 stolen_from frame =
  let eto = get_eto g player in
  let rcompleted = get_research_completed eto field in
  let have_tech = TechSet.mem tech rcompleted in
  if have_tech || Tech.(tech = Tech.none) then
    if Tech.((get_techdata eto field).project = tech) then begin
      if is_ai g player then begin
        ai_tech_next g player field
      end else begin
        update_techdata eto field (fun td -> {td with project = Tech.none});
        match source with
        | Techsource_trade -> add_newtech g player field tech source a8 stolen_from frame
        | _ -> ()
      end
    end
  else begin (* don't have this tech *)
    update_research_completed eto field (fun rc -> TechSet.add tech rc);

    if is_human g player then begin
      add_newtech g player field tech source a8 stolen_from frame
    end;

    if Tech.((get_techdata eto field).project = tech) then begin
      update_techdata eto field (fun td ->
        {td with project = Tech.none; investment = 1})
    end;

    if Tech.((get_techdata eto field).project = Tech.none) &&
        is_ai g player then begin
      ai_tech_next g player field
    end
  end


let tech_share g field accepted from_dead =
  (* Search for players to take techs from *)
  let tech_sources =
    fold_perplayer g (fun acc player pp ->
      if Bool.equal pp.refuse accepted && (from_dead || pp.alive) then
        (* We can take from this race *)
        let rc = get_research_completed pp.eto field in
        TechSet.fold (fun tech acc ->
          TechMap.add tech player acc)
          rc acc
      else
        acc
    )
    ~init:TechMap.empty
  in
  (* Add collected techs to other races *)
  iter_perplayer g (fun player pp ->
    if Bool.equal pp.refuse accepted || not pp.alive then ()
    else
      if pp.is_ai then begin
        (* Add collected techs to AI *)
        update_research_completed (get_eto g player) field (fun rc ->
          TechMap.fold (fun tech _ acc -> TechSet.add tech acc) tech_sources rc)
      end else begin
        TechMap.to_iter tech_sources
        |> Iter.take_while (fun _ ->
            Vector.length (get_events_perplayer g player).newtechs < newtech_max)
        |> Iter.iter (fun (tech, source_p) ->
          if not @@ player_has_tech g field tech player then
            (* BUG:? Odd conversion of source player to a8
             * And why don't we track who we took it from? (player.none)
             *)
            get_new g player field tech Techsource_trade (Player.to_int source_p) Player.none false
          )
      end
  )

let player_best_tech g field tech_base tech_step tech_max player =
  (* Q:Why do we skip by a step here? *)
  let tech_base_i = Tech.to_int tech_base in
  let start = if tech_base_i >= 2 then tech_base_i else tech_step in
  let iter_range =
    (Iter.int_range_by ~step:tech_step start @@ tech_max - 1)
  in
  Iter.fold (fun acc tech_i ->
      let tech = Tech.of_int tech_i in
      if player_has_tech g field tech player then tech else acc)
    Tech.none
    iter_range

(* Get the cost of a missile base, which is outfitted with the latest tech *)
let get_base_cost g player =
  let eto = get_eto g player in
  let percent_of field = (get_techdata eto field).percent in
  let cost =
    get_base_cost_mod_armor  g player          (percent_of Tech_field_construction) +
    get_base_cost_mod_weap   g eto.base_weapon (percent_of Tech_field_weapon) +
    get_base_cost_mod_shield g eto.base_shield (percent_of Tech_field_force_field) +
    get_base_cost_mod_comp   g eto.base_comp   (percent_of Tech_field_computer) +
    get_base_cost_mod_jammer g player          (percent_of Tech_field_computer)
  in
  let cost = (cost * 3) / 5 in
  let cost =
    if is_ai g player then
      Ai.base_cost_reduce g player cost
    else cost
  in
  if cost < 50 then 50 else cost

let () =
  Game_misc.get_base_cost := get_base_cost

let get_base_weapon g player tech =
  Shiptech.fold_weapon (fun acc i weapon ->
      if Tech.(weapon.tech <= tech) &&
         weapon.damage_min = weapon.damage_max && (* BUG? *)
         weapon.num_shots = 2 &&
         weapon.miss_type = 0 &&
         not weapon.is_bio &&
         weapon.num_miss = 1 &&
         player_has_tech g Tech_field_weapon weapon.tech player
      then i else acc)
    ~init:Weapon_laser (* BUG? *)

let get_base_weapon2 g player tech default =
  Shiptech.fold_weapon
  (fun acc i weapon ->
    if weapon.num_miss > 1 &&
       Shiptech.(weapon_to_enum i < weapon_to_enum Weapon_plasma_torpedo) &&
       player_has_tech g Tech_field_weapon weapon.tech player
      then i else acc)
  ~init:default

let get_best_shield g player tech =
  let open Shiptech in
  fold_shield (fun acc i shield ->
    if Tech.(shield.tech <= tech) &&
       player_has_tech g Tech_field_force_field shield.tech player
    then i else acc)
  ~init:Shield_none

let get_best_comp g player tech =
  let open Shiptech in
  fold_comp (fun acc i comp ->
    if Tech.(comp.tech <= tech) &&
       player_has_tech g Tech_field_computer comp.tech player
    then i else acc)
  ~init:Comp_none

(* Extract & update utility of available techs
 * mutates etos
 * TODO: externalize, similar to shiptech
 *)
let update_tech_util g =
  let open Techtypes in
  iter_perplayer g (fun player perplayer ->
    let eto = get_eto g player in
    let rc_plan = get_research_completed eto Tech_field_planetology in
    let check_plan tech = TechSet.mem (plan_to_tech tech) rc_plan in
    let have_colony_for =
      if check_plan Plan_controlled_radiated_env then Planet.Radiated else
      if check_plan Plan_controlled_toxic_env then Planet.Toxic else
      if check_plan Plan_controlled_inferno_env then Planet.Inferno else
      if check_plan Plan_controlled_dead_env then Planet.Dead else
      if check_plan Plan_controlled_tundra_env then Planet.Tundra else
      if check_plan Plan_controlled_barren_env then Planet.Barren else
      Planet.Minimal
    in
    let have_colony_for = match eto.race with
      | Silicoid -> Planet.Radiated | _ -> have_colony_for in
    let check_plan_not_silicoid x =
      check_plan x && (match eto.race with Silicoid -> false | _ -> true) in
    let have_adv_soil_enrich = check_plan_not_silicoid Plan_advanced_soil_enrichment in
    let have_atmos_terra = check_plan_not_silicoid Plan_atmospheric_terraforming in
    let have_soil_enrich = check_plan_not_silicoid Plan_soil_enrichment in
    let inc_pop_cost =
      if check_plan Plan_advanced_cloning then 5
      else if check_plan Plan_cloning then 10
      else 20
    in
    let inc_pop_cost = match eto.race with Sakkra -> inc_pop_cost * 2 / 3 | _ -> inc_pop_cost in
    let have_terraform_n, terraform_cost_per_inc =
      if check_plan Plan_complete_terraforming then 120, 2 else
      if check_plan Plan_improved_terraforming_100 then 100, 2 else
      if check_plan Plan_improved_terraforming_80 then 80, 2 else
      if check_plan Plan_improved_terraforming_60 then 60, 3 else
      if check_plan Plan_improved_terraforming_50 then 50, 3 else
      if check_plan Plan_improved_terraforming_40 then 40, 4 else
      if check_plan Plan_improved_terraforming_30 then 30, 4 else
      if check_plan Plan_improved_terraforming_20 then 20, 5 else
      if check_plan Plan_improved_terraforming_10 then 10, 5 else
      0, 5
    in
    let rc_prop = get_research_completed eto Tech_field_propulsion in
    let check_prop tech = TechSet.mem (prop_to_tech tech) rc_prop in
    let have_combat_transporter = check_prop Prop_combat_transporters in
    let have_eco_restoration_n =
      if check_plan Plan_complete_eco_restoration then 20 else
      if check_plan Plan_advanced_eco_restoration then 10 else
      if check_plan Plan_enhanced_eco_restoration then 5 else
      if check_plan Plan_improved_eco_restoration then 3 else
      2
    in
    let rc_comp = get_research_completed eto Tech_field_computer in
    let check_comp tech = TechSet.mem (comp_to_tech tech) rc_comp in
    let scanner_range =
      if check_comp Comp_advanced_space_scanner then 9 else
      if check_comp Comp_improved_space_scanner then 7 else
      if check_comp Comp_deep_space_scanner then 5 else
      3
    in
    let have_stargates = check_prop Prop_intergalactic_star_gates in
    let have_hyperspace_comm = check_comp Comp_hyperspace_communications in
    let have_ia_scanner =
      check_comp Comp_improved_space_scanner || check_comp Comp_advanced_space_scanner in
    let have_adv_scanner =
      check_comp Comp_advanced_space_scanner in
    let b =
      if check_comp Comp_improved_robotic_controls_vii then 7 else
      if check_comp Comp_improved_robotic_controls_vi then 6 else
      if check_comp Comp_improved_robotic_controls_v then 5 else
      if check_comp Comp_improved_robotic_controls_iv then 4 else
      if check_comp Comp_improved_robotic_controls_iii then 3 else
      2
    in
    let colonist_oper_factories = match eto.race with Meklar -> b + 2 | _ -> b in
    let rc_cons = get_research_completed eto Tech_field_construction in
    let check_cons tech = TechSet.mem (cons_to_tech tech) rc_cons in
    let b =
      if check_cons Cons_improved_industrial_tech_2 then 2 else
      if check_cons Cons_improved_industrial_tech_3 then 3 else
      if check_cons Cons_improved_industrial_tech_4 then 4 else
      if check_cons Cons_improved_industrial_tech_5 then 5 else
      if check_cons Cons_improved_industrial_tech_6 then 6 else
      if check_cons Cons_improved_industrial_tech_7 then 7 else
      if check_cons Cons_improved_industrial_tech_8 then 8 else
      if check_cons Cons_improved_industrial_tech_9 then 9 else
      10
    in
    let factory_cost = b in
    let factory_adj_cost = match eto.race with Meklar -> b | _ -> b * colonist_oper_factories / 2 in
    let ind_waste_scale =
      if check_cons Cons_industrial_waste_elimination then 0 else
      if check_cons Cons_reduced_industrial_waste_20 then 2 else
      if check_cons Cons_reduced_industrial_waste_40 then 4 else
      if check_cons Cons_reduced_industrial_waste_60 then 6 else
      if check_cons Cons_reduced_industrial_waste_80 then 8 else
      10
    in
    let fuel_range =
      if check_prop Prop_thorium_cells then 30 else
      if check_prop Prop_trilithium_crystals then 10 else
      if check_prop Prop_reajax_ii_fuel_cells then 9 else
      if check_prop Prop_uridium_fuel_cells then 8 else
      if check_prop Prop_dotomite_crystals then 7 else
      if check_prop Prop_uridium_fuel_cells then 6 else
      if check_prop Prop_deuterium_fuel_cells then 5 else
      if check_prop Prop_hydrogen_fuel_cells then 4 else
      3
    in
    let have_planet_shield =
      let rc_ffld = get_research_completed eto Tech_field_force_field in
      let check_ffld tech = TechSet.mem (ffld_to_tech tech) rc_ffld in
      if check_ffld Ffld_class_xx_planetary_shield then 20 else
      if check_ffld Ffld_class_xv_planetary_shield then 15 else
      if check_ffld Ffld_class_x_planetary_shield then 10 else
      if check_ffld Ffld_class_v_planetary_shield then 5 else
      0
    in
    let planet_shield_cost = Num.num_pshield_cost.(have_planet_shield / 5) in
    let have_engine =
      if check_prop Prop_hyper_drives then 8 else
      if check_prop Prop_inter_phased_drives then 7 else
      if check_prop Prop_anti_matter_drives then 6 else
      if check_prop Prop_ion_drives then 5 else
      if check_prop Prop_impulse_drives then 4 else
      if check_prop Prop_fusion_drives then 3 else
      if check_prop Prop_sub_light_drives then 2 else
      if check_prop Prop_nuclear_engines then 1 else
      1
    in
    let have_sub_space_int = check_prop Prop_sub_space_interdictor in
    let antidote =
      if check_plan Plan_universal_antidote then 2 else
      if check_plan Plan_bio_toxin_antidote then 1 else
      0
    in
    let scanner_range =
      if perplayer.gaux.flag_cheat_galaxy then 30 else scanner_range
    in
    let techu = {
      have_colony_for; have_adv_soil_enrich; have_atmos_terra; have_soil_enrich;
      inc_pop_cost; have_terraform_n; terraform_cost_per_inc; have_combat_transporter;
      have_eco_restoration_n; scanner_range; have_stargates; have_hyperspace_comm;
      have_ia_scanner; have_adv_scanner; colonist_oper_factories; factory_cost;
      factory_adj_cost; ind_waste_scale; fuel_range; have_planet_shield;
      planet_shield_cost; have_engine; have_sub_space_int; antidote;
    }
    in
    eto.techu <- techu
    )

let get_name gaux field tech =
  if Tech.(tech = none) then
    Game_str.get_te_field field
  else
    "" (* TODO: get research text from lbx *)

let get_newtech_msg g player newtech =
  let race = (get_eto g player).race in
  let sp = Printf.sprintf in
  let open Game_str in
  match newtech.source with
  | Techsource_research ->
      sp "%s %s %s %s" (get_race race) nt_achieve (get_te_field newtech.field) nt_break
  | Techsource_spy ->
      sp "%s %s %s" (get_race race) nt_infil g.planets.(newtech.v06).name
  | Techsource_found ->
      (* TODO: Make this more clearer *)
      if newtech.v06 = newtech_v06_orion then nt_orion
      else if newtech.v06 >= 0 then
        sp "%s %s %s" nt_ruins g.planets.(newtech.v06).name nt_discover
      else
        sp "%s %s %S" nt_scouts g.planets.(-(newtech.v06 + 1)).name nt_discover
  | Techsource_AI_spy -> nt_choose
     (* This is labeled as Techsource_choose. reused? *)
  | Techsource_trade ->
      let target_race = (get_eto g @@ Player.of_int newtech.v06).race in
      sp "%s %s %s %s" (get_race target_race) nt_reveal (get_te_field newtech.field) nt_secrets

let calc_research_investment invest slider total_research_bc =
  let max_bonus = invest * 3 / 20 in
  (* percent invested *)
  let to_add = slider * total_research_bc / 100 in
  let bonus = min (to_add * 2) max_bonus in
  let invest = invest + to_add + bonus in
  invest

let current_research_common eto field f =
  let td = get_techdata eto field in
  let slider = get_techslider eto field in
  let cost, invest = td.cost, td.investment in
  if cost = 0 || slider = 0 then 0
  else
    let invest =
      calc_research_investment invest slider eto.money.total_research_bc
    in
    f invest cost

let current_research_percent1 eto field =
  current_research_common eto field (fun invest cost ->
    let percent = invest * 100 / cost in
    percent)

let current_research_percent2 eto field =
  current_research_common eto field (fun invest cost ->
    if invest <= cost then 0
    else
      let percent = (invest - cost) * 50 / cost in
      set_range percent 0 99)

let current_research_has_max_bonus eto field =
  let td = get_techdata eto field in
  let slider = get_techslider eto field in
  let cost, invest = td.cost, td.investment in
  if cost = 0 || slider = 0 then false
  else
    let max_bonus = invest * 3 / 20 in
    (* percent invested *)
    let to_add = slider * eto.money.total_research_bc / 100 in
    (max_bonus <= to_add * 2) && to_add > 0

let current_research_time_score eto field =
  let td = get_techdata eto field in
  let f = current_research_percent1 eto field in
  let s =
    if f < 100 then
      122 * f
    else if not @@ current_research_has_max_bonus eto field then
      let g = f - 100 in
      g * g
    else
      let g = 2 * f - 100 in
      g * g
  in
  s * td.cost

(* mutates tech_sliders *)
(* keep adjusting until it stabilizes, giving us max_bonus *)
let set_to_max_bonus eto field =
  let had_bonus = current_research_has_max_bonus eto field in
  let rec adjust prev =
    let v = if had_bonus then prev - 1 else prev + 1
      |> set_range 0 100
    in
    update_techslider eto field (fun _ -> v);
    Game_misc.adjust_slider_group eto.tech_sliders (tech_field_to_enum field) v;
    let has_bonus = current_research_has_max_bonus eto field in
    let v = get_techslider eto field in
    if Bool.equal has_bonus had_bonus && v <> prev then adjust v
    else ()
  in
  adjust (get_techslider eto field)

(* distribute a value gradually among sliders *)
let distribute_sliders eto check_pct1 s =
  (* @check_pct1: disallow exceeding 100%? *)
  let rec loop s old_s =
    (* wait for stabilization or 0 *)
    if s > 0 && s < old_s then begin
      let old_s = s in
      let rs = ref s in
      iter_techdata eto (fun i _ ->
        (* Check if we maxed out *)
        if current_research_has_max_bonus eto i ||
          (check_pct1 && current_research_percent1 eto i > 100) then ()
        else begin
          add_techslider eto i 1;
          decr rs;
        end);
      loop !rs old_s
    end else s
    in
    loop s 100

let set_to_min eto =
  (* Set all sliders to 0 or 1 *)
  iter_techdata eto (fun i td ->
    let value = if td.investment > 0 then 1 else 0 in
    update_techslider eto i (fun _ -> value));
  (* feed old_s as 100 just to get one iteration *)
  let s = distribute_sliders eto true (100-tech_field_num) in
  (* continue to distribute to sliders, but allow to exceed 100% *)
  let _ = distribute_sliders eto false s in
  add_techslider eto tech_field_last s

let set_to_optimal eto =
  update_techsliders eto (fun _ _ -> 1);
  let s = distribute_sliders eto true (100-tech_field_num) in
  if s > 0 then begin
    let timescores = map_techdata eto (fun i _ -> current_research_time_score eto i) in
    (* distribute s to minimal sliders *)
    let rec loop s =
      if s <= 0 then begin
        let min_i = array_min timescores in
        let field = tech_field_of_int min_i in
        add_techslider eto field 1;
        timescores.(min_i) <- current_research_time_score eto field;
        loop (s-1)
      end else ()
    in
    loop s
  end
    else ()

(* Update nexttechs with fields we can choose from *)
let finish_new g player =
  let eto = get_eto g player in
  let events_pp = get_events_perplayer g player in
  let choices =
    (* check newtech fields *)
    Vector.fold (fun acc nt ->
      let project = (get_techdata eto nt.field).project in
      if Tech.(project = nt.tech) then
        FieldSet.add nt.field acc
      else acc)
      FieldSet.empty
      events_pp.newtechs
  in
  let choices =
    (* check project fields *)
    fold_techdata eto (fun acc field td ->
      if Tech.(td.project = none) && td.investment > 0 then
        FieldSet.add field acc
      else acc)
      ~init:choices
  in
  iter_techdata eto (fun field td ->
    let nexttech =
      if FieldSet.mem field choices then
        get_next_techs g player field
      else
        Array.empty
    in
    update_nexttechs events_pp field (fun _ -> nexttech)
    )

let can_choose g player field =
  let events_pp = get_events_perplayer g player in
  let nexttech = get_nexttech events_pp field in
  Array.length nexttech > 0

(* get cost of tech *)
let get_next_rp g player field tech =
  let i = Tech.to_int tech in
  let cost = i * i in (* TODO: make more resilient *)
  let race = (get_eto g player).race in
  let cost = cost * Num.get_tech_costmulr race field in
  if is_ai g player then
    let cost = cost * Ai.tech_cost g player in
    cost / 100
  else
    let cost = cost * Num.get_tech_costmuld g.difficulty in
    (cost / 1000) * 10

let start_next g player field tech =
  let eto = get_eto g player in
  update_techdata eto field (fun td ->
    let investment =
      if Tech.(td.project = Tech.none) then td.investment else 0
    in
    let project = tech in
    let cost = get_next_rp g player field tech in
    {td with investment; project; cost});
  let events_pp = get_events_perplayer g player in
  update_nexttechs events_pp field (fun _ -> Array.empty)

let get_field_top_tech g player field =
  let eto = get_eto g player in
  let rc = get_research_completed eto field in
  TechSet.max_elt rc

let get_field_percent g player field =
  let eto = get_eto g player in
  let rc = get_research_completed eto field in
  let len = TechSet.cardinal rc in
  let tmax = Tech.to_int @@ TechSet.max_elt rc in
  let v = match len with
    | 1 -> 1
    | _ -> (tmax - 1) / 5 + 2
  in
  let v = tmax + len - v in
  set_range v 1 99

let research g =
  iter_players g (fun (player:Player.t) ->
    let eto = get_eto g player in
    iter_fields (fun field ->
      let td = get_techdata eto field in
      let slider = get_techslider eto field in
      (* Calc how much we've invested *)
      let investment = calc_research_investment td.investment slider eto.money.total_research_bc in
      let percent = get_field_percent g player field in
      update_techdata eto field (fun td -> {td with investment; percent});
      (* So long as we're still investing (also, 1st tech has 0 cost *)
      if td.cost <> 0 && slider <> 0 && eto.money.total_research_bc <> 0 then begin
        (* If we've exceeded the cost *)
        if td.cost < investment then
          (* Chance of completing research *)
          let chance = (investment - td.cost) * 250 / td.cost in
          if random_1_n 500 g.seed <= chance then begin
            get_new g player field td.project Techsource_research
              (planet_get_random g player) Player.none false
          end
        else
          if td.cost <> 0 then begin (* WASBUG? 1 RP for first tech rounded down to 0 *)
            let investment = investment * 9 / 10 in (* why ? *)
            update_techdata eto field (fun td -> {td with investment; percent})
          end
      end
  ));
  update_tech_util g
