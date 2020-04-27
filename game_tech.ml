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

let get_base_cost_mod_armor g player percent =
  let idx =
    Shiptech.armor_foldi ~init:0
      (fun i acc ship_armor ->
        if player_has_tech g Tech_field_construction ship_armor.tech_i player
        then i else acc)
  in
  let idx = if idx > 0 then idx - 1 else 0 in (* BUG? *)
  let mult = get_tech_reduce_50 percent in
  let armor = Shiptech.tbl_armor.(idx) in
  let cost1 = (Shiptech.get_armor_hull armor Ship_hull_large).cost in
  let cost2 = (Shiptech.hull_get Ship_hull_large).cost in
  (cost1 + cost2) * mult / 1500

let get_base_cost_mod_weap g tech_i percent =
  let mult = get_tech_reduce_50 percent * 9 in
  (Shiptech.tbl_weap.(tech_i).cost * mult) / 1000

let get_base_cost_mod_shield g tech_i percent =
  let mult = get_tech_reduce_50 percent in
  let shield = Shiptech.tbl_shield.(tech_i) in
  let hull_data = Shiptech.get_shield_hull shield Ship_hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

let get_base_cost_mod_comp g tech_i percent =
  let mult = get_tech_reduce_50 percent in
  let comp = Shiptech.tbl_comp.(tech_i) in
  let hull_data = Shiptech.get_comp_hull comp Ship_hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

let get_base_cost_mod_jammer g player percent =
  let tech_i =
    Shiptech.jammer_foldi ~init:0
      (fun i acc jammer ->
        if player_has_tech g Tech_field_computer jammer.tech_i player then i
        else acc)
  in
  let mult = get_tech_reduce_50 percent in
  let jammer = Shiptech.tbl_jammer.(tech_i) in
  let hull_data = Shiptech.get_jammer_hull jammer Ship_hull_large in
  (hull_data.cost * mult) / 1000 + hull_data.power / 10

  (* Add a newtech event *)
  (* mutates newtech *)
let add_newtech g player field tech source a8 stolen_from frame =
  let ev_player = get_events_perplayer g player in
  let num = ev_player.newtech.num in
  if num < newtech_max then begin
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
    let n = ev_player.newtech in
    ev_player.newtech <- {n with d = add::n.d; num = n.num + 1}
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
  update_nexttech events_pp field (fun nt -> {nt with num = 0})

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
    fold_perplayer (fun acc player pp ->
      if Bool.equal pp.refuse accepted && (from_dead || pp.alive) then
        (* We can take from this race *)
        let rc = get_research_completed pp.eto field in
        TechSet.fold (fun tech acc ->
          TechMap.add tech player acc)
          rc acc
      else
        acc
    )
    g
    ~init:TechMap.empty
  in
  (* Add collected techs to other races *)
  iter_perplayer (fun player pp ->
    if Bool.equal pp.refuse accepted || not pp.alive then ()
    else
      if pp.is_ai then begin
        (* Add collected techs to AI *)
        update_research_completed (get_eto g player) field (fun rc ->
          TechMap.fold (fun tech _ acc -> TechSet.add tech acc) tech_sources rc)
      end else begin
        TechMap.to_iter tech_sources
        |> Iter.take_while (fun _ ->
            (get_events_perplayer g player).newtech.num < newtech_max)
        |> Iter.iter (fun (tech, source_p) ->
          if not @@ player_has_tech g field tech player then
            (* BUG:? Odd conversion of source player to a8
             * And why don't we track who we took it from? (player.none)
             *)
            get_new g player field tech Techsource_trade (Player.to_int source_p) Player.none false
          )
      end
  )

