open Containers
open Utils
open Types

(* Shiptech vs tech
 * Shiptech is individual weapons/systems, as opposed to tech, which is
 * the research the shiptech relies on.
 *)

type weapon =
  | Weapon_none
  | Weapon_nuclear_bomb
  | Weapon_laser
  | Weapon_nuclear_missile_2
  | Weapon_nuclear_missile_5
  | Weapon_heavy_laser
  | Weapon_hyper_v_rocket_2
  | Weapon_hyper_v_rocker_5
  | Weapon_gatling_laser
  | Weapon_neutron_pellet_gun
  | Weapon_hyper_x_rocket_2
  | Weapon_hyper_x_rocker_5
  | Weapon_fusion_bomb
  | Weapon_ion_cannon
  | Weapon_heavy_ion_cannon
  | Weapon_scatter_pack_v_2
  | Weapon_scatter_pack_v_5
  | Weapon_death_spores
  | Weapon_mass_driver
  | Weapon_merculite_missile_2
  | Weapon_merculite_missile_5
  | Weapon_neutron_blaster
  | Weapon_heavy_blast_cannon
  | Weapon_anti_matter_bomb
  | Weapon_graviton_beam
  | Weapon_stinger_missile_2
  | Weapon_stinger_missile_5
  | Weapon_hard_beam
  | Weapon_fusion_beam
  | Weapon_heavy_fusion_beam
  | Weapon_omega_v_bomb
  | Weapon_anti_matter_torp
  | Weapon_megabolt_cannon
  | Weapon_phasor
  | Weapon_heavy_phasor
  | Weapon_scatter_pack_vii_2
  | Weapon_scatter_pack_vii_5
  | Weapon_doom_virus
  | Weapon_auto_blaster
  | Weapon_pulson_missile_2
  | Weapon_pulson_missile_5
  | Weapon_tachyon_beam
  | Weapon_gauss_autocannon
  | Weapon_particle_peam
  | Weapon_hercular_missile_2
  | Weapon_hercular_missile_5
  | Weapon_plasma_cannon
  | Weapon_disruptor
  | Weapon_pulse_phasor
  | Weapon_neutronium_bomb
  | Weapon_bio_terminator
  | Weapon_hellfire_torpedo
  | Weapon_zeon_missile
  | Weapon_zeon_missile_5
  | Weapon_proton_torpedo
  | Weapon_scatter_pack_x_2
  | Weapon_scatter_pack_x_5
  | Weapon_tri_focus_plasma
  | Weapon_stellar_converter
  | Weapon_mauler_device
  | Weapon_plasma_torpedo
  | Weapon_crystal_ray
  | Weapon_death_ray
  | Weapon_amoeba_stream
  [@@deriving enum]

let weapon_to_tech sh = Tech.of_int @@ weapon_to_enum sh

type comp =
  | Comp_none
  | Comp_mark_i
  | Comp_mark_ii
  | Comp_mark_iii
  | Comp_mark_iv
  | Comp_mark_v
  | Comp_mark_vi
  | Comp_mark_vii
  | Comp_mark_viii
  | Comp_mark_ix
  | Comp_mark_x
  | Comp_mark_xi
  [@@deriving enum]

let comp_to_tech sh = Tech.of_int @@ comp_to_enum sh

type engine =
  | Engine_retros
  | Engine_nuclear
  | Engine_sub_light
  | Engine_fusion
  | Engine_impulse
  | Engine_ion
  | Engine_anti_matter
  | Engine_interphased
  | Engine_hyperthrust
  [@@deriving enum]

let engine_to_tech sh = Tech.of_int @@ engine_to_enum sh

type armor =
  | Armor_titanium
  | Armor_titanium_ii
  | Armor_duralloy
  | Armor_duralloy_ii
  | Armor_zortrium
  | Armor_zortrium_ii
  | Armor_andrium
  | Armor_andrium_ii
  | Armor_tritanium
  | Armor_tritanium_ii
  | Armor_adamantium
  | Armor_adamantium_ii
  | Armor_neutronium
  | Armor_neutronium_ii
  [@@deriving enum]

let armor_to_tech sh = Tech.of_int @@ armor_to_enum sh

let prev_armor x = (armor_to_enum x) - 1 |> armor_of_enum |> Option.get_exn

type shield =
  | Shield_none
  | Shield_class_i
  | Shield_class_ii
  | Shield_class_iii
  | Shield_class_iv
  | Shield_class_v
  | Shield_class_vi
  | Shield_class_vii
  | Shield_class_ix
  | Shield_class_xi
  | Shield_class_xiii
  | Shield_class_xv
  [@@deriving enum]

let shield_to_tech sh = Tech.of_int @@ shield_to_enum sh

type jammer =
  | Jammer_none
  | Jammer_i
  | Jammer_ii
  | Jammer_iii
  | Jammer_iv
  | Jammer_v
  | Jammer_vi
  | Jammer_vii
  | Jammer_viii
  | Jammer_ix
  | Jammer_x
  [@@deriving enum]

let jammer_to_tech sh = Tech.of_int @@ jammer_to_enum sh

type special =
  | Special_none
  | Special_reserve_fuel_tanks
  | Special_standard_colony_base
  | Special_barren_colony_base
  | Special_tundra_colony_base
  | Special_dead_colony_base
  | Special_inferno_colony_base
  | Special_toxic_colony_base
  | Special_radiated_colony_base
  | Special_battle_scanner
  | Special_anti_missile_rockets
  | Special_repulsor_beam
  | Special_warp_dissipator
  | Special_energy_pulsar
  | Special_inertial_stabilizer
  | Special_zyro_shield
  | Special_automated_repair
  | Special_stasis_field
  | Special_cloaking_device
  | Special_ion_stream_projector
  | Special_high_energy_focus
  | Special_ionic_pulsar
  | Special_black_hole_generator
  | Special_sub_space_teleporter
  | Special_ligtning_shield
  | Special_neutron_stream_projector
  | Special_adv_damage_control
  | Special_technology_nullifier
  | Special_inertial_nullifier
  | Special_oracle_interface
  | Special_displacement_device
  [@@deriving enum]

type hull =
  | Hull_small
  | Hull_medium
  | Hull_large
  | Hull_huge
  [@@deriving enum]

type st_weapon = {
  name: string;
  extra_text: string;
  damage_min: int;
  damage_max: int;
  range: int;
  extra_acc: int;
  halve_shield: bool;
  is_bomb: bool;
  damage_fade: bool;
  miss_type: int;
  damage_mul: int;
  num_fire: int;
  num_shots: int;
  cost: int;
  space: int;
  power: int;
  is_bio: bool;
  tech: Tech.t; (* regular tech we depend on *)
  v24: int; (* beam? missile: fuel *)
  dtbl: int list; (* beam: color table; missile: 0=speed *)
  sound: int;
  num_miss: int; (* beam: streaming *)
}

let mk_weapon name extra_text damage_min damage_max range
    extra_acc halve_shield is_bomb damage_fade miss_type damage_mul
    num_fire num_shots cost space power is_bio tech v24
    dtbl sound num_miss =
      {name; extra_text; damage_min; damage_max; range;
    extra_acc; halve_shield; is_bomb; damage_fade; miss_type; damage_mul;
    num_fire; num_shots; cost; space; power; is_bio; tech; v24;
    dtbl; sound; num_miss}

type per_ship_hull = {
  power: int;
  space: int;
  cost: int;
}

type st_comp = {
  name: string;
  shiphull: per_ship_hull array; (* ship_hull_num *)
  tech: Tech.t;
  level: int;
}

let get_comp_hull comp hull = comp.shiphull.(hull_to_enum hull)

let mk_comp name power_l space_l cost_l tech_i level =
  let shiphull =
    Utils.map3 (fun power space cost -> {power; space; cost}) power_l space_l cost_l
    |> Array.of_list
  in
  let tech = Tech.of_int tech_i in
  {name; tech; shiphull; level}

type st_jammer = {
  name: string;
  shiphull: per_ship_hull array; (* ship_hull_num *)
  tech: Tech.t;
  level: int;
}

let get_jammer_hull jammer hull = jammer.shiphull.(hull_to_enum hull)

let mk_jammer name power_l space_l cost_l tech_i level =
  let shiphull =
    Utils.map3 (fun power space cost -> {power; space; cost}) power_l space_l cost_l
    |> Array.of_list
  in
  let tech = Tech.of_int tech_i in
  {name; shiphull; tech; level}

type st_engine = {
  name: string;
  power: int;
  space: int;
  cost: int;
  warp: int;
  tech: Tech.t;
}

let mk_engine name power space cost warp tech_i : st_engine =
  let tech = Tech.of_int tech_i in
  {name; power; space; cost; warp; tech}

type armor_per_ship_hull = {
  cost: int;
  space: int;
}

type st_armor = {
  name: string;
  shiphull: armor_per_ship_hull array;
  armor: int;
  tech: Tech.t;
}

let get_armor_hull armor hull = armor.shiphull.(hull_to_enum hull)

let mk_armor name cost_l space_l armor tech_i =
  let shiphull =
    List.map2 (fun cost space -> {cost; space}) cost_l space_l
    |> Array.of_list
  in
  let tech = Tech.of_int tech_i in
  {name; shiphull; armor; tech}

type st_shield = {
  name: string;
  shiphull: per_ship_hull array;
  absorb: int;
  tech: Tech.t;
}

let get_shield_hull shield hull = shield.shiphull.(hull_to_enum hull)

let mk_shield name cost_l space_l power_l absorb tech_i =
  let tech = Tech.of_int tech_i in
  let shiphull =
    Utils.map3 (fun power space cost -> {power; space; cost}) power_l space_l cost_l
    |> Array.of_list
  in
  {name; shiphull; absorb; tech}

(* boolean flags *)
type ship_special_bool =
  | Ship_special_bool_scanner
  | Ship_special_bool_repulsor
  | Ship_special_bool_warpdis
  | Ship_special_bool_stasis
  | Ship_special_bool_cloak
  | Ship_special_bool_blackhole
  | Ship_special_bool_subspace
  | Ship_special_bool_technull
  | Ship_special_bool_oracle
  | Ship_special_bool_disp
  [@@deriving enum]

type st_special = {
  name: string;
  extra_str: string;
  shiphull: per_ship_hull array;
  tech: Tech.t;
  field: tech_field;
  stype: int;
  repair: int;
  extraman: int;
  misshield: int;
  extrarange: int;
  pulsar: int;
  stream: int;
  boolmask: int;
}

let mk_special name extra_str cost_l space_l power_l tech field stype repair
  extraman misshield extrarange pulsar stream boolmask =
  let shiphull =
    Utils.map3 (fun power space cost -> {power; space; cost}) power_l space_l cost_l
    |> Array.of_list
  in
  {name; extra_str; shiphull; tech; field; stype; repair; extraman; misshield; extrarange;
  pulsar; stream; boolmask}


type st_hull = {
  name: string;
  cost: int;
  space: int;
  hits: int;
  power: int;
  defense: int;
}

let mk_hull name cost space hits power defense =
  {name; cost; space; hits; power; defense}

let tbl_weapon = [|
  mk_weapon "NONE" ""
    0 0 0
    0 false false false
    0 1 0 (-1)
    0 0 0
    false (Techtypes.weap_to_tech Techtypes.Weap_none)
    0 [0; 0; 0; 0; 0; 0; 0]
    0 0
  ;
  mk_weapon "NUCLEAR_BOMB" "GROUND_ATTACKS_ONLY"
    3 12 1
    0 false true false
    0 1 1 10
    30 40 10
    false (Techtypes.weap_to_tech Techtypes.Weap_lasers)
    0 [0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0]
    0x25 0
  ;
  mk_weapon "LASER" ""
    1 4 1
    0 false false false
    0 1 1 (-1)
    30 10 25
    false (Techtypes.weap_to_tech Techtypes.Weap_lasers)
    0 [0x40; 0x41; 0x42; 0x43; 0x44; 0x45; 0x46]
    0x7 0
    ;
  mk_weapon "NUCLEAR MISSILE" "2 SHOTS, +1 SPEED"
    4 4 6
    0 false false false
    0 1 1 2
    70 50 20
    false (Techtypes.weap_to_tech Techtypes.Weap_lasers)
    2 [ 0x60; 0x0; 0x0; 0x6; 0x50; 0x0; 0x0 ]
    0x8 1
    ;
  mk_weapon "NUCLEAR MISSILE" "5 SHOTS"
    4 4 4
    0 false false false
    0 1 1 5
    105 75 30
    false (Techtypes.weap_to_tech Techtypes.Weap_lasers)
    2 [ 0x40; 0x0; 0x0; 0x6; 0x50; 0x0; 0x0 ]
    0x8 1
    ;
  mk_weapon "HEAVY LASER" ""
        1 7 2
        0 false false false
        0 1 1 (-1)
        90 30 75
        false (Techtypes.weap_to_tech Techtypes.Weap_lasers)
        0 [0x40;0x41;0x42;0x43;0x44;0x45;0x46]
        0xa 0
    ;
  mk_weapon "HYPER-V ROCKET" "2 SHOTS, +1 SPEED"
        6 6 7
        0 false false false
        0 1 1 2
        90 70 20
        false (Techtypes.weap_to_tech Techtypes.Weap_hyper_v_rockets)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "HYPER-V ROCKET" "5 SHOTS"
        6 6 5
        0 false false false
        0 1 1 5
        135 105 30
        false (Techtypes.weap_to_tech Techtypes.Weap_hyper_v_rockets)
        2 [0x50;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "GATLING LASER" "FIRES 4 TIMES/TURN"
        1 4 1
        0 false false false
        0 1 4 (-1)
        90 20 70
        false (Techtypes.weap_to_tech Techtypes.Weap_gatling_laser)
        0 [0x43;0x41;0x3f;0x25;0x3f;0x41;0x43]
        0x1 0
    ;
  mk_weapon "NEUTRON PELLET GUN" "HALVES ENEMY SHIELDS"
        2 5 1
        0 true false false
        0 1 1 (-1)
        30 15 25
        false (Techtypes.weap_to_tech Techtypes.Weap_neutron_pellet_gun)
        0 [0x0;0xae;0x0;0x0;0x0;0xae;0x0]
        0x1 0
    ;
  mk_weapon "HYPER-X ROCKET" "2 SHOTS, +1 TO HIT"
        8 8 7
        1 false false false
        0 1 1 2
        120 100 20
        false (Techtypes.weap_to_tech Techtypes.Weap_hyper_x_rockets)
        2 [0x60;0x0;0x0;0x6;0x50;0x0;0x0]
        0x8 1
    ;
  mk_weapon "HYPER-X ROCKET" "5 SHOTS, +1 TO HIT"
        8 8 5
        1 false false false
        0 1 1 5
        180 150 30
        false (Techtypes.weap_to_tech Techtypes.Weap_hyper_x_rockets)
        2 [0x50;0x0;0x0;0x6;0x50;0x0;0x0]
        0x8 1
    ;
  mk_weapon "FUSION BOMB" "GROUND ATTACKS ONLY"
        5 20 1
        0 false true false
        0 1 1 10
        30 50 10
        false (Techtypes.weap_to_tech Techtypes.Weap_fusion_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  mk_weapon "ION CANNON" ""
        3 8 1
        0 false false false
        0 1 1 (-1)
        40 15 35
        false (Techtypes.weap_to_tech Techtypes.Weap_ion_cannon)
        1 [0xe4;0xe5;0xe6;0xe7;0xe6;0xe5;0xe4]
        0x10 0
    ;
  mk_weapon "HEAVY ION CANNON" ""
        3 15 2
        0 false false false
        0 1 1 (-1)
        110 45 105
        false (Techtypes.weap_to_tech Techtypes.Weap_ion_cannon)
        1 [0xe4;0xe5;0xe6;0xe7;0xe6;0xe5;0xe4]
        0xf 0
    ;
  mk_weapon "SCATTER PACK V" "2 SHOTS, MIRVS TO 5"
        6 6 7
        1 false false false
        0 1 1 2
        180 115 50
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_v_rockets)
        2 [0x60;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 5
    ;
  mk_weapon "SCATTER PACK V" "5 SHOTS, MIRVS TO 5"
        6 6 5
        1 false false false
        0 1 1 5
        270 170 80
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_v_rockets)
        2 [0x50;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 5
    ;
  mk_weapon "DEATH SPORES" "BIOLOGICAL WEAPON"
        1 1 1
        0 false true false
        0 1 1 5
        100 100 10
        true (Techtypes.plan_to_tech Techtypes.Plan_death_spores)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  mk_weapon "MASS DRIVER" "HALVES ENEMY SHIELDS"
        5 8 1
        0 true false false
        0 1 1 (-1)
        90 55 50
        false (Techtypes.weap_to_tech Techtypes.Weap_mass_driver)
        0 [0x0;0xc;0x0;0x0;0x0;0xc;0x0]
        0x17 0
    ;
  mk_weapon "MERCULITE MISSILE" "2 SHOTS, +2 TO HIT"
        10 10 8
        2 false false false
        0 1 1 2
        130 105 20
        false (Techtypes.weap_to_tech Techtypes.Weap_merculite_missiles)
        2 [0x80;0x0;0x0;0x4;0x78;0x0;0x0]
        0x8 1
    ;
  mk_weapon "MERCULITE MISSILE" "5 SHOTS, +2 TO HIT"
        10 10 6
        2 false false false
        0 1 1 5
        195 155 30
        false (Techtypes.weap_to_tech Techtypes.Weap_merculite_missiles)
        2 [0x60;0x0;0x0;0x4;0x78;0x0;0x0]
        0x8 1
    ;
  mk_weapon "NEUTRON BLASTER" ""
        3 12 1
        0 false false false
        0 1 1 (-1)
        60 20 60
        false (Techtypes.weap_to_tech Techtypes.Weap_neutron_blaster)
        0 [0xcf;0xce;0xcd;0xcc;0xcb;0xca;0xc9]
        0xa 0
    ;
  mk_weapon "HEAVY BLAST CANNON" ""
        3 24 2
        0 false false false
        0 1 1 (-1)
        180 60 180
        false (Techtypes.weap_to_tech Techtypes.Weap_neutron_blaster)
        0 [0xcf;0xce;0xcd;0xcc;0xcb;0xca;0xc9]
        0x12 0
    ;
  mk_weapon "ANTI-MATTER BOMB" "GROUND ATTACKS ONLY"
        10 40 1
        0 false true false
        0 1 1 10
        50 75 10
        false (Techtypes.weap_to_tech Techtypes.Weap_anti_matter_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  mk_weapon "GRAVITON BEAM" "STREAMING ATTACK"
        1 15 1
        0 false false false
        0 1 1 (-1)
        60 20 50
        false (Techtypes.weap_to_tech Techtypes.Weap_graviton_beam)
        2 [0xd1;0xd2;0xd3;0xd4;0xd5;0xd6;0xd7]
        0x0 1
    ;
  mk_weapon "STINGER MISSILE" "2 SHOTS, +3 TO HIT"
        15 15 9
        3 false false false
        0 1 1 2
        190 155 30
        false (Techtypes.weap_to_tech Techtypes.Weap_stinger_missiles)
        2 [0x90;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "STINGER MISSILE" "5 SHOTS, +3 TO HIT"
        15 15 7
        3 false false false
        0 1 1 5
        270 230 45
        false (Techtypes.weap_to_tech Techtypes.Weap_stinger_missiles)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "HARD BEAM" "HALVES SHIELD STR"
        8 12 1
        0 true false false
        0 1 1 (-1)
        120 50 100
        false (Techtypes.weap_to_tech Techtypes.Weap_hard_beam)
        0 [0xa1;0x96;0xa1;0x96;0xa1;0x96;0xa1]
        0x1 0
    ;
  mk_weapon "FUSION BEAM" ""
        4 16 1
        0 false false false
        0 1 1 (-1)
        70 20 75
        false (Techtypes.weap_to_tech Techtypes.Weap_fusion_beam)
        0 [0xb7;0xb6;0xb5;0xb4;0xb3;0xb2;0xb1]
        0xb 0
    ;
  mk_weapon "HEAVY FUSION BEAM" ""
        4 30 2
        0 false false false
        0 1 1 (-1)
        210 60 225
        false (Techtypes.weap_to_tech Techtypes.Weap_fusion_beam)
        0 [0xb7;0xb6;0xb5;0xb4;0xb3;0xb2;0xb1]
        0x19 0
    ;
  mk_weapon "OMEGA-V BOMB" "GROUND ATTACKS ONLY"
        20 50 1
        0 false true false
        0 1 1 10
        80 140 10
        false (Techtypes.weap_to_tech Techtypes.Weap_omega_v_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  mk_weapon "ANTI-MATTER TORP" "FIRES ONE PER 2 TURNS"
        30 30 8
        4 false false false
        1 1 1 (-1)
        300 75 300
        false (Techtypes.weap_to_tech Techtypes.Weap_anti_matter_torpedoes)
        2 [0x80;0x0;0x0;0x3;0xa0;0x0;0x0]
        0x11 1
    ;
  mk_weapon "MEGABOLT CANNON" "+3 LEVELS TO HIT"
        2 20 1
        3 false false false
        0 1 1 (-1)
        80 30 65
        false (Techtypes.weap_to_tech Techtypes.Weap_megabolt_cannon)
        3 [0xaf;0xd7;0xae;0xd7;0xaf;0xd7;0xae]
        0x12 0
    ;
  mk_weapon "PHASOR" ""
        5 20 1
        0 false false false
        0 1 1 (-1)
        90 20 90
        false (Techtypes.weap_to_tech Techtypes.Weap_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1c 0
    ;
  mk_weapon "HEAVY PHASOR" ""
        5 40 2
        0 false false false
        0 1 1 (-1)
        260 60 270
        false (Techtypes.weap_to_tech Techtypes.Weap_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1a 0
    ;
  mk_weapon "SCATTER PACK VII" "2 SHOTS, MIRVS TO 7"
        10 10 8
        2 false false false
        0 1 1 2
        280 170 50
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_vii_missiles)
        2 [0x80;0x0;0x0;0x4;0x78;0x0;0x0]
        0x3 7
    ;
  mk_weapon "SCATTER PACK VII" "5 SHOTS, MIRVS TO 7"
        10 10 6
        2 false false false
        0 1 1 5
        420 230 80
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_vii_missiles)
        2 [0x60;0x0;0x0;0x4;0x78;0x0;0x0]
        0x3 7
    ;
  mk_weapon "DOOM VIRUS" "BIOLOGICAL WEAPON"
        2 2 1
        0 false true false
        0 1 1 5
        150 200 10
        true (Techtypes.plan_to_tech Techtypes.Plan_doom_virus)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  mk_weapon "AUTO BLASTER" "FIRES 3 TIMES/TURN"
        4 16 1
        0 false false false
        0 1 3 (-1)
        140 30 90
        false (Techtypes.weap_to_tech Techtypes.Weap_auto_blaster)
        1 [0xbe;0xbd;0xbc;0xbb;0xba;0xb9;0xb8]
        0x1 0
    ;
  mk_weapon "PULSON MISSILE" "2 SHOTS, +4 TO HIT"
        20 20 10
        4 false false false
        0 1 1 2
        200 160 40
        false (Techtypes.weap_to_tech Techtypes.Weap_pulson_missiles)
        2 [0xa0;0x0;0x0;0x4;0xa0;0x0;0x0]
        0x8 1
    ;
  mk_weapon "PULSON MISSILE" "5 SHOTS, +4 TO HIT"
        20 20 8
        4 false false false
        0 1 1 5
        300 240 60
        false (Techtypes.weap_to_tech Techtypes.Weap_pulson_missiles)
        2 [0x80;0x0;0x0;0x4;0xa0;0x0;0x0]
        0x8 1
    ;
  mk_weapon "TACHYON BEAM" "STREAMING ATTACK"
        1 25 1
        0 false false false
        0 1 1 (-1)
        90 30 70
        false (Techtypes.weap_to_tech Techtypes.Weap_tachyon_beam)
        2 [0x8e;0x8c;0x8b;0x8a;0x89;0x88;0x87]
        0x1f 1
    ;
  mk_weapon "GAUSS AUTOCANON" "1/2 SHIELDS, FIRES 4"
        7 10 1
        0 true false false
        0 1 4 (-1)
        280 105 105
        false (Techtypes.weap_to_tech Techtypes.Weap_gauss_autocannon)
        0 [0x0;0xf;0x0;0x12;0x0;0x15;0x0]
        0x1 0
    ;
  mk_weapon "PARTICLE BEAM" "HALVES SHIELD STR"
        10 20 1
        0 true false false
        0 1 1 (-1)
        150 90 75
        false (Techtypes.weap_to_tech Techtypes.Weap_particle_beam)
        0 [0x0;0xf;0x0;0x12;0x0;0x15;0x0]
        0x21 0
    ;
  mk_weapon "HERCULAR MISSILE" "2 SHOTS, +5 TO HIT"
        25 25 10
        5 false false false
        0 1 1 2
        260 220 40
        false (Techtypes.weap_to_tech Techtypes.Weap_hercular_missiles)
        2 [0xb0;0x0;0x0;0x6;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "HERCULAR MISSILE" "5 SHOTS, +5 TO HIT"
        25 25 9
        5 false false false
        0 1 1 5
        390 330 60
        false (Techtypes.weap_to_tech Techtypes.Weap_hercular_missiles)
        2 [0x90;0x0;0x0;0x6;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "PLASMA CANNON" ""
        6 30 1
        0 false false false
        0 1 1 (-1)
        120 30 100
        false (Techtypes.weap_to_tech Techtypes.Weap_plasma_cannon)
        2 [0x46;0x45;0x44;0x43;0x44;0x45;0x46]
        0x1e 0
    ;
  mk_weapon "DISRUPTOR" ""
        10 40 2
        0 false false false
        0 1 1 (-1)
        210 70 160
        false (Techtypes.weap_to_tech Techtypes.Weap_disruptor)
        0 [0xf7;0xf6;0xf5;0xf4;0xf3;0xf2;0xf1]
        0x1c 0
    ;
  mk_weapon "PULSE PHASOR" "FIRES 3 TIMES/TURN"
        5 20 1
        0 false false false
        0 1 3 (-1)
        250 40 120
        false (Techtypes.weap_to_tech Techtypes.Weap_pulse_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1 0
    ;
  mk_weapon "NEUTRONIUM BOMB" "GROUND ATTACKS ONLY"
        40 70 1
        0 false true false
        0 1 1 10
        90 200 10
        false (Techtypes.weap_to_tech Techtypes.Weap_neutronium_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x14 0
    ;
  mk_weapon "BIO TERMINATOR" "BIOLOGICAL WEAPON"
        3 3 1
        0 false true false
        0 1 1 5
        200 300 10
        true (Techtypes.plan_to_tech Techtypes.Plan_bio_terminator)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x14 0
    ;
  mk_weapon "HELLFIRE TORPEDO" "HITS ALL FOUR SHIELDS"
        25 25 10
        6 false false false
        2 4 1 (-1)
        500 150 350
        false (Techtypes.weap_to_tech Techtypes.Weap_hellfire_torpedoes)
        2 [0xa0;0x0;0x0;0x4;0x8c;0x0;0x0]
        0x11 1
    ;
  mk_weapon "ZEON MISSILE" "2 SHOTS, +6 TO HIT"
        30 30 9
        6 false false false
        0 1 1 2
        300 250 50
        false (Techtypes.weap_to_tech Techtypes.Weap_zeon_missiles)
        2 [0xc0;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "ZEON MISSILE" "5 SHOTS, +6 TO HIT"
        30 30 7
        6 false false false
        0 1 1 5
        450 375 75
        false (Techtypes.weap_to_tech Techtypes.Weap_zeon_missiles)
        2 [0xa0;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  mk_weapon "PROTON TORPEDO" "FIRES ONE PER 2 TURNS"
        75 75 10
        6 false false false
        3 1 1 (-1)
        500 100 400
        false (Techtypes.weap_to_tech Techtypes.Weap_proton_torpedoes)
        2 [0xff;0x0;0x0;0x3;0xc8;0x0;0x0]
        0x11 1
    ;
  mk_weapon "SCATTER PACK X" "2 SHOTS, MIRVS TO 10"
        15 15 10
        3 false false false
        0 1 1 2
        300 250 50
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_x_missiles)
        2 [0x90;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 10
    ;
  mk_weapon "SCATTER PACK X" "5 SHOTS, MIRVS TO 10"
        15 15 10
        3 false false false
        0 1 1 5
        450 420 80
        false (Techtypes.weap_to_tech Techtypes.Weap_scatter_pack_x_missiles)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 10
    ;
  mk_weapon "TRI-FOCUS PLASMA" ""
        20 50 1
        0 false false false
        0 1 1 (-1)
        250 70 180
        false (Techtypes.weap_to_tech Techtypes.Weap_tri_focus_plasma_cannon)
        1 [0x46;0x45;0x44;0x43;0x44;0x45;0x46]
        0x1b 0
    ;
  mk_weapon "STELLAR CONVERTER" "HITS ALL FOUR SHIELDS"
        10 35 3
        0 false false false
        0 4 1 (-1)
        500 200 300
        false (Techtypes.weap_to_tech Techtypes.Weap_stellar_converter)
        3 [0x49;0x56;0x71;0x46;0x49;0x56;0x71]
        0x15 0
    ;
  mk_weapon "MAULER DEVICE" "CRUEL BRUTAL DAMAGE"
        20 100 1
        0 false false false
        0 1 1 (-1)
        550 150 300
        false (Techtypes.weap_to_tech Techtypes.Weap_mauler_device)
        4 [0xbb;0xbc;0xbd;0xbd;0xbd;0xbc;0xbb]
        0x23 0
    ;
  mk_weapon "PLASMA TORPEDO" "LOSES 15 DAMAGE/HEX"
        150 150 10
        7 false false true
        4 1 1 (-1)
        600 150 450
        false (Techtypes.weap_to_tech Techtypes.Weap_plasma_torpedoes)
        2 [0xc0;0x0;0x0;0x3;0xa0;0x0;0x0]
        0x11 1
    ;
  mk_weapon "CRYSTAL RAY" ""
        100 300 3
        0 false false false
        0 4 1 (-1)
        600 200 400
        false (Tech.of_int 101) (* BUG? *)
        3 [0xe;0xc7;0xe;0xe;0xef;0xe;0xc7]
        0x18 0
    ;
  mk_weapon "DEATH RAY" ""
        200 1000 1
        0 false false false
        0 1 1 (-1)
        1000 2000 2000
        false (Techtypes.weap_to_tech Techtypes.Weap_death_ray)
        4 [0xcb;0xc5;0xc4;0x46;0xc4;0xc5;0xcb]
        0x1d 0
    ;
  mk_weapon "AMEOBA STREAM" ""
        250 1000 3
        0 false false false
        0 1 1 (-1)
        600 200 400
        false (Tech.of_int 101)
        2 [0x46;0x46;0xdc;0xdc;0xdc;0xd3;0xd3]
        0xc 1
|]

let get_weapon x = tbl_weapon.(weapon_to_enum x)

let fold_weapon f ~init =
  let g acc i x = f (Option.get_exn @@ weapon_of_enum i) acc x in
  Array.foldi g init tbl_weapon

let tbl_comp = [|
  mk_comp "NONE" [0; 0; 0; 0] [0; 0; 0; 0] [0; 0; 0; 0] 0 0;
  mk_comp "MARK I" [5; 10; 20; 100] [5; 10; 20; 100] [40; 200; 1000; 5000] 1 1;
  mk_comp "MARK II" [7; 15; 30; 150] [7; 15; 30; 150] [50; 240; 1200; 6000] 5 2;
  mk_comp "MARK III" [10; 20; 40; 200] [10; 20; 40; 200] [60; 280; 1400; 7000] 10 3;
  mk_comp "MARK IV" [12; 25; 50; 250] [12; 25; 50; 250] [70; 320; 1600; 8000] 15 4;
  mk_comp "MARK V" [15; 30; 60; 300] [15; 30; 60; 300] [80; 360; 1800; 9000] 20 5;
  mk_comp "MARK VI" [17; 35; 70; 350] [17; 35; 70; 350] [90; 400; 2000; 10000] 25 6;
  mk_comp "MARK VII" [20; 40; 80; 400] [20; 40; 80; 400] [100; 440; 2200; 11000] 30 7;
  mk_comp "MARK VIII" [22; 45; 90; 450] [22; 45; 90; 450] [110; 480; 2400; 12000] 35 8;
  mk_comp "MARK IX" [25; 50; 100; 500] [25; 50; 100; 500] [120; 520; 2600; 13000] 40 9;
  mk_comp "MARK X" [27; 55; 110; 550] [27; 55; 110; 550] [130; 560; 2800; 14000] 45 10;
  mk_comp "MARK XI" [30; 60; 120; 600] [30; 60; 120; 600] [140; 600; 3000; 15000] 50 11;
|]

let get_comp x = tbl_comp.(comp_to_enum x)

let fold_comp f ~init =
  let g acc i x = f (Option.get_exn @@ comp_of_enum i) acc x in
  Array.foldi g init tbl_comp

let tbl_engine = [|
   mk_engine "RETROS" 10 10 20 1 1 ;
   mk_engine "NUCLEAR" 20 18 40 2 6 ;
   mk_engine "SUB-LIGHT" 30 26 60 3 12 ;
   mk_engine "FUSION" 40 33 80 4 18 ;
   mk_engine "IMPULSE" 50 36 100 5 24 ;
   mk_engine "ION" 60 40 120 6 30 ;
   mk_engine "ANTI-MATTER" 70 44 140 7 36 ;
   mk_engine "INTERPHASED" 80 47 160 8 42 ;
   mk_engine "HYPERTHRUST" 90 50 180 9 48 ;
|]

let get_engine x = tbl_engine.(engine_to_enum x)

let tbl_armor = [|
  mk_armor "TITANIUM"      [0; 0; 0; 0] [0; 0; 0; 0] 100 1;
  mk_armor "TITANIUM II"   [20; 100; 500; 2500] [20; 80; 400; 2000] 150 1;
  mk_armor "DURALLOY"      [20; 100; 600; 3000] [2; 10; 60; 300] 150 10;
  mk_armor "DURALLOY II"   [30; 150; 900; 4500] [25; 85; 425; 2100] 225 10;
  mk_armor "ZORTRIUM"      [40; 200; 1000; 5000] [4; 20; 100; 500] 200 17;
  mk_armor "ZORTRIUM II"   [60; 300; 1500; 7500] [30; 100; 500; 2500] 300 17;
  mk_armor "ANDRIUM"       [60; 300; 1500; 7500] [6; 30; 150; 750] 250 26;
  mk_armor "ANDRIUM II"    [90; 450; 2250; 11250] [35; 115; 575; 2875] 375 26;
  mk_armor "TRITANIUM"     [80; 400; 2000; 10000] [8; 40; 200; 1000] 300 34;
  mk_armor "TRITANIUM II"  [120; 600; 3000; 15000] [40; 130; 650; 3250] 450 34;
  mk_armor "ADAMANTIUM"    [100; 500; 2500; 12500] [10; 50; 250; 1250] 350 42;
  mk_armor "ADAMANTIUM II" [150; 750; 3750; 18750] [45; 150; 750; 3750] 525 42;
  mk_armor "NEUTRONIUM"    [120; 600; 3000; 15000] [12; 60; 300; 1500] 400 50;
  mk_armor "NEUTRONIUM II" [180; 900; 4500; 25000] [50; 175; 875; 4375] 600 50;
|]

let get_armor x = tbl_armor.(armor_to_enum x)

let fold_armor f ~init =
  let g acc i x = f (Option.get_exn @@ armor_of_enum i) acc x in
  Array.foldi g init tbl_armor

let tbl_shield = [|
  mk_shield "NONE" [ 0; 0; 0; 0 ] [ 0; 0; 0; 0 ] [ 0; 0; 0; 0 ] 0 0;
  mk_shield "CLASS I" [ 30; 190; 1200; 7500 ] [ 5; 20; 60; 250 ] [ 5; 20; 60; 250 ] 1 1;
  mk_shield "CLASS II" [ 35; 220; 1400; 8750 ] [ 10; 35; 90; 375 ] [ 10; 35; 90; 375 ] 2 4;
  mk_shield "CLASS III" [ 40; 250; 1600; 10000 ] [ 15; 50; 120; 500 ] [ 15; 50; 120; 500 ] 3 10;
  mk_shield "CLASS IV" [ 45; 280; 1800; 11250 ] [ 20; 65; 150; 675 ] [ 20; 65; 150; 675 ] 4 14;
  mk_shield "CLASS V" [ 50; 310; 2000; 12500 ] [ 25; 80; 180; 750 ] [ 25; 80; 180; 750 ] 5 20;
  mk_shield "CLASS VI" [ 55; 340; 2200; 13750 ] [ 30; 95; 210; 875 ] [ 30; 95; 210; 875 ] 6 24;
  mk_shield "CLASS VII" [ 60; 370; 2400; 15000 ] [ 35; 110; 240; 1000 ] [ 35; 110; 240; 1000 ] 7 30;
  mk_shield "CLASS IX" [ 65; 400; 2600; 16250 ] [ 40; 125; 270; 1125 ] [ 40; 125; 270; 1125 ] 9 34;
  mk_shield "CLASS XI" [ 70; 430; 2800; 17500 ] [ 45; 140; 300; 1250 ] [ 45; 140; 300; 1250 ] 11 40;
  mk_shield "CLASS XIII" [ 80; 460; 3000; 18750 ] [ 50; 155; 330; 1375 ] [ 50; 155; 330; 1375 ] 13 44;
  mk_shield "CLASS XV" [ 90; 490; 3200; 20000 ] [ 55; 160; 360; 1500 ] [ 55; 160; 360; 1500 ] 15 50;
|]

let get_shield x = tbl_shield.(shield_to_enum x)

let fold_shield f ~init =
  let g acc i x = f (Option.get_exn @@ shield_of_enum i) acc x in
  Array.foldi g init tbl_shield

let tbl_jammer = [|
  mk_jammer "NONE"        [ 0; 0; 0; 0 ] [ 0; 0; 0; 0 ] [ 0; 0; 0; 0 ] 0 0;
  mk_jammer "JAMMER I"    [ 10; 20; 40; 170 ] [ 10; 20; 40; 170 ] [ 25; 150; 1000; 6250 ] 2 1;
  mk_jammer "JAMMER II"   [ 15; 30; 60; 250 ] [ 15; 30; 60; 250 ] [ 27; 165; 1100; 6875 ] 7 2;
  mk_jammer "JAMMER III"  [ 20; 40; 80; 330 ] [ 20; 40; 80; 330 ] [ 30; 180; 1200; 7500 ] 12 3;
  mk_jammer "JAMMER IV"   [ 25; 50; 100; 410 ] [ 25; 50; 100; 410 ] [ 32; 195; 1300; 8125 ] 17 4;
  mk_jammer "JAMMER V"    [ 30; 60; 120; 490 ] [ 30; 60; 120; 490 ] [ 35; 210; 1400; 8750 ] 22 5;
  mk_jammer "JAMMER VI"   [ 35; 70; 140; 570 ] [ 35; 70; 140; 570 ] [ 37; 225; 1500; 9375 ] 27 6;
  mk_jammer "JAMMER VII"  [ 40; 80; 160; 650 ] [ 40; 80; 160; 650 ] [ 40; 240; 1600; 10000 ] 32 7;
  mk_jammer "JAMMER VIII" [ 45; 90; 180; 730 ] [ 45; 90; 180; 730 ] [ 42; 255; 1700; 10625 ] 37 8;
  mk_jammer "JAMMER IX"   [ 50; 100; 200; 810 ] [ 50; 100; 200; 810 ] [ 45; 270; 1800; 11250 ] 42 9;
  mk_jammer "JAMMER X"    [ 55; 110; 220; 900 ] [ 55; 110; 220; 900 ] [ 50; 285; 1900; 11875 ] 47 10;
|]

let get_jammer x = tbl_jammer.(jammer_to_enum x)

let fold_jammer f ~init =
  let g acc i x = f (Option.get_exn @@ jammer_of_enum i) acc x in
  Array.foldi g init tbl_jammer

let tbl_hull = [|
  mk_hull "SMALL" 60 40 3 2 2;
  mk_hull "MEDIUM" 360 200 18 15 1;
  mk_hull "LARGE" 2000 1000 100 100 0;
  mk_hull "HUGE" 12000 5000 600 700 (-1);
|]

let get_hull x = tbl_hull.(hull_to_enum x)

let tbl_special = [|
    mk_special "NONE" ""
        [ 0; 0; 0; 0 ]
        [ 0; 0; 0; 0 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.cons_to_tech Techtypes.Cons_none) Tech_field_construction 0
        0 0 0 0 0 0
        0
    ;
    mk_special "RESERVE FUEL TANKS" "EXTENDS SHIP RANGE BY 3 PARSECS"
        [ 20; 100; 500; 2500 ]
        [ 20; 100; 500; 2500 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.cons_to_tech Techtypes.Cons_reserve_fuel_tanks) Tech_field_construction 1
        0 0 0 0 0 0
        0
    ;
    mk_special "STANDARD COLONY BASE" "ALLOWS NORMAL PLANET LANDINGS"
        [ 3500; 3500; 3500; 3500 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_ecological_restoration) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "BARREN COLONY BASE" "ALLOWS BARREN PLANET LANDINGS"
        [ 3750; 3750; 3750; 3750 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_barren_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "TUNDRA COLONY BASE" "ALLOWS TUNDRA PLANET LANDINGS"
        [ 4000; 4000; 4000; 4000 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_tundra_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "DEAD COLONY BASE" "ALLOWS DEAD PLANET LANDINGS"
        [ 4250; 4250; 4250; 4250 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_dead_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "INFERNO COLONY BASE" "ALLOWS INFERNO PLANET LANDINGS"
        [ 4500; 4500; 4500; 4500 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_inferno_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "TOXIC COLONY BASE" "ALLOWS TOXIC PLANET LANDINGS"
        [ 4750; 4750; 4750; 4750 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_toxic_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "RADIATED COLONY BASE" "ALLOWS RADIATED PLANET LANDINGS"
        [ 5000; 5000; 5000; 5000 ]
        [ 700; 700; 700; 700 ]
        [ 0; 0; 0; 0 ]
        (Techtypes.plan_to_tech Techtypes.Plan_controlled_radiated_env) Tech_field_planetology 2
        0 0 0 0 0 0
        0
    ;
    mk_special "BATTLE SCANNER" "DISPLAYS ENEMY SHIP STATS"
        [ 300; 300; 300; 300 ]
        [ 50; 50; 50; 50 ]
        [ 50; 50; 50; 50 ]
        (Techtypes.comp_to_tech Techtypes.Comp_battle_scanner) Tech_field_computer 3
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_scanner))
    ;
    mk_special "ANTI-MISSILE ROCKETS" "40% CHANCE MISSILES DESTROYED"
        [ 100; 100; 100; 100 ]
        [ 2; 10; 50; 250 ]
        [ 8; 40; 200; 1000 ]
        (Techtypes.weap_to_tech Techtypes.Weap_anti_missile_rockets) Tech_field_weapon 4
        0 0 40 0 0 0
        0
    ;
    mk_special "REPULSOR BEAM" "MOVES ENEMY SHIPS BACK 1 SPACE"
        [ 550; 550; 550; 550 ]
        [ 100; 100; 100; 100 ]
        [ 200; 200; 200; 200 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_repulsor_beam) Tech_field_force_field 5
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_repulsor))
    ;
    mk_special "WARP DISSIPATOR" "REDUCES SPEED OF ENEMY SHIPS"
        [ 650; 650; 650; 650 ]
        [ 100; 100; 100; 100 ]
        [ 300; 300; 300; 300 ]
        (Techtypes.prop_to_tech Techtypes.Prop_warp_dissipator) Tech_field_propulsion 6
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_warpdis))
    ;
    mk_special "ENERGY PULSAR" "EXPANDS TO INFLICT 1-5 HITS"
        [ 750; 750; 750; 750 ]
        [ 150; 150; 150; 150 ]
        [ 250; 250; 250; 250 ]
        (Techtypes.prop_to_tech Techtypes.Prop_energy_pulsar) Tech_field_propulsion 7
        0 0 0 0 1 0
        0
    ;
    mk_special "INERTIAL STABILIZER" "ADDS +2 TO MANEUVERABILITY"
        [ 20; 75; 500; 2700 ]
        [ 4; 20; 100; 500 ]
        [ 8; 40; 200; 1000 ]
        (Techtypes.prop_to_tech Techtypes.Prop_inertial_stabilizer) Tech_field_propulsion 8
        0 2 0 0 0 0
        0
    ;
    mk_special "ZYRO SHIELD" "75% CHANCE MISSILES DESTROYED"
        [ 50; 100; 200; 300 ]
        [ 4; 20; 100; 500 ]
        [ 12; 60; 300; 1500 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_zyro_shield) Tech_field_force_field 4
        0 0 75 0 0 0
        0
    ;
    mk_special "AUTOMATED REPAIR" "HEALS 15% OF SHIP'S HITS A TURN"
        [ 2; 8; 50; 300 ]
        [ 3; 15; 100; 600 ]
        [ 3; 10; 50; 300 ]
        (Techtypes.cons_to_tech Techtypes.Cons_automated_repair_system) Tech_field_construction 9
        15 0 0 0 0 0
        0
    ;
    mk_special "STASIS FIELD" "ENEMY FROZEN FOR 1 TURN"
        [ 2500; 2500; 2500; 2500 ]
        [ 200; 200; 200; 200 ]
        [ 275; 275; 275; 275 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_stasis_field) Tech_field_force_field 10
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_stasis))
    ;
    mk_special "CLOAKING DEVICE" "RENDERS SHIPS NEARLY INVISIBLE"
        [ 30; 150; 750; 3750 ]
        [ 5; 25; 120; 600 ]
        [ 10; 50; 250; 1250 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_cloaking_device) Tech_field_force_field 11
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_cloak))
    ;
    mk_special "ION STREAM PROJECTOR" "REDUCES ENEMY ARMOR BY 20%"
        [ 1000; 1000; 1000; 1000 ]
        [ 250; 250; 250; 250 ]
        [ 500; 500; 500; 500 ]
        (Techtypes.weap_to_tech Techtypes.Weap_ion_stream_projector) Tech_field_weapon 12
        0 0 0 0 0 1
        0
    ;
    mk_special "HIGH ENERGY FOCUS" "INCREASES WEAPON RANGE BY 3"
        [ 30; 135; 625; 3500 ]
        [ 35; 100; 150; 500 ]
        [ 65; 200; 350; 1000 ]
        (Techtypes.prop_to_tech Techtypes.Prop_high_energy_focus) Tech_field_propulsion 13
        0 0 0 3 0 0
        0
    ;
    mk_special "IONIC PULSAR" "EXPANDS TO INFLICT 2-10 HITS"
        [ 1500; 1500; 1500; 1500 ]
        [ 400; 400; 400; 400 ]
        [ 750; 750; 750; 750 ]
        (Techtypes.prop_to_tech Techtypes.Prop_ionic_pulsar) Tech_field_propulsion 7
        0 0 0 0 2 0
        0
    ;
    mk_special "BLACK HOLE GENERATOR" "KILLS 25%-100% OF ENEMY SHIPS"
        [ 2750; 2750; 2750; 2750 ]
        [ 750; 750; 750; 750 ]
        [ 750; 750; 750; 750 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_black_hole_generator) Tech_field_force_field 14
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_blackhole))
    ;
    mk_special "SUB SPACE TELEPORTER" "TELEPORTS SHIP IN COMBAT"
        [ 25; 100; 450; 2250 ]
        [ 4; 20; 100; 500 ]
        [ 16; 80; 400; 2000 ]
        (Techtypes.prop_to_tech Techtypes.Prop_sub_space_teleporter) Tech_field_propulsion 15
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_subspace))
    ;
    mk_special "LIGHTNING SHIELD" "100% CHANCE MISSILES DESTROYED"
        [ 200; 300; 400; 500 ]
        [ 6; 30; 150; 750 ]
        [ 15; 70; 350; 1750 ]
        (Techtypes.ffld_to_tech Techtypes.Ffld_lightning_shield) Tech_field_force_field 4
        0 0 100 0 0 0
        0
    ;
    mk_special "NEUTRON STREAM PROJECTOR" "REDUCES ENEMY ARMOR BY 40%"
        [ 2000; 2000; 2000; 2000 ]
        [ 500; 500; 500; 500 ]
        [ 1250; 1250; 1250; 1250 ]
        (Techtypes.weap_to_tech Techtypes.Weap_neutron_stream_projector) Tech_field_weapon 16
        0 0 0 0 0 2
        0
    ;
    mk_special "ADV DAMAGE CONTROL" "HEALS 30% OF SHIP'S HITS A TURN"
        [ 40; 200; 1000; 5000 ]
        [ 9; 45; 300; 1800 ]
        [ 9; 30; 150; 450 ]
        (Techtypes.cons_to_tech Techtypes.Cons_advanced_damage_control) Tech_field_construction 9
        30 0 0 0 0 0
        0
    ;
    mk_special "TECHNOLOGY NULLIFIER" "DESTROYS ENEMY COMPUTERS"
        [ 3000; 3000; 3000; 3000 ]
        [ 750; 750; 750; 750 ]
        [ 1000; 1000; 1000; 1000 ]
        (Techtypes.comp_to_tech Techtypes.Comp_technology_nullifier) Tech_field_computer 17
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_technull))
    ;
    mk_special "INERTIAL NULLIFIER" "ADDS +4 TO MANEUVERABILITY"
        [ 60; 200; 1500; 5000 ]
        [ 6; 30; 150; 750 ]
        [ 12; 60; 300; 1500 ]
        (Techtypes.prop_to_tech Techtypes.Prop_inertial_nullifier) Tech_field_propulsion 8
        0 4 0 0 0 0
        0
    ;
    mk_special "ORACLE INTERFACE" "CONCENTRATES BEAM ATTACKS"
        [ 30; 150; 600; 2750 ]
        [ 8; 40; 200; 1000 ]
        [ 12; 60; 300; 1500 ]
        (Techtypes.comp_to_tech Techtypes.Comp_oracle_interface) Tech_field_computer 20
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_oracle))
    ;
    mk_special "DISPLACMENT DEVICE" "1/3 OF ALL ENEMY ATTACKS MISS"
        [ 30; 150; 300; 2750 ]
        [ 10; 50; 225; 1250 ]
        [ 10; 50; 225; 1250 ]
        (Techtypes.prop_to_tech Techtypes.Prop_displacement_device) Tech_field_propulsion 21
        0 0 0 0 0 0
        (1 lsl (ship_special_bool_to_enum Ship_special_bool_disp))
    ;
|]

