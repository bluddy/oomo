open Types
open Techtypes

type weapon =
  | Wpn_none
  | Wpn_nuclear_bomb
  | Wpn_laser
  | Wpn_nuclear_missile_2
  | Wpn_nuclear_missile_5
  | Wpn_heavy_laser
  | Wpn_hyper_v_rocket_2
  | Wpn_hyper_v_rocker_5
  | Wpn_gatling_laser
  | Wpn_neutron_pellet_gun
  | Wpn_hyper_x_rocket_2
  | Wpn_hyper_x_rocker_5
  | Wpn_fusion_bomb
  | Wpn_ion_cannon
  | Wpn_heavy_ion_cannon
  | Wpn_scatter_pack_v_2
  | Wpn_scatter_pack_v_5
  | Wpn_death_spores
  | Wpn_mass_driver
  | Wpn_merculite_missile_2
  | Wpn_merculite_missile_5
  | Wpn_neutron_blaster
  | Wpn_heavy_blast_cannon
  | Wpn_anti_matter_bomb
  | Wpn_graviton_beam
  | Wpn_stinger_missile_2
  | Wpn_stinger_missile_5
  | Wpn_hard_beam
  | Wpn_fusion_beam
  | Wpn_heavy_fusion_beam
  | Wpn_omega_v_bomb
  | Wpn_anti_matter_torp
  | Wpn_megabolt_cannon
  | Wpn_phasor
  | Wpn_heavy_phasor
  | Wpn_scatter_pack_vii_2
  | Wpn_scatter_pack_vii_5
  | Wpn_doom_virus
  | Wpn_auto_blaster
  | Wpn_pulson_missile_2
  | Wpn_pulson_missile_5
  | Wpn_tachyon_beam
  | Wpn_gauss_autocannon
  | Wpn_particle_peam
  | Wpn_hercular_missile_2
  | Wpn_hercular_missile_5
  | Wpn_plasma_cannon
  | Wpn_disruptor
  | Wpn_pulse_phasor
  | Wpn_neutronium_bomb
  | Wpn_bio_terminator
  | Wpn_hellfire_torpedo
  | Wpn_zeon_missile
  | Wpn_zeon_missile_5
  | Wpn_proton_torpedo
  | Wpn_scatter_pack_x_2
  | Wpn_scatter_pack_x_5
  | Wpn_tri_focus_plasma
  | Wpn_stellar_converter
  | Wpn_mauler_device
  | Wpn_plasma_torpedo
  | Wpn_crystal_ray
  | Wpn_death_ray
  | Weapon_amoeba_stream
  [@@deriving enum]

type ship_comp =
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

type ship_engine =
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

type ship_armor =
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

type ship_shield =
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

type ship_jammer =
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

type ship_special =
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

type ship_hull =
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
  numfire: int;
  numshots: int;
  cost: int;
  space: int;
  power: int;
  is_bio: bool;
  tech_i: int;
  v24: int; (* beam? missile: fuel *)
  dtbl: int list; (* beam: color table; missile: 0=speed *)
  sound: int;
  nummiss: int; (* beam: streaming *)
}

let make_weapon name extra_text damage_min damage_max range
    extra_acc halve_shield is_bomb damage_fade miss_type damage_mul
    numfire numshots cost space power is_bio tech_i v24
    dtbl sound nummiss =
      {name; extra_text; damage_min; damage_max; range;
    extra_acc; halve_shield; is_bomb; damage_fade; miss_type; damage_mul;
    numfire; numshots; cost; space; power; is_bio; tech_i; v24;
    dtbl; sound; nummiss}

type per_ship_hull = {
  power: int;
  space: int;
  cost: int;
}

type st_comp = {
  name: string;
  shiphull: per_ship_hull array; (* ship_hull_num *)
  tech_i: int;
  level: int;
}

type st_jammer = {
  name: string;
  shiphull: per_ship_hull array; (* ship_hull_num *)
  tech_i: int;
  level: int;
}

type st_engine = {
  name: string;
  power: int;
  space: int;
  cost: int;
  warp: int;
  tech_i: int;
}

type st_armor = {
  name: string;
  perhull: per_ship_hull array;
  armor: int;
  tech_i: int;
}

type st_shield = {
  name: string;
  perhull: per_ship_hull array;
  absorb: int;
  tech_i: int;
}

type special_bool =
  | Special_bool_scanner
  | Special_bool_repulsor
  | Special_bool_warpdis
  | Special_bool_stasis
  | Special_bool_cloak
  | Special_bool_blackhole
  | Special_bool_subspace
  | Special_bool_technull
  | Special_bool_oracle
  | Special_bool_disp

type st_special = {
  name: string;
  extra_ext: string;
  perhull: per_ship_hull array;
  tech_i: int;
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

type st_hull = {
  name: string;
  cost: int;
  space: int;
  hits: int;
  power: int;
  defense: int;
}

let weapon_table = [|
  make_weapon "NONE" ""
    0 0 0
    0 false false false
    0 1 0 (-1)
    0 0 0
    false 0
    0 [0; 0; 0; 0; 0; 0; 0]
    0 0
  ;
  make_weapon "NUCLEAR_BOMB" "GROUND_ATTACKS_ONLY"
    3 12 1
    0 false true false
    0 1 1 10
    30 40 10
    false (tech_weap_to_enum @@ Tech_weap_lasers)
    0 [0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0]
    0x25 0
  ;
  make_weapon "LASER" ""
    1 4 1
    0 false false false
    0 1 1 (-1)
    30 10 25
    false (tech_weap_to_enum Tech_weap_lasers)
    0 [0x40; 0x41; 0x42; 0x43; 0x44; 0x45; 0x46]
    0x7 0
    ;
  make_weapon "NUCLEAR MISSILE" "2 SHOTS, +1 SPEED"
    4 4 6
    0 false false false
    0 1 1 2
    70 50 20
    false (tech_weap_to_enum Tech_weap_lasers)
    2 [ 0x60; 0x0; 0x0; 0x6; 0x50; 0x0; 0x0 ]
    0x8 1
    ;
  make_weapon "NUCLEAR MISSILE" "5 SHOTS"
    4 4 4
    0 false false false
    0 1 1 5
    105 75 30
    false (tech_weap_to_enum Tech_weap_lasers)
    2 [ 0x40; 0x0; 0x0; 0x6; 0x50; 0x0; 0x0 ]
    0x8 1
    ;
  make_weapon "HEAVY LASER" ""
        1 7 2
        0 false false false
        0 1 1 (-1)
        90 30 75
        false (tech_weap_to_enum Tech_weap_lasers)
        0 [0x40;0x41;0x42;0x43;0x44;0x45;0x46]
        0xa 0
    ;
  make_weapon "HYPER-V ROCKET" "2 SHOTS, +1 SPEED"
        6 6 7
        0 false false false
        0 1 1 2
        90 70 20
        false (tech_weap_to_enum Tech_weap_hyper_v_rockets)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "HYPER-V ROCKET" "5 SHOTS"
        6 6 5
        0 false false false
        0 1 1 5
        135 105 30
        false (tech_weap_to_enum Tech_weap_hyper_v_rockets)
        2 [0x50;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "GATLING LASER" "FIRES 4 TIMES/TURN"
        1 4 1
        0 false false false
        0 1 4 (-1)
        90 20 70
        false (tech_weap_to_enum Tech_weap_gatling_laser)
        0 [0x43;0x41;0x3f;0x25;0x3f;0x41;0x43]
        0x1 0
    ;
  make_weapon "NEUTRON PELLET GUN" "HALVES ENEMY SHIELDS"
        2 5 1
        0 true false false
        0 1 1 (-1)
        30 15 25
        false (tech_weap_to_enum Tech_weap_neutron_pellet_gun)
        0 [0x0;0xae;0x0;0x0;0x0;0xae;0x0]
        0x1 0
    ;
  make_weapon "HYPER-X ROCKET" "2 SHOTS, +1 TO HIT"
        8 8 7
        1 false false false
        0 1 1 2
        120 100 20
        false (tech_weap_to_enum Tech_weap_hyper_x_rockets)
        2 [0x60;0x0;0x0;0x6;0x50;0x0;0x0]
        0x8 1
    ;
  make_weapon "HYPER-X ROCKET" "5 SHOTS, +1 TO HIT"
        8 8 5
        1 false false false
        0 1 1 5
        180 150 30
        false (tech_weap_to_enum Tech_weap_hyper_x_rockets)
        2 [0x50;0x0;0x0;0x6;0x50;0x0;0x0]
        0x8 1
    ;
  make_weapon "FUSION BOMB" "GROUND ATTACKS ONLY"
        5 20 1
        0 false true false
        0 1 1 10
        30 50 10
        false (tech_weap_to_enum Tech_weap_fusion_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  make_weapon "ION CANNON" ""
        3 8 1
        0 false false false
        0 1 1 (-1)
        40 15 35
        false (tech_weap_to_enum Tech_weap_ion_cannon)
        1 [0xe4;0xe5;0xe6;0xe7;0xe6;0xe5;0xe4]
        0x10 0
    ;
  make_weapon "HEAVY ION CANNON" ""
        3 15 2
        0 false false false
        0 1 1 (-1)
        110 45 105
        false (tech_weap_to_enum Tech_weap_ion_cannon)
        1 [0xe4;0xe5;0xe6;0xe7;0xe6;0xe5;0xe4]
        0xf 0
    ;
  make_weapon "SCATTER PACK V" "2 SHOTS, MIRVS TO 5"
        6 6 7
        1 false false false
        0 1 1 2
        180 115 50
        false (tech_weap_to_enum Tech_weap_scatter_pack_v_rockets)
        2 [0x60;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 5
    ;
  make_weapon "SCATTER PACK V" "5 SHOTS, MIRVS TO 5"
        6 6 5
        1 false false false
        0 1 1 5
        270 170 80
        false (tech_weap_to_enum Tech_weap_scatter_pack_v_rockets)
        2 [0x50;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 5
    ;
  make_weapon "DEATH SPORES" "BIOLOGICAL WEAPON"
        1 1 1
        0 false true false
        0 1 1 5
        100 100 10
        true (tech_plan_to_enum Tech_plan_death_spores)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  make_weapon "MASS DRIVER" "HALVES ENEMY SHIELDS"
        5 8 1
        0 true false false
        0 1 1 (-1)
        90 55 50
        false (tech_weap_to_enum Tech_weap_mass_driver)
        0 [0x0;0xc;0x0;0x0;0x0;0xc;0x0]
        0x17 0
    ;
  make_weapon "MERCULITE MISSILE" "2 SHOTS, +2 TO HIT"
        10 10 8
        2 false false false
        0 1 1 2
        130 105 20
        false (tech_weap_to_enum Tech_weap_merculite_missiles)
        2 [0x80;0x0;0x0;0x4;0x78;0x0;0x0]
        0x8 1
    ;
  make_weapon "MERCULITE MISSILE" "5 SHOTS, +2 TO HIT"
        10 10 6
        2 false false false
        0 1 1 5
        195 155 30
        false (tech_weap_to_enum Tech_weap_merculite_missiles)
        2 [0x60;0x0;0x0;0x4;0x78;0x0;0x0]
        0x8 1
    ;
  make_weapon "NEUTRON BLASTER" ""
        3 12 1
        0 false false false
        0 1 1 (-1)
        60 20 60
        false (tech_weap_to_enum Tech_weap_neutron_blaster)
        0 [0xcf;0xce;0xcd;0xcc;0xcb;0xca;0xc9]
        0xa 0
    ;
  make_weapon "HEAVY BLAST CANNON" ""
        3 24 2
        0 false false false
        0 1 1 (-1)
        180 60 180
        false (tech_weap_to_enum Tech_weap_neutron_blaster)
        0 [0xcf;0xce;0xcd;0xcc;0xcb;0xca;0xc9]
        0x12 0
    ;
  make_weapon "ANTI-MATTER BOMB" "GROUND ATTACKS ONLY"
        10 40 1
        0 false true false
        0 1 1 10
        50 75 10
        false (tech_weap_to_enum Tech_weap_anti_matter_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  make_weapon "GRAVITON BEAM" "STREAMING ATTACK"
        1 15 1
        0 false false false
        0 1 1 (-1)
        60 20 50
        false (tech_weap_to_enum Tech_weap_graviton_beam)
        2 [0xd1;0xd2;0xd3;0xd4;0xd5;0xd6;0xd7]
        0x0 1
    ;
  make_weapon "STINGER MISSILE" "2 SHOTS, +3 TO HIT"
        15 15 9
        3 false false false
        0 1 1 2
        190 155 30
        false (tech_weap_to_enum Tech_weap_stinger_missiles)
        2 [0x90;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "STINGER MISSILE" "5 SHOTS, +3 TO HIT"
        15 15 7
        3 false false false
        0 1 1 5
        270 230 45
        false (tech_weap_to_enum Tech_weap_stinger_missiles)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "HARD BEAM" "HALVES SHIELD STR"
        8 12 1
        0 true false false
        0 1 1 (-1)
        120 50 100
        false (tech_weap_to_enum Tech_weap_hard_beam)
        0 [0xa1;0x96;0xa1;0x96;0xa1;0x96;0xa1]
        0x1 0
    ;
  make_weapon "FUSION BEAM" ""
        4 16 1
        0 false false false
        0 1 1 (-1)
        70 20 75
        false (tech_weap_to_enum Tech_weap_fusion_beam)
        0 [0xb7;0xb6;0xb5;0xb4;0xb3;0xb2;0xb1]
        0xb 0
    ;
  make_weapon "HEAVY FUSION BEAM" ""
        4 30 2
        0 false false false
        0 1 1 (-1)
        210 60 225
        false (tech_weap_to_enum Tech_weap_fusion_beam)
        0 [0xb7;0xb6;0xb5;0xb4;0xb3;0xb2;0xb1]
        0x19 0
    ;
  make_weapon "OMEGA-V BOMB" "GROUND ATTACKS ONLY"
        20 50 1
        0 false true false
        0 1 1 10
        80 140 10
        false (tech_weap_to_enum Tech_weap_omega_v_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  make_weapon "ANTI-MATTER TORP" "FIRES ONE PER 2 TURNS"
        30 30 8
        4 false false false
        1 1 1 (-1)
        300 75 300
        false (tech_weap_to_enum Tech_weap_anti_matter_torpedoes)
        2 [0x80;0x0;0x0;0x3;0xa0;0x0;0x0]
        0x11 1
    ;
  make_weapon "MEGABOLT CANNON" "+3 LEVELS TO HIT"
        2 20 1
        3 false false false
        0 1 1 (-1)
        80 30 65
        false (tech_weap_to_enum Tech_weap_megabolt_cannon)
        3 [0xaf;0xd7;0xae;0xd7;0xaf;0xd7;0xae]
        0x12 0
    ;
  make_weapon "PHASOR" ""
        5 20 1
        0 false false false
        0 1 1 (-1)
        90 20 90
        false (tech_weap_to_enum Tech_weap_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1c 0
    ;
  make_weapon "HEAVY PHASOR" ""
        5 40 2
        0 false false false
        0 1 1 (-1)
        260 60 270
        false (tech_weap_to_enum Tech_weap_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1a 0
    ;
  make_weapon "SCATTER PACK VII" "2 SHOTS, MIRVS TO 7"
        10 10 8
        2 false false false
        0 1 1 2
        280 170 50
        false (tech_weap_to_enum Tech_weap_scatter_pack_vii_missiles)
        2 [0x80;0x0;0x0;0x4;0x78;0x0;0x0]
        0x3 7
    ;
  make_weapon "SCATTER PACK VII" "5 SHOTS, MIRVS TO 7"
        10 10 6
        2 false false false
        0 1 1 5
        420 230 80
        false (tech_weap_to_enum Tech_weap_scatter_pack_vii_missiles)
        2 [0x60;0x0;0x0;0x4;0x78;0x0;0x0]
        0x3 7
    ;
  make_weapon "DOOM VIRUS" "BIOLOGICAL WEAPON"
        2 2 1
        0 false true false
        0 1 1 5
        150 200 10
        true (tech_plan_to_enum Tech_plan_doom_virus)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x25 0
    ;
  make_weapon "AUTO BLASTER" "FIRES 3 TIMES/TURN"
        4 16 1
        0 false false false
        0 1 3 (-1)
        140 30 90
        false (tech_weap_to_enum Tech_weap_auto_blaster)
        1 [0xbe;0xbd;0xbc;0xbb;0xba;0xb9;0xb8]
        0x1 0
    ;
  make_weapon "PULSON MISSILE" "2 SHOTS, +4 TO HIT"
        20 20 10
        4 false false false
        0 1 1 2
        200 160 40
        false (tech_weap_to_enum Tech_weap_pulson_missiles)
        2 [0xa0;0x0;0x0;0x4;0xa0;0x0;0x0]
        0x8 1
    ;
  make_weapon "PULSON MISSILE" "5 SHOTS, +4 TO HIT"
        20 20 8
        4 false false false
        0 1 1 5
        300 240 60
        false (tech_weap_to_enum Tech_weap_pulson_missiles)
        2 [0x80;0x0;0x0;0x4;0xa0;0x0;0x0]
        0x8 1
    ;
  make_weapon "TACHYON BEAM" "STREAMING ATTACK"
        1 25 1
        0 false false false
        0 1 1 (-1)
        90 30 70
        false (tech_weap_to_enum Tech_weap_tachyon_beam)
        2 [0x8e;0x8c;0x8b;0x8a;0x89;0x88;0x87]
        0x1f 1
    ;
  make_weapon "GAUSS AUTOCANON" "1/2 SHIELDS, FIRES 4"
        7 10 1
        0 true false false
        0 1 4 (-1)
        280 105 105
        false (tech_weap_to_enum Tech_weap_gauss_autocannon)
        0 [0x0;0xf;0x0;0x12;0x0;0x15;0x0]
        0x1 0
    ;
  make_weapon "PARTICLE BEAM" "HALVES SHIELD STR"
        10 20 1
        0 true false false
        0 1 1 (-1)
        150 90 75
        false (tech_weap_to_enum Tech_weap_particle_beam)
        0 [0x0;0xf;0x0;0x12;0x0;0x15;0x0]
        0x21 0
    ;
  make_weapon "HERCULAR MISSILE" "2 SHOTS, +5 TO HIT"
        25 25 10
        5 false false false
        0 1 1 2
        260 220 40
        false (tech_weap_to_enum Tech_weap_hercular_missiles)
        2 [0xb0;0x0;0x0;0x6;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "HERCULAR MISSILE" "5 SHOTS, +5 TO HIT"
        25 25 9
        5 false false false
        0 1 1 5
        390 330 60
        false (tech_weap_to_enum Tech_weap_hercular_missiles)
        2 [0x90;0x0;0x0;0x6;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "PLASMA CANNON" ""
        6 30 1
        0 false false false
        0 1 1 (-1)
        120 30 100
        false (tech_weap_to_enum Tech_weap_plasma_cannon)
        2 [0x46;0x45;0x44;0x43;0x44;0x45;0x46]
        0x1e 0
    ;
  make_weapon "DISRUPTOR" ""
        10 40 2
        0 false false false
        0 1 1 (-1)
        210 70 160
        false (tech_weap_to_enum Tech_weap_disruptor)
        0 [0xf7;0xf6;0xf5;0xf4;0xf3;0xf2;0xf1]
        0x1c 0
    ;
  make_weapon "PULSE PHASOR" "FIRES 3 TIMES/TURN"
        5 20 1
        0 false false false
        0 1 3 (-1)
        250 40 120
        false (tech_weap_to_enum Tech_weap_pulse_phasor)
        1 [0xdb;0xdc;0xdd;0xde;0xdd;0xdc;0xdb]
        0x1 0
    ;
  make_weapon "NEUTRONIUM BOMB" "GROUND ATTACKS ONLY"
        40 70 1
        0 false true false
        0 1 1 10
        90 200 10
        false (tech_weap_to_enum Tech_weap_neutronium_bomb)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x14 0
    ;
  make_weapon "BIO TERMINATOR" "BIOLOGICAL WEAPON"
        3 3 1
        0 false true false
        0 1 1 5
        200 300 10
        true (tech_plan_to_enum Tech_plan_bio_terminator)
        0 [0x0;0x0;0x0;0x0;0x0;0x0;0x0]
        0x14 0
    ;
  make_weapon "HELLFIRE TORPEDO" "HITS ALL FOUR SHIELDS"
        25 25 10
        6 false false false
        2 4 1 (-1)
        500 150 350
        false (tech_weap_to_enum Tech_weap_hellfire_torpedoes)
        2 [0xa0;0x0;0x0;0x4;0x8c;0x0;0x0]
        0x11 1
    ;
  make_weapon "ZEON MISSILE" "2 SHOTS, +6 TO HIT"
        30 30 9
        6 false false false
        0 1 1 2
        300 250 50
        false (tech_weap_to_enum Tech_weap_zeon_missiles)
        2 [0xc0;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "ZEON MISSILE" "5 SHOTS, +6 TO HIT"
        30 30 7
        6 false false false
        0 1 1 5
        450 375 75
        false (tech_weap_to_enum Tech_weap_zeon_missiles)
        2 [0xa0;0x0;0x0;0x5;0x64;0x0;0x0]
        0x8 1
    ;
  make_weapon "PROTON TORPEDO" "FIRES ONE PER 2 TURNS"
        75 75 10
        6 false false false
        3 1 1 (-1)
        500 100 400
        false (tech_weap_to_enum Tech_weap_proton_torpedoes)
        2 [0xff;0x0;0x0;0x3;0xc8;0x0;0x0]
        0x11 1
    ;
  make_weapon "SCATTER PACK X" "2 SHOTS, MIRVS TO 10"
        15 15 10
        3 false false false
        0 1 1 2
        300 250 50
        false (tech_weap_to_enum Tech_weap_scatter_pack_x_missiles)
        2 [0x90;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 10
    ;
  make_weapon "SCATTER PACK X" "5 SHOTS, MIRVS TO 10"
        15 15 10
        3 false false false
        0 1 1 5
        450 420 80
        false (tech_weap_to_enum Tech_weap_scatter_pack_x_missiles)
        2 [0x70;0x0;0x0;0x5;0x64;0x0;0x0]
        0x3 10
    ;
  make_weapon "TRI-FOCUS PLASMA" ""
        20 50 1
        0 false false false
        0 1 1 (-1)
        250 70 180
        false (tech_weap_to_enum Tech_weap_tri_focus_plasma_cannon)
        1 [0x46;0x45;0x44;0x43;0x44;0x45;0x46]
        0x1b 0
    ;
  make_weapon "STELLAR CONVERTER" "HITS ALL FOUR SHIELDS"
        10 35 3
        0 false false false
        0 4 1 (-1)
        500 200 300
        false (tech_weap_to_enum Tech_weap_stellar_converter)
        3 [0x49;0x56;0x71;0x46;0x49;0x56;0x71]
        0x15 0
    ;
  make_weapon "MAULER DEVICE" "CRUEL BRUTAL DAMAGE"
        20 100 1
        0 false false false
        0 1 1 (-1)
        550 150 300
        false (tech_weap_to_enum Tech_weap_mauler_device)
        4 [0xbb;0xbc;0xbd;0xbd;0xbd;0xbc;0xbb]
        0x23 0
    ;
  make_weapon "PLASMA TORPEDO" "LOSES 15 DAMAGE/HEX"
        150 150 10
        7 false false true
        4 1 1 (-1)
        600 150 450
        false (tech_weap_to_enum Tech_weap_plasma_torpedoes)
        2 [0xc0;0x0;0x0;0x3;0xa0;0x0;0x0]
        0x11 1
    ;
  make_weapon "CRYSTAL RAY" ""
        100 300 3
        0 false false false
        0 4 1 (-1)
        600 200 400
        false 101
        3 [0xe;0xc7;0xe;0xe;0xef;0xe;0xc7]
        0x18 0
    ;
  make_weapon "DEATH RAY" ""
        200 1000 1
        0 false false false
        0 1 1 (-1)
        1000 2000 2000
        false (tech_weap_to_enum Tech_weap_death_ray)
        4 [0xcb;0xc5;0xc4;0x46;0xc4;0xc5;0xcb]
        0x1d 0
    ;
  make_weapon "AMEOBA STREAM" ""
        250 1000 3
        0 false false false
        0 1 1 (-1)
        600 200 400
        false 101
        2 [0x46;0x46;0xdc;0xdc;0xdc;0xd3;0xd3]
        0xc 1
|]

(*
struct shiptech_comp_s tbl_shiptech_comp[SHIP_COMP_NUM] = {
    { &game_str_st_none, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, 0, 0 },
    { &game_str_tbl_st_comp[0], { 5, 10, 20, 100 }, { 5, 10, 20, 100 }, { 40, 200, 1000, 5000 }, 1, 1 },
    { &game_str_tbl_st_comp[1], { 7, 15, 30, 150 }, { 7, 15, 30, 150 }, { 50, 240, 1200, 6000 }, 5, 2 },
    { &game_str_tbl_st_comp[2], { 10, 20, 40, 200 }, { 10, 20, 40, 200 }, { 60, 280, 1400, 7000 }, 10, 3 },
    { &game_str_tbl_st_comp[3], { 12, 25, 50, 250 }, { 12, 25, 50, 250 }, { 70, 320, 1600, 8000 }, 15, 4 },
    { &game_str_tbl_st_comp[4], { 15, 30, 60, 300 }, { 15, 30, 60, 300 }, { 80, 360, 1800, 9000 }, 20, 5 },
    { &game_str_tbl_st_comp[5], { 17, 35, 70, 350 }, { 17, 35, 70, 350 }, { 90, 400, 2000, 10000 }, 25, 6 },
    { &game_str_tbl_st_comp[6], { 20, 40, 80, 400 }, { 20, 40, 80, 400 }, { 100, 440, 2200, 11000 }, 30, 7 },
    { &game_str_tbl_st_comp[7], { 22, 45, 90, 450 }, { 22, 45, 90, 450 }, { 110, 480, 2400, 12000 }, 35, 8 },
    { &game_str_tbl_st_comp[8], { 25, 50, 100, 500 }, { 25, 50, 100, 500 }, { 120, 520, 2600, 13000 }, 40, 9 },
    { &game_str_tbl_st_comp[9], { 27, 55, 110, 550 }, { 27, 55, 110, 550 }, { 130, 560, 2800, 14000 }, 45, 10 },
    { &game_str_tbl_st_comp[10], { 30, 60, 120, 600 }, { 30, 60, 120, 600 }, { 140, 600, 3000, 15000 }, 50, 11 }
};

struct shiptech_engine_s tbl_shiptech_engine[SHIP_ENGINE_NUM] = {
    { &game_str_tbl_st_engine[0], 10, 10, 20, 1, 1 },
    { &game_str_tbl_st_engine[1], 20, 18, 40, 2, 6 },
    { &game_str_tbl_st_engine[2], 30, 26, 60, 3, 12 },
    { &game_str_tbl_st_engine[3], 40, 33, 80, 4, 18 },
    { &game_str_tbl_st_engine[4], 50, 36, 100, 5, 24 },
    { &game_str_tbl_st_engine[5], 60, 40, 120, 6, 30 },
    { &game_str_tbl_st_engine[6], 70, 44, 140, 7, 36 },
    { &game_str_tbl_st_engine[7], 80, 47, 160, 8, 42 },
    { &game_str_tbl_st_engine[8], 90, 50, 180, 9, 48 }
};

struct shiptech_armor_s tbl_shiptech_armor[SHIP_ARMOR_NUM] = {
    { &game_str_tbl_st_armor[0], { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, 100, 1 },
    { &game_str_tbl_st_armor[1], { 20, 100, 500, 2500 }, { 20, 80, 400, 2000 }, 150, 1 },
    { &game_str_tbl_st_armor[2], { 20, 100, 600, 3000 }, { 2, 10, 60, 300 }, 150, 10 },
    { &game_str_tbl_st_armor[3], { 30, 150, 900, 4500 }, { 25, 85, 425, 2100 }, 225, 10 },
    { &game_str_tbl_st_armor[4], { 40, 200, 1000, 5000 }, { 4, 20, 100, 500 }, 200, 17 },
    { &game_str_tbl_st_armor[5], { 60, 300, 1500, 7500 }, { 30, 100, 500, 2500 }, 300, 17 },
    { &game_str_tbl_st_armor[6], { 60, 300, 1500, 7500 }, { 6, 30, 150, 750 }, 250, 26 },
    { &game_str_tbl_st_armor[7], { 90, 450, 2250, 11250 }, { 35, 115, 575, 2875 }, 375, 26 },
    { &game_str_tbl_st_armor[8], { 80, 400, 2000, 10000 }, { 8, 40, 200, 1000 }, 300, 34 },
    { &game_str_tbl_st_armor[9], { 120, 600, 3000, 15000 }, { 40, 130, 650, 3250 }, 450, 34 },
    { &game_str_tbl_st_armor[10], { 100, 500, 2500, 12500 }, { 10, 50, 250, 1250 }, 350, 42 },
    { &game_str_tbl_st_armor[11], { 150, 750, 3750, 18750 }, { 45, 150, 750, 3750 }, 525, 42 },
    { &game_str_tbl_st_armor[12], { 120, 600, 3000, 15000 }, { 12, 60, 300, 1500 }, 400, 50 },
    { &game_str_tbl_st_armor[13], { 180, 900, 4500, 25000 }, { 50, 175, 875, 4375 }, 600, 50 }
};

struct shiptech_shield_s tbl_shiptech_shield[SHIP_SHIELD_NUM] = {
    { &game_str_st_none, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, 0, 0 },
    { &game_str_tbl_st_shield[0], { 30, 190, 1200, 7500 }, { 5, 20, 60, 250 }, { 5, 20, 60, 250 }, 1, 1 },
    { &game_str_tbl_st_shield[1], { 35, 220, 1400, 8750 }, { 10, 35, 90, 375 }, { 10, 35, 90, 375 }, 2, 4 },
    { &game_str_tbl_st_shield[2], { 40, 250, 1600, 10000 }, { 15, 50, 120, 500 }, { 15, 50, 120, 500 }, 3, 10 },
    { &game_str_tbl_st_shield[3], { 45, 280, 1800, 11250 }, { 20, 65, 150, 675 }, { 20, 65, 150, 675 }, 4, 14 },
    { &game_str_tbl_st_shield[4], { 50, 310, 2000, 12500 }, { 25, 80, 180, 750 }, { 25, 80, 180, 750 }, 5, 20 },
    { &game_str_tbl_st_shield[5], { 55, 340, 2200, 13750 }, { 30, 95, 210, 875 }, { 30, 95, 210, 875 }, 6, 24 },
    { &game_str_tbl_st_shield[6], { 60, 370, 2400, 15000 }, { 35, 110, 240, 1000 }, { 35, 110, 240, 1000 }, 7, 30 },
    { &game_str_tbl_st_shield[7], { 65, 400, 2600, 16250 }, { 40, 125, 270, 1125 }, { 40, 125, 270, 1125 }, 9, 34 },
    { &game_str_tbl_st_shield[8], { 70, 430, 2800, 17500 }, { 45, 140, 300, 1250 }, { 45, 140, 300, 1250 }, 11, 40 },
    { &game_str_tbl_st_shield[9], { 80, 460, 3000, 18750 }, { 50, 155, 330, 1375 }, { 50, 155, 330, 1375 }, 13, 44 },
    { &game_str_tbl_st_shield[10], { 90, 490, 3200, 20000 }, { 55, 160, 360, 1500 }, { 55, 160, 360, 1500 }, 15, 50 }
/*
    { NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    { NULL, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
*/
};

struct shiptech_jammer_s tbl_shiptech_jammer[SHIP_JAMMER_NUM] = {
    { &game_str_st_none, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, 0, 0 },
    { &game_str_tbl_st_jammer[0], { 10, 20, 40, 170 }, { 10, 20, 40, 170 }, { 25, 150, 1000, 6250 }, 2, 1 },
    { &game_str_tbl_st_jammer[1], { 15, 30, 60, 250 }, { 15, 30, 60, 250 }, { 27, 165, 1100, 6875 }, 7, 2 },
    { &game_str_tbl_st_jammer[2], { 20, 40, 80, 330 }, { 20, 40, 80, 330 }, { 30, 180, 1200, 7500 }, 12, 3 },
    { &game_str_tbl_st_jammer[3], { 25, 50, 100, 410 }, { 25, 50, 100, 410 }, { 32, 195, 1300, 8125 }, 17, 4 },
    { &game_str_tbl_st_jammer[4], { 30, 60, 120, 490 }, { 30, 60, 120, 490 }, { 35, 210, 1400, 8750 }, 22, 5 },
    { &game_str_tbl_st_jammer[5], { 35, 70, 140, 570 }, { 35, 70, 140, 570 }, { 37, 225, 1500, 9375 }, 27, 6 },
    { &game_str_tbl_st_jammer[6], { 40, 80, 160, 650 }, { 40, 80, 160, 650 }, { 40, 240, 1600, 10000 }, 32, 7 },
    { &game_str_tbl_st_jammer[7], { 45, 90, 180, 730 }, { 45, 90, 180, 730 }, { 42, 255, 1700, 10625 }, 37, 8 },
    { &game_str_tbl_st_jammer[8], { 50, 100, 200, 810 }, { 50, 100, 200, 810 }, { 45, 270, 1800, 11250 }, 42, 9 },
    { &game_str_tbl_st_jammer[9], { 55, 110, 220, 900 }, { 55, 110, 220, 900 }, { 50, 285, 1900, 11875 }, 47, 10 }
};

struct shiptech_special_s tbl_shiptech_special[SHIP_SPECIAL_NUM] = {
    { /* NONE */
        &game_str_st_none, &strempty,
        { 0, 0, 0, 0 },
        { 0, 0, 0, 0 },
        { 0, 0, 0, 0 },
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* RESERVE FUEL TANKS */
        &game_str_tbl_st_special[0], &game_str_tbl_st_specialx[0],
        { 20, 100, 500, 2500 },
        { 20, 100, 500, 2500 },
        { 0, 0, 0, 0 },
        TECH_CONS_RESERVE_FUEL_TANKS, TECH_FIELD_CONSTRUCTION, 1,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* STANDARD COLONY BASE */
        &game_str_tbl_st_special[1], &game_str_tbl_st_specialx[1],
        { 3500, 3500, 3500, 3500 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_ECOLOGICAL_RESTORATION, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* BARREN COLONY BASE */
        &game_str_tbl_st_special[2], &game_str_tbl_st_specialx[2],
        { 3750, 3750, 3750, 3750 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_BARREN_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* TUNDRA COLONY BASE */
        &game_str_tbl_st_special[3], &game_str_tbl_st_specialx[3],
        { 4000, 4000, 4000, 4000 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_TUNDRA_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* DEAD COLONY BASE */
        &game_str_tbl_st_special[4], &game_str_tbl_st_specialx[4],
        { 4250, 4250, 4250, 4250 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_DEAD_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* INFERNO COLONY BASE */
        &game_str_tbl_st_special[5], &game_str_tbl_st_specialx[5],
        { 4500, 4500, 4500, 4500 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_INFERNO_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* TOXIC COLONY BASE */
        &game_str_tbl_st_special[6], &game_str_tbl_st_specialx[6],
        { 4750, 4750, 4750, 4750 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_TOXIC_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* RADIATED COLONY BASE */
        &game_str_tbl_st_special[7], &game_str_tbl_st_specialx[7],
        { 5000, 5000, 5000, 5000 },
        { 700, 700, 700, 700 },
        { 0, 0, 0, 0 },
        TECH_PLAN_CONTROLLED_RADIATED_ENVIRONMENT, TECH_FIELD_PLANETOLOGY, 2,
        0, 0, 0, 0, 0, 0,
        0
    },
    { /* BATTLE SCANNER */
        &game_str_tbl_st_special[8], &game_str_tbl_st_specialx[8],
        { 300, 300, 300, 300 },
        { 50, 50, 50, 50 },
        { 50, 50, 50, 50 },
        TECH_COMP_BATTLE_SCANNER, TECH_FIELD_COMPUTER, 3,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_SCANNER)
    },
    { /* ANTI-MISSILE ROCKETS */
        &game_str_tbl_st_special[9], &game_str_tbl_st_specialx[9],
        { 100, 100, 100, 100 },
        { 2, 10, 50, 250 },
        { 8, 40, 200, 1000 },
        TECH_WEAP_ANTI_MISSILE_ROCKETS, TECH_FIELD_WEAPON, 4,
        0, 0, 40, 0, 0, 0,
        0
    },
    { /* REPULSOR BEAM */
        &game_str_tbl_st_special[10], &game_str_tbl_st_specialx[10],
        { 550, 550, 550, 550 },
        { 100, 100, 100, 100 },
        { 200, 200, 200, 200 },
        TECH_FFLD_REPULSOR_BEAM, TECH_FIELD_FORCE_FIELD, 5,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_REPULSOR)
    },
    { /* WARP DISSIPATOR */
        &game_str_tbl_st_special[11], &game_str_tbl_st_specialx[11],
        { 650, 650, 650, 650 },
        { 100, 100, 100, 100 },
        { 300, 300, 300, 300 },
        TECH_PROP_WARP_DISSIPATOR, TECH_FIELD_PROPULSION, 6,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_WARPDIS)
    },
    { /* ENERGY PULSAR */
        &game_str_tbl_st_special[12], &game_str_tbl_st_specialx[12],
        { 750, 750, 750, 750 },
        { 150, 150, 150, 150 },
        { 250, 250, 250, 250 },
        TECH_PROP_ENERGY_PULSAR, TECH_FIELD_PROPULSION, 7,
        0, 0, 0, 0, 1, 0,
        0
    },
    { /* INERTIAL STABILIZER */
        &game_str_tbl_st_special[13], &game_str_tbl_st_specialx[13],
        { 20, 75, 500, 2700 },
        { 4, 20, 100, 500 },
        { 8, 40, 200, 1000 },
        TECH_PROP_INERTIAL_STABILIZER, TECH_FIELD_PROPULSION, 8,
        0, 2, 0, 0, 0, 0,
        0
    },
    { /* ZYRO SHIELD */
        &game_str_tbl_st_special[14], &game_str_tbl_st_specialx[14],
        { 50, 100, 200, 300 },
        { 4, 20, 100, 500 },
        { 12, 60, 300, 1500 },
        TECH_FFLD_ZYRO_SHIELD, TECH_FIELD_FORCE_FIELD, 4,
        0, 0, 75, 0, 0, 0,
        0
    },
    { /* AUTOMATED REPAIR */
        &game_str_tbl_st_special[15], &game_str_tbl_st_specialx[15],
        { 2, 8, 50, 300 },
        { 3, 15, 100, 600 },
        { 3, 10, 50, 300 },
        TECH_CONS_AUTOMATED_REPAIR_SYSTEM, TECH_FIELD_CONSTRUCTION, 9,
        15, 0, 0, 0, 0, 0,
        0
    },
    { /* STASIS FIELD */
        &game_str_tbl_st_special[16], &game_str_tbl_st_specialx[16],
        { 2500, 2500, 2500, 2500 },
        { 200, 200, 200, 200 },
        { 275, 275, 275, 275 },
        TECH_FFLD_STASIS_FIELD, TECH_FIELD_FORCE_FIELD, 10,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_STASIS)
    },
    { /* CLOAKING DEVICE */
        &game_str_tbl_st_special[17], &game_str_tbl_st_specialx[17],
        { 30, 150, 750, 3750 },
        { 5, 25, 120, 600 },
        { 10, 50, 250, 1250 },
        TECH_FFLD_CLOAKING_DEVICE, TECH_FIELD_FORCE_FIELD, 11,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_CLOAK)
    },
    { /* ION STREAM PROJECTOR */
        &game_str_tbl_st_special[18], &game_str_tbl_st_specialx[18],
        { 1000, 1000, 1000, 1000 },
        { 250, 250, 250, 250 },
        { 500, 500, 500, 500 },
        TECH_WEAP_ION_STREAM_PROJECTOR, TECH_FIELD_WEAPON, 12,
        0, 0, 0, 0, 0, 1,
        0
    },
    { /* HIGH ENERGY FOCUS */
        &game_str_tbl_st_special[19], &game_str_tbl_st_specialx[19],
        { 30, 135, 625, 3500 },
        { 35, 100, 150, 500 },
        { 65, 200, 350, 1000 },
        TECH_PROP_HIGH_ENERGY_FOCUS, TECH_FIELD_PROPULSION, 13,
        0, 0, 0, 3, 0, 0,
        0
    },
    { /* IONIC PULSAR */
        &game_str_tbl_st_special[20], &game_str_tbl_st_specialx[20],
        { 1500, 1500, 1500, 1500 },
        { 400, 400, 400, 400 },
        { 750, 750, 750, 750 },
        TECH_PROP_IONIC_PULSAR, TECH_FIELD_PROPULSION, 7,
        0, 0, 0, 0, 2, 0,
        0
    },
    { /* BLACK HOLE GENERATOR */
        &game_str_tbl_st_special[21], &game_str_tbl_st_specialx[21],
        { 2750, 2750, 2750, 2750 },
        { 750, 750, 750, 750 },
        { 750, 750, 750, 750 },
        TECH_FFLD_BLACK_HOLE_GENERATOR, TECH_FIELD_FORCE_FIELD, 14,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_BLACKHOLE)
    },
    { /* SUB SPACE TELEPORTER */
        &game_str_tbl_st_special[22], &game_str_tbl_st_specialx[22],
        { 25, 100, 450, 2250 },
        { 4, 20, 100, 500 },
        { 16, 80, 400, 2000 },
        TECH_PROP_SUB_SPACE_TELEPORTER, TECH_FIELD_PROPULSION, 15,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_SUBSPACE)
    },
    { /* LIGHTNING SHIELD */
        &game_str_tbl_st_special[23], &game_str_tbl_st_specialx[23],
        { 200, 300, 400, 500 },
        { 6, 30, 150, 750 },
        { 15, 70, 350, 1750 },
        TECH_FFLD_LIGHTNING_SHIELD, TECH_FIELD_FORCE_FIELD, 4,
        0, 0, 100, 0, 0, 0,
        0
    },
    { /* NEUTRON STREAM PROJECTOR */
        &game_str_tbl_st_special[24], &game_str_tbl_st_specialx[24],
        { 2000, 2000, 2000, 2000 },
        { 500, 500, 500, 500 },
        { 1250, 1250, 1250, 1250 },
        TECH_WEAP_NEUTRON_STREAM_PROJECTOR, TECH_FIELD_WEAPON, 16,
        0, 0, 0, 0, 0, 2,
        0
    },
    { /* ADV DAMAGE CONTROL */
        &game_str_tbl_st_special[25], &game_str_tbl_st_specialx[25],
        { 40, 200, 1000, 5000 },
        { 9, 45, 300, 1800 },
        { 9, 30, 150, 450 },
        TECH_CONS_ADVANCED_DAMAGE_CONTROL, TECH_FIELD_CONSTRUCTION, 9,
        30, 0, 0, 0, 0, 0,
        0
    },
    { /* TECHNOLOGY NULLIFIER */
        &game_str_tbl_st_special[26], &game_str_tbl_st_specialx[26],
        { 3000, 3000, 3000, 3000 },
        { 750, 750, 750, 750 },
        { 1000, 1000, 1000, 1000 },
        TECH_COMP_TECHNOLOGY_NULLIFIER, TECH_FIELD_COMPUTER, 17,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_TECHNULL)
    },
    { /* INERTIAL NULLIFIER */
        &game_str_tbl_st_special[27], &game_str_tbl_st_specialx[27],
        { 60, 200, 1500, 5000 },
        { 6, 30, 150, 750 },
        { 12, 60, 300, 1500 },
        TECH_PROP_INERTIAL_NULLIFIER, TECH_FIELD_PROPULSION, 8,
        0, 4, 0, 0, 0, 0,
        0
    },
    { /* ORACLE INTERFACE */
        &game_str_tbl_st_special[28], &game_str_tbl_st_specialx[28],
        { 30, 150, 600, 2750 },
        { 8, 40, 200, 1000 },
        { 12, 60, 300, 1500 },
        TECH_COMP_ORACLE_INTERFACE, TECH_FIELD_COMPUTER, 20,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_ORACLE)
    },
    { /* DISPLACMENT DEVICE */
        &game_str_tbl_st_special[29], &game_str_tbl_st_specialx[29],
        { 30, 150, 300, 2750 },
        { 10, 50, 225, 1250 },
        { 10, 50, 225, 1250 },
        TECH_PROP_DISPLACEMENT_DEVICE, TECH_FIELD_PROPULSION, 21,
        0, 0, 0, 0, 0, 0,
        (1 << SHIP_SPECIAL_BOOL_DISP)
    }
};

struct shiptech_hull_s tbl_shiptech_hull[SHIP_HULL_NUM] = {
    { &game_str_tbl_st_hull[0], 60, 40, 3, 2, 2 },
    { &game_str_tbl_st_hull[1], 360, 200, 18, 15, 1 },
    { &game_str_tbl_st_hull[2], 2000, 1000, 100, 100, 0 },
    { &game_str_tbl_st_hull[3], 12000, 5000, 600, 700, -1 }
};
*)


