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
  make_weapon "NONE" " "
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
    false (tech_weap_to_enum @@ Weap_lasers)
    0 [0x0; 0x0; 0x0; 0x0; 0x0; 0x0; 0x0]
    0x25 0
  ;





|]
