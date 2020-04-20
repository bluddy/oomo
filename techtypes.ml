open Types

type tech_comp =
  | Tech_comp_none
  | Tech_comp_battle_scanner
  | Tech_comp_ecm_jammer_mark_i
  | Tech_comp_unused_2
  | Tech_comp_deep_space_scanner
  | Tech_comp_battle_computer_mark_ii
  | Tech_comp_unused_5
  | Tech_comp_ecm_jammer_mark_ii
  | Tech_comp_improved_robotic_controls_iii
  | Tech_comp_unused_8
  | Tech_comp_battle_computer_mark_iii
  | Tech_comp_unused_10
  | Tech_comp_ecm_jammer_mark_iii
  | Tech_comp_improved_space_scanner
  | Tech_comp_unused_13
  | Tech_comp_battle_computer_mark_iv
  | Tech_comp_unused_15
  | Tech_comp_ecm_jammer_mark_iv
  | Tech_comp_improved_robotic_controls_iv
  | Tech_comp_unused_18
  | Tech_comp_battle_computer_mark_v
  | Tech_comp_unused_20
  | Tech_comp_ecm_jammer_mark_v
  | Tech_comp_advanced_space_scanner
  | Tech_comp_unused_23
  | Tech_comp_battle_computer_mark_vi
  | Tech_comp_unused_25
  | Tech_comp_ecm_jammer_mark_vi
  | Tech_comp_improved_robotic_controls_v
  | Tech_comp_unused_28
  | Tech_comp_battle_computer_mark_vii
  | Tech_comp_unused_30
  | Tech_comp_ecm_jammer_mark_vii
  | Tech_comp_unused_32
  | Tech_comp_hyperspace_communications
  | Tech_comp_battle_computer_mark_viii
  | Tech_comp_unused_35
  | Tech_comp_ecm_jammer_mark_viii
  | Tech_comp_improved_robotic_controls_vi
  | Tech_comp_unused_38
  | Tech_comp_battle_computer_mark_ix
  | Tech_comp_unused_40
  | Tech_comp_ecm_jammer_mark_ix
  | Tech_comp_unused_42
  | Tech_comp_unused_43
  | Tech_comp_battle_computer_mark_x
  | Tech_comp_oracle_interface
  | Tech_comp_ecm_jammer_mark_x
  | Tech_comp_improved_robotic_controls_vii
  | Tech_comp_technology_nullifier
  | Tech_comp_battle_computer_mark_xi
  [@@deriving enum]

type tech_cons =
  | Tech_cons_none
  | Tech_cons_reserve_fuel_tanks
  | Tech_cons_unused_1
  | Tech_cons_improved_industrial_tech_9
  | Tech_cons_unused_3
  | Tech_cons_reduced_industrial_waste_80
  | Tech_cons_unused_5
  | Tech_cons_unused_6
  | Tech_cons_improved_industrial_tech_8
  | Tech_cons_unused_8
  | Tech_cons_duralloy_armor
  | Tech_cons_battle_suits
  | Tech_cons_unused_11
  | Tech_cons_improved_industrial_tech_7
  | Tech_cons_automated_repair_system
  | Tech_cons_reduced_industrial_waste_60
  | Tech_cons_unused_15
  | Tech_cons_zortium_armor
  | Tech_cons_improved_industrial_tech_6
  | Tech_cons_unused_18
  | Tech_cons_unused_19
  | Tech_cons_unused_20
  | Tech_cons_unused_21
  | Tech_cons_improved_industrial_tech_5
  | Tech_cons_armored_exoskeleton
  | Tech_cons_reduced_industrial_waste_40
  | Tech_cons_andrium_armor
  | Tech_cons_unused_26
  | Tech_cons_improved_industrial_tech_4
  | Tech_cons_unused_28
  | Tech_cons_unused_29
  | Tech_cons_unused_30
  | Tech_cons_unused_31
  | Tech_cons_improved_industrial_tech_3
  | Tech_cons_tritanium_armor
  | Tech_cons_reduced_industrial_waste_20
  | Tech_cons_advanced_damage_control
  | Tech_cons_unused_36
  | Tech_cons_improved_industrial_tech_2
  | Tech_cons_unused_38
  | Tech_cons_powered_armor
  | Tech_cons_unused_40
  | Tech_cons_adamantium_armor
  | Tech_cons_unused_42
  | Tech_cons_unused_43
  | Tech_cons_industrial_waste_elimination
  | Tech_cons_unused_45
  | Tech_cons_unused_46
  | Tech_cons_unused_47
  | Tech_cons_unused_48
  | Tech_cons_neutronium_armor
  [@@deriving enum]

type tech_ffld =
  | Tech_ffld_none
  | Tech_ffld_class_i_deflector_shields
  | Tech_ffld_unused_1
  | Tech_ffld_unused_2
  | Tech_ffld_class_ii_deflector_shields
  | Tech_ffld_unused_4
  | Tech_ffld_unused_5
  | Tech_ffld_unused_6
  | Tech_ffld_personal_deflector_shield
  | Tech_ffld_unused_8
  | Tech_ffld_class_iii_deflector_shields
  | Tech_ffld_unused_10
  | Tech_ffld_class_v_planetary_shield
  | Tech_ffld_unused_12
  | Tech_ffld_class_iv_deflector_shields
  | Tech_ffld_unused_14
  | Tech_ffld_repulsor_beam
  | Tech_ffld_unused_16
  | Tech_ffld_unused_17
  | Tech_ffld_unused_18
  | Tech_ffld_class_v_deflector_shields
  | Tech_ffld_personal_absorption_shield
  | Tech_ffld_class_x_planetary_shield
  | Tech_ffld_unused_22
  | Tech_ffld_class_vi_deflector_shields
  | Tech_ffld_unused_24
  | Tech_ffld_unused_25
  | Tech_ffld_cloaking_device
  | Tech_ffld_unused_27
  | Tech_ffld_unused_28
  | Tech_ffld_class_vii_deflector_shields
  | Tech_ffld_zyro_shield
  | Tech_ffld_class_xv_planetary_shield
  | Tech_ffld_unused_32
  | Tech_ffld_class_ix_deflector_shields
  | Tech_ffld_unused_34
  | Tech_ffld_unused_35
  | Tech_ffld_stasis_field
  | Tech_ffld_personal_barrier_shield
  | Tech_ffld_unused_38
  | Tech_ffld_class_xi_deflector_shields
  | Tech_ffld_unused_40
  | Tech_ffld_class_xx_planetary_shield
  | Tech_ffld_black_hole_generator
  | Tech_ffld_class_xiii_deflector_shields
  | Tech_ffld_unused_44
  | Tech_ffld_lightning_shield
  | Tech_ffld_unused_46
  | Tech_ffld_unused_47
  | Tech_ffld_unused_48
  | Tech_ffld_class_xv_deflector_shields
  [@@deriving enum]

type tech_plan =
  | Tech_plan_none
  | Tech_plan_ecological_restoration
  | Tech_plan_improved_terraforming_10
  | Tech_plan_controlled_barren_environment
  | Tech_plan_unused_3
  | Tech_plan_improved_eco_restoration
  | Tech_plan_controlled_tundra_environment
  | Tech_plan_unused_6
  | Tech_plan_improved_terraforming_20
  | Tech_plan_controlled_dead_environment
  | Tech_plan_death_spores
  | Tech_plan_unused_10
  | Tech_plan_controlled_inferno_environment
  | Tech_plan_enhanced_eco_restoration
  | Tech_plan_improved_terraforming_30
  | Tech_plan_controlled_toxic_environment
  | Tech_plan_soil_enrichment
  | Tech_plan_bio_toxin_antidote
  | Tech_plan_controlled_radiated_environment
  | Tech_plan_unused_18
  | Tech_plan_improved_terraforming_40
  | Tech_plan_cloning
  | Tech_plan_atmospheric_terraforming
  | Tech_plan_unused_22
  | Tech_plan_advanced_eco_restoration
  | Tech_plan_unused_24
  | Tech_plan_improved_terraforming_50
  | Tech_plan_doom_virus
  | Tech_plan_unused_27
  | Tech_plan_unused_28
  | Tech_plan_advanced_soil_enrichment
  | Tech_plan_unused_30
  | Tech_plan_improved_terraforming_60
  | Tech_plan_unused_32
  | Tech_plan_complete_eco_restoration
  | Tech_plan_unused_34
  | Tech_plan_universal_antidote
  | Tech_plan_unused_36
  | Tech_plan_improved_terraforming_80
  | Tech_plan_unused_38
  | Tech_plan_bio_terminator
  | Tech_plan_unused_40
  | Tech_plan_advanced_cloning
  | Tech_plan_unused_42
  | Tech_plan_improved_terraforming_100
  | Tech_plan_unused_44
  | Tech_plan_unused_45
  | Tech_plan_unused_46
  | Tech_plan_unused_47
  | Tech_plan_unused_48
  | Tech_plan_complete_terraforming
  [@@deriving enum]

type tech_prop =
  | Tech_prop_none
  | Tech_prop_retro_engines
  | Tech_prop_unused_1
  | Tech_prop_hydrogen_fuel_cells
  | Tech_prop_unused_3
  | Tech_prop_deuterium_fuel_cells
  | Tech_prop_nuclear_engines
  | Tech_prop_unused_6
  | Tech_prop_unused_7
  | Tech_prop_irridium_fuel_cells
  | Tech_prop_inertial_stabilizer
  | Tech_prop_unused_10
  | Tech_prop_sub_light_drives
  | Tech_prop_unused_12
  | Tech_prop_dotomite_crystals
  | Tech_prop_unused_14
  | Tech_prop_energy_pulsar
  | Tech_prop_unused_16
  | Tech_prop_fusion_drives
  | Tech_prop_uridium_fuel_cells
  | Tech_prop_warp_dissipator
  | Tech_prop_unused_20
  | Tech_prop_unused_21
  | Tech_prop_reajax_ii_fuel_cells
  | Tech_prop_impulse_drives
  | Tech_prop_unused_24
  | Tech_prop_unused_25
  | Tech_prop_intergalactic_star_gates
  | Tech_prop_unused_27
  | Tech_prop_trilithium_crystals
  | Tech_prop_ion_drives
  | Tech_prop_unused_30
  | Tech_prop_unused_31
  | Tech_prop_unused_32
  | Tech_prop_high_energy_focus
  | Tech_prop_unused_34
  | Tech_prop_anti_matter_drives
  | Tech_prop_unused_36
  | Tech_prop_sub_space_teleporter
  | Tech_prop_unused_38
  | Tech_prop_ionic_pulsar
  | Tech_prop_thorium_cells
  | Tech_prop_inter_phased_drives
  | Tech_prop_sub_space_interdiction
  | Tech_prop_unused_43
  | Tech_prop_combat_transporters
  | Tech_prop_inertial_nullifier
  | Tech_prop_unused_46
  | Tech_prop_hyper_drives
  | Tech_prop_unused_48
  | Tech_prop_displacement_device
  [@@deriving enum]

type tech_weap =
  | Tech_weap_none
  | Tech_weap_lasers
  | Tech_weap_hand_lasers
  | Tech_weap_unused_2
  | Tech_weap_hyper_v_rockets
  | Tech_weap_gatling_laser
  | Tech_weap_anti_missile_rockets
  | Tech_weap_neutron_pellet_gun
  | Tech_weap_hyper_x_rockets
  | Tech_weap_fusion_bomb
  | Tech_weap_ion_cannon
  | Tech_weap_scatter_pack_v_rockets
  | Tech_weap_ion_rifle
  | Tech_weap_mass_driver
  | Tech_weap_merculite_missiles
  | Tech_weap_neutron_blaster
  | Tech_weap_anti_matter_bomb
  | Tech_weap_graviton_beam
  | Tech_weap_stinger_missiles
  | Tech_weap_hard_beam
  | Tech_weap_fusion_beam
  | Tech_weap_ion_stream_projector
  | Tech_weap_omega_v_bomb
  | Tech_weap_anti_matter_torpedoes
  | Tech_weap_fusion_rifle
  | Tech_weap_megabolt_cannon
  | Tech_weap_phasor
  | Tech_weap_scatter_pack_vii_missiles
  | Tech_weap_auto_blaster
  | Tech_weap_pulson_missiles
  | Tech_weap_tachyon_beam
  | Tech_weap_hand_phasor
  | Tech_weap_gauss_autocannon
  | Tech_weap_particle_beam
  | Tech_weap_hercular_missiles
  | Tech_weap_plasma_cannon
  | Tech_weap_death_ray
  | Tech_weap_disruptor
  | Tech_weap_pulse_phasor
  | Tech_weap_neutronium_bomb
  | Tech_weap_hellfire_torpedoes
  | Tech_weap_zeon_missiles
  | Tech_weap_plasma_rifle
  | Tech_weap_proton_torpedoes
  | Tech_weap_scatter_pack_x_missiles
  | Tech_weap_tri_focus_plasma_cannon
  | Tech_weap_stellar_converter
  | Tech_weap_neutron_stream_projector
  | Tech_weap_mauler_device
  | Tech_weap_unused_48
  | Tech_weap_plasma_torpedoes
  [@@deriving enum]
