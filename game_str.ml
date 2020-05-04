open Types

let mm_continue = "Continue"
let mm_load = "Load Game"
let mm_new = "New Game"
let mm_quit = "Quit to OS"

let get_race = function
  | Human -> "Human"
  | Mrrshan -> "Mrrshan"
  | Silicoid -> "Silicoid"
  | Sakkra -> "Sakkra"
  | Psilon -> "Psilon"
  | Alkari -> "Alkari"
  | Klackon -> "Klackon"
  | Bulrathi -> "Bulrathi"
  | Meklar -> "Meklar"
  | Darlok -> "Darlok"
  | RandomRace -> "Random"

let get_races race = get_race race ^ "s"
let tbl_banner = [ "Blue"; "Green"; "Purple"; "Red"; "White"; "Yellow"; "Random" ]
let tbl_gsize = [ "Small"; "Medium"; "Large"; "Huge" ]
let tbl_diffic = [ "Simple"; "Easy"; "Average"; "Hard"; "Impossible" ]
let tbl_oppon = [ "One"; "Two"; "Three"; "Four"; "Five" ]

let tbl_traits = [
    "EXPERT TRADERS"; "AND MAGNIFICENT"; "DIPLOMATS";
    "SUPERIOR GUNNERS"; ""; "";
    "IMMUNE TO"; "HOSTILE PLANET"; "ENVIRONMENTS";
    "INCREASED"; "POPULATION"; "GROWTH";
    "SUPERIOR"; "RESEARCH"; "TECHNIQUES";
    "SUPERIOR PILOTS"; ""; "";
    "INCREASED"; "WORKER"; "PRODUCTION";
    "TERRIFIC GROUND"; "FIGHTERS"; "";
    "ENHANCED"; "FACTORY"; "CONTROLS";
    "SUPREME SPIES"; ""; ""
]

let tbl_trait1 = [
    "Xenophobic"; "Ruthless"; "Aggressive"; "Erratic"; "Honorable"; "Pacifistic"
]

let tbl_trait2 = [
    "Diplomat"; "Militarist"; "Expansionist"; "Technologist"; "Industrialist"; "Ecologist"
]

let ng_choose_race = "Choose Race"
let ng_choose_banner = "Choose Banner"
let ng_your_name = "Your Name..."
let ng_home_name = "Home World..."
let ng_ai = "AI"
let ng_computer = "Computer"
let ng_player = "Player"
let ng_cancel = "Cancel"
let ng_ok = "Start Game"
let ng_allai = "Must have at least one human player!"

let tbl_planet_names = [
    "Paranar"; "Denubius"; "Antares"; "Draconis"; "Zoctan"; "Rigel"; "Talas"; "Moro"; "Quayal"; "Neptunus";
    "Jinga"; "Argus"; "Escalon"; "Berel"; "Collassa"; "Whynil"; "Nordia"; "Tau Cygni"; "Phyco"; "Firma";
    "Keeta"; "Arietis"; "Rhilus"; "Willow"; "Mu Delphi"; "Stalaz"; "Gorra"; "Beta Ceti"; "Spica"; "Omicron";
    "Rha"; "Kailis"; "Vulcan"; "Centauri"; "Herculis"; "Endoria"; "Kulthos"; "Hyboria"; "Zhardan"; "Yarrow";
    "Incedius"; "Paladia"; "Romulas"; "Galos"; "Uxmai"; "Thrax"; "Laan"; "Imra"; "Selia"; "Seidon";
    "Tao"; "Rana"; "Vox"; "Maalor"; "Xudax"; "Helos"; "Crypto"; "Gion"; "Phantos"; "Reticuli";
    "Maretta"; "Toranor"; "Exis"; "Tyr"; "Ajax"; "Obaca"; "Dolz"; "Drakka"; "Ryoun"; "Vega";
    "Anraq"; "Gienah"; "Rotan"; "Proxima"; "Mobas"; "Iranha"; "Celtsi"; "Dunatis"; "Morrig"; "Primodius";
    "Nyarl"; "Ukko"; "Crius"; "Hyades"; "Kronos"; "Guradas"; "Rayden"; "Kakata"; "Misha"; "Xendalla";
    "Artemis"; "Aurora"; "Proteus"; "Esper"; "Darrian"; "Trax"; "Xengara"; "Nitzer"; "Simius"; "Bootis";
    "Pollus"; "Cygni"; "Aquilae"; "Volantis"; "Tauri"; "Regulus"; "Klystron"; "Lyae"; "Capella"; "Alcor"
]

let tbl_home_names = [
    "Sol"; "Fierias"; "Cryslon"; "Sssla"; "Mentar"; "Altair"; "Kholdan"; "Ursa"; "Meklon"; "Nazin"; "Randomia"
]

let rndempname = "Mr Random"
let planet_name_orion = "Orion"

let tbl_stship_names = [
    "SCOUT"; "FIGHTER"; "DESTROYER"; "BOMBER"; "COLONY SHIP"; "NONE"
]

let tbl_monsh_names = [
    "SPACE CRYSTAL"; "SPACE AMOEBA"; "GUARDIAN"
]

let tbl_mon_names = [
    "Space Crystal"; "Space Amoeba"; "Guardian"
]

let ai_colonyship = "COLONY SHIP"

let st_none = "NONE"
let st_none2 = "None"

let tbl_st_weap = [
    "NUCLEAR BOMB"; "LASER"; "NUCLEAR MISSILE"; "NUCLEAR MISSILE"; "HEAVY LASER"; "HYPER-V ROCKET"; "HYPER-V ROCKET"; "GATLING LASER";
    "NEUTRON PELLET GUN"; "HYPER-X ROCKET"; "HYPER-X ROCKET"; "FUSION BOMB"; "ION CANNON"; "HEAVY ION CANNON"; "SCATTER PACK V"; "SCATTER PACK V";
    "DEATH SPORES"; "MASS DRIVER"; "MERCULITE MISSILE"; "MERCULITE MISSILE"; "NEUTRON BLASTER"; "HEAVY BLAST CANNON"; "ANTI-MATTER BOMB"; "GRAVITON BEAM";
    "STINGER MISSILE"; "STINGER MISSILE"; "HARD BEAM"; "FUSION BEAM"; "HEAVY FUSION BEAM"; "OMEGA-V BOMB"; "ANTI-MATTER TORP"; "MEGABOLT CANNON";
    "PHASOR"; "HEAVY PHASOR"; "SCATTER PACK VII"; "SCATTER PACK VII"; "DOOM VIRUS"; "AUTO BLASTER"; "PULSON MISSILE"; "PULSON MISSILE";
    "TACHYON BEAM"; "GAUSS AUTOCANNON"; "PARTICLE BEAM"; "HERCULAR MISSILE"; "HERCULAR MISSILE"; "PLASMA CANNON"; "DISRUPTOR"; "PULSE PHASOR";
    "NEUTRONIUM BOMB"; "BIO TERMINATOR"; "HELLFIRE TORPEDO"; "ZEON MISSILE"; "ZEON MISSILE"; "PROTON TORPEDO"; "SCATTER PACK X"; "SCATTER PACK X";
    "TRI-FOCUS PLASMA"; "STELLAR CONVERTER"; "MAULER DEVICE"; "PLASMA TORPEDO"; "CRYSTAL RAY"; "DEATH RAY"; "AMEOBA STREAM"
]

let tbl_st_weapx = [
    "GROUND ATTACKS ONLY"; " "; "2 SHOTS; +1 SPEED"; "5 SHOTS"; " "; "2 SHOTS; +1 SPEED"; "5 SHOTS"; "FIRES 4 TIMES/TURN ";
    "HALVES ENEMY SHIELDS"; "2 SHOTS; +1 TO HIT"; "5 SHOTS; +1 TO HIT"; "GROUND ATTACKS ONLY "; " "; " "; "2 SHOTS; MIRVS TO 5"; "5 SHOTS; MIRVS TO 5";
    "BIOLOGICAL WEAPON"; "HALVES ENEMY SHIELDS"; "2 SHOTS; +2 TO HIT"; "5 SHOTS; +2 TO HIT"; " "; " "; "GROUND ATTACKS ONLY"; "STREAMING ATTACK";
    "2 SHOTS; +3 TO HIT"; "5 SHOTS; +3 TO HIT"; "HALVES SHIELD STR"; " "; " "; "GROUND ATTACKS ONLY "; "FIRES ONE PER 2 TURNS"; "+3 LEVELS TO HIT";
    " "; " "; "2 SHOTS; MIRVS TO 7"; "5 SHOTS; MIRVS TO 7"; "BIOLOGICAL WEAPON"; "FIRES 3 TIMES/TURN"; "2 SHOTS; +4 TO HIT"; "5 SHOTS; +4 TO HIT";
    "STREAMING ATTACK"; "1/2 SHIELDS; FIRES 4$"; "HALVES SHIELDS STR"; "2 SHOTS; +5 TO HIT"; "5 SHOTS; +5 TO HIT"; " "; " "; "FIRES 3 TIMES/TURN";
    "GROUND ATTACKS ONLY"; "BIOLOGICAL WEAPON"; "HITS ALL FOUR SHIELDS"; "2 SHOTS; +6 TO HIT"; "5 SHOTS; +6 TO HIT"; "FIRES ONE PER 2 TURNS"; "2 SHOTS; MIRVS TO 10"; "5 SHOTS; MIRVS TO 10";
    " "; "HITS ALL FOUR SHIELDS"; "CRUEL BRUTAL DAMAGE"; "LOSES 15 DAMAGE/HEX"; ""; ""; ""
]

let tbl_st_comp = [
    "MARK I"; "MARK II"; "MARK III"; "MARK IV"; "MARK V"; "MARK VI"; "MARK VII"; "MARK VIII";
    "MARK IX"; "MARK X"; "MARK XI"
]

let tbl_st_engine = [
    "RETROS"; "NUCLEAR"; "SUB-LIGHT"; "FUSION"; "IMPULSE"; "ION"; "ANTI-MATTER"; "INTERPHASED"; "HYPERTHRUST"
]

let tbl_st_armor = [
    "TITANIUM"; "TITANIUM II"; "DURALLOY"; "DURALLOY II"; "ZORTRIUM"; "ZORTRIUM II"; "ANDRIUM"; "ANDRIUM II"; "TRITANIUM";
    "TRITANIUM II"; "ADAMANTIUM"; "ADAMANTIUM II"; "NEUTRONIUM"; "NEUTRONIUM II"
]

let tbl_st_shield = [
    "CLASS I"; "CLASS II"; "CLASS III"; "CLASS IV"; "CLASS V"; "CLASS VI"; "CLASS VII"; "CLASS IX";
    "CLASS XI"; "CLASS XIII"; "CLASS XV"
]

let tbl_st_jammer = [
    "JAMMER I"; "JAMMER II"; "JAMMER III"; "JAMMER IV"; "JAMMER V"; "JAMMER VI"; "JAMMER VII"; "JAMMER VIII";
    "JAMMER IX"; "JAMMER X"
]

let tbl_st_specsh = [
    "NO SPECIALS"; "RESERVE TANKS"; "COLONY BASE"; "BARREN BASE"; "TUNDRA BASE"; "DEAD BASE"; "INFERNO BASE"; "TOXIC BASE";
    "RADIATED BASE"; "BATTLE SCANNER"; "ANTI-MISSILES"; "REPULSOR BEAM"; "WARP DISSIPATOR"; "ENERGY PULSAR"; "INERT STABILIZER"; "ZYRO SHIELD";
    "AUTO REPAIR"; "STASIS FIELD"; "CLOAKING DEVICE"; "ION STREAM"; "H-ENERGY FOCUS"; "IONIC PULSAR"; "BLACK HOLE GEN"; "TELEPORTER";
    "LIGHTNING SHIELD"; "NEUTRON STREAM"; "ADV DMG CONTROL"; "TECH NULLIFIER"; "INERT. NULLIFIER"; "ORACLE INTERFACE"; "DISPLACE DEVICE"
]
let tbl_st_special = [
    "RESERVE FUEL TANKS"; "STANDARD COLONY BASE"; "BARREN COLONY BASE"; "TUNDRA COLONY BASE"; "DEAD COLONY BASE"; "INFERNO COLONY BASE"; "TOXIC COLONY BASE"; "RADIATED COLONY BASE";
    "BATTLE SCANNER"; "ANTI-MISSILE ROCKETS"; "REPULSOR BEAM"; "WARP DISSIPATOR"; "ENERGY PULSAR"; "INERTIAL STABILIZER"; "ZYRO SHIELD"; "AUTOMATED REPAIR";
    "STASIS FIELD"; "CLOAKING DEVICE"; "ION STREAM PROJECTOR"; "HIGH ENERGY FOCUS"; "IONIC PULSAR"; "BLACK HOLE GENERATOR"; "SUB SPACE TELEPORTER"; "LIGHTNING SHIELD";
    "NEUTRON STREAM PROJECTOR"; "ADV DAMAGE CONTROL"; "TECHNOLOGY NULLIFIER"; "INERTIAL NULLIFIER"; "ORACLE INTERFACE"; "DISPLACMENT DEVICE"
]
let tbl_st_specialx = [
    "EXTENDS SHIP RANGE BY 3 PARSECS"; "ALLOWS NORMAL PLANET LANDINGS"; "ALLOWS BARREN PLANET LANDINGS"; "ALLOWS TUNDRA PLANET LANDINGS"; "ALLOWS DEAD PLANET LANDINGS"; "ALLOWS INFERNO PLANET LANDINGS";
    "ALLOWS TOXIC PLANET LANDINGS"; "ALLOWS RADIATED PLANET LANDINGS"; "DISPLAYS ENEMY SHIP STATS"; "40% CHANCE MISSILES DESTROYED"; "MOVES ENEMY SHIPS BACK 1 SPACE"; "REDUCES SPEED OF ENEMY SHIPS";
    "EXPANDS TO INFLICT 1-5 HITS"; "ADDS +2 TO MANEUVERABILITY"; "75% CHANCE MISSILES DESTROYED"; "HEALS 15% OF SHIP'S HITS A TURN"; "ENEMY FROZEN FOR 1 TURN"; "RENDERS SHIPS NEARLY INVISIBLE";
    "REDUCES ENEMY ARMOR BY 20%"; "INCREASES WEAPON RANGES BY 3"; "EXPANDS TO INFLICT 2-10 HITS"; "KILLS 25%-100% OF ENEMY SHIPS"; "TELEPORTS SHIP IN COMBAT"; "100% CHANCE MISSILES DESTROYED";
    "REDUCES ENEMY ARMOR BY 40%"; "HEALS 30% OF SHIP'S HITS A TURN"; "DESTROYS ENEMY COMPUTERS"; "ADDS +4 TO MANEUVERABILITY"; "CONCENTRATES BEAM ATTACKS"; "1/3 OF ALL ENEMY ATTACKS MISS"
]

let tbl_st_hull = [ "SMALL"; "MEDIUM"; "LARGE"; "HUGE" ]

let sm_crystal = "CRYSTAL"
let sm_amoeba = "AMOEBA"
let sm_game = "Game"
let sm_design = "Design"
let sm_fleet = "Fleet"
let sm_map = "Map"
let sm_races = "Races"
let sm_planets = "Planets"
let sm_tech = "Tech"
let sm_next_turn = "Next Turn"

let tbl_sm_stinfo = [
    "Yellow stars offer the best chance of discovering terran and sub-terran planets.";
    "Red stars are old; dull stars that commonly have poor planets.";
    "Green stars are moderately bright and have a wide range of planetary types.";
    "White stars burn incredibly hot and generally have hostile planets.";
    "Blue stars are relatively young stars with mineral rich lifeless planets.";
    "Neutron stars are rare and offer the greatest chance of finding rich planets."
]

let sm_range = "Range"
let sm_parsec = "Parsec"
let sm_parsecs = "Parsecs"
let sm_parsecs2 = "PARSECS"
let sm_colony = "Colony"
let sm_lastrep = "Last Reported As A"
let sm_stargate = "STAR GATE"
let sm_prodnone = "NONE"
let sm_prod_y = "Y"
let sm_defupg = "UPGRD"
let sm_defshld = "SHIELD"
let sm_refit = "REFIT"
let sm_indmax = "MAX"
let sm_indres = "RESERV"
let sm_ecowaste = "WASTE"
let sm_ecoclean = "CLEAN"
let sm_ecoatmos = "ATMOS"
let sm_ecotform = "T-FORM"
let sm_ecosoil = "SOIL"
let sm_ecogaia = "GAIA"
let sm_ecopop = "POP"
let sm_unexplored = "UNEXPLORED"
let sm_nohabit = "NO HABITABLE"
let sm__planets = "PLANETS"

let tbl_sm_pltype = [
    "NO HABITABLE PLANETS"; "RADIATED"; "TOXIC"; "INFERNO"; "DEAD";
    "TUNDRA"; "BARREN"; "MINIMAL"; "DESERT"; "STEPPE"; "ARID"; "OCEAN";
    "JUNGLE"; "TERRAN"; "GAIA"
]

let sm_plague = "Plague"
let sm_nova = "Nova"
let sm_comet = "Comet"
let sm_pirates = "Pirates"
let sm_rebellion = "Rebellion"
let sm_unrest = "UNREST"
let sm_accident = "Accident"

let tbl_sm_pgrowth = [
    "HOSTILE"; " "; "FERTILE"; "GAIA"
]

let tbl_sm_pspecial = [
    "ULTRA POOR"; "POOR"; ""; "ARTIFACTS"; "RICH"; "ULTRA RICH"; "4$ TECH"
]

let sm_pop = "POP"
let sm_max = "MAX"

let sm_hasreached = "has reached its"
let sm_indmaxof = "industry maximum of"
let sm_factories = "factories"
let sm_extrares = " The extra spent was placed in the planetary reserve."
let sm_popmaxof = "population maximum of"
let sm_colonists = "colonists"
let sm_hasterraf = "has been terraformed to a"
let sm_new = "new"
let tbl_sm_terraf = [
    "normal"; "fertile"; "gaia"
]
let sm_envwith = "environment with"
let tbl_sm_envmore = [
    ""; "150% of "; "double "
]
let sm_stdgrow = "the standard growth rate"
let sm_hasfsgate = "has finished building a stargate"
let sm_hasfshield = "has completed building a Class"
let sm_planshield = "Planetary Shield"
let sm_planratio = " Planetary spending ratios may be changed at this time. "

let sm_fleetdep = "FLEET DEPLOYMENT"
let sm_destoor = "DESTINATION IS OUT OF RANGE;"
let sm_destoor2 = "OUT OF RANGE"
let sm_parsfromcc = "PARSECS FROM CLOSEST COLONY"
let sm_eta = "ETA"
let sm_turn = "TURN"
let sm_turns = "TURNS"
let sm_chdest = "Choose destination and number to send"

let sm_outsr = "OUT SHIP RANGE BY"

let sm_sreloc = "Ship Relocation"
let sm_sreloc2 = "Choose another star system under your control to redirect newly built ships to."
let sm_delay = "DELAY"

let sm_seltr = "Select a destination star system to send colonists or troops to."
let sm_notrange = "You do not have the required ship range to reach the system."
let sm_notrange1 = "The star system is"
let sm_notrange2 = "parsecs away and you have a maximum range of"
let sm_notrange3 = "parsecs."
let sm_trfirste = "You must first explore a star system and form a new colony before transporting colonists."
let sm_trcontr1 = "You must have at least controlled"
let sm_trcontr2 = "environments to land troops onto the planet."
let sm_trfirstc = "You must first build a ship equipped with a colony base and create a new colony before sending out transports."
let sm_trwarna = "Warning; destination is owned by an ally"
let sm_trwarnm1 = "Warning - Target planet can only support"
let sm_trwarnm2 = "million!"
let sm_trchnum1 = "Choose number of colonists to transport"
let sm_trchnum2 = "Choose number of troops to transport"
let sm_trans1 = "Transport"
let sm_transs = "Transports"
let sm_tdest = "Destination"

let sm_bomb1 = "Bomb the"
let sm_bomb2 = "Enemy Planet?"
let sm_trinb1 = "Troop Transport"
let sm_trinb1s = "Troop Transport"
let sm_trinb2 = "Currently Enroute"

let sm_obomb1 = "Orbital"
let sm_obomb2 = "Bombardment"
let sm_cdest1 = "colony"
let sm_cdest2 = "destroyed"
let sm_ineff1 = "bombing"
let sm_ineff2 = "ineffective"
let sm_bkill1 = "MILLION"
let sm_bkill2 = "KILLED"
let sm_bfact1 = "FACTORY"
let sm_bfact1s = "FACTORIES"
let sm_bfact2 = "DESTROYED"

let sm_traad1 = "transports attempting to land on"
let sm_traad2 = "were all destroyed."
let sm_trbdb1 = "The base at"
let sm_trbdb2 = "was destroyed before the transports arrived leaving the colonists without supplies and shelter. All have perished."

let sm_inorbit = "In Orbit"

let tbl_roman = [
    " "; "I"; "II"; "III"; "IV"; "V"; "VI"; "VII";
    "VIII"; "IX"; "X"; "XI"; "XII"; "XIII"; "XIV"; "XV";
    "XVI"; "XVII"; "XVIII"; "XIX"; "XX"; "XXI"; "XXII"; "XXIII";
    "XXIV"; "XXV"; "XXVI"; "XXVII"; "XXVIII"; "XXIX"; "XXX"
]

let no_events = "No Events"
let bc = "BC"
let y = "Y"
let year = "Year"
let player = "Player"

let pl_reserve = "Reserve"
let pl_plague = "PLAGUE"
let pl_nova = "SUPER NOVA"
let pl_comet = "COMET"
let pl_pirates = "PIRATES"
let pl_rebellion = "REBELLION"
let pl_unrest = "UNREST"
let pl_accident = "ACCIDENT"
let pl_spending = "Spending Costs"
let pl_tincome = "Total Income"
let pl_transof = "Transfer of planetary"
let pl_resto = "reserves to"

let sd_cancel = "CANCEL"
let sd_build = "BUILD"
let sd_clear = "CLEAR"
let sd_comp = "Computer"
let sd_shield = "Shield"
let sd_ecm = "Ecm"
let sd_armor = "Armor"
let sd_engine = "Engine"
let sd_man = "Maneuver"
let tbl_sd_spec = [
    "Special 1"; "Special 2"; "Special 3"
]
let tbl_sd_weap = [
    "Weapon 1"; "Weapon 2"; "Weapon 3"; "Weapon 4";
]
let sd_count = "Count"
let sd_sweap = "Ship Weapons"
let sd_damage = "Damage"
let sd_rng = "Rng"
let sd_notes = "Notes"
let sd_hp = "HIT POINTS"
let sd_warp = "WARP"
let sd_def = "DEF"
let sd_cspeed = "COMBAT SPEED"
let sd_absorbs = "ABSORBS"
let sd_hit = "HIT"
let sd_hits = "HITS"
let sd_misdef = "MISSILE DEF"
let sd_att = "ATTACK LEVEL"
let sd_comptype = "COMPUTER TYPE"
let sd_cost = "COST"
let sd_size = "SIZE"
let sd_power = "POWER"
let sd_space = "SPACE"
let sd_comps = "COMPUTERS"
let sd_shieldtype = "SHIELD TYPE"
let sd_shields = "SHIELDS"
let sd_ecmtype = "ECM TYPE"
let sd_ecm2 = "ECM"
let sd_armortype = "ARMOR TYPE"
let sd_armor2 = "ARMOR"
let sd_engtype = "ENGINE TYPE"
let sd_numengs = "NUM ENGINES"
let sd_engs = "ENGINES"
let sd_man1 = "MANEUVER"
let sd_man2 = "MANEUVERABILITY"
let sd_class = "CLASS"
let sd_speed = "SPEED"
let sd_max = "MAX"
let sd_weapname = "WEAPON NAME"
let sd_descr = "DESCRIPTION"
let sd_dmg = "DMG"
let sd_weaps = "WEAPONS"
let sd_specname = "SPECIAL NAME"
let sd_specs = "SPECIAL DEVICES"

let sp_only6 = "Only 6 ships may be commisioned at one time. 1/4 the decomissioned ship's cost is placed in the planetary reserve."
let sp_wantscrap = "Do you want to scrap this ship?"
let sp_before = "Before a new design can be created; you must first scrap one of the six current designs."
let sp_cost = "Cost"

let fl_station = "STATION"
let fl_inorbit = "IN ORBIT"
let fl_moving = "MOVING TO"
let fl_unknown = "UNKNOWN"
let fl_system = "SYSTEM"

let gm_tchar = "TJOASDMBUEIPRN"
let tbl_gm_spec = [
    "U POOR"; "POOR"; ""; "ARTIFACTS"; "RICH"; "U RICH"; "ORION"
]
let gm_unable = "Unable to land on"
let gm_prod = "PROD"
let gm_tech = "TECH"
let gm_1_3 = "1/3"
let gm_1_2 = "1/2"
let gm_2x = "2$ "
let gm_3x = "3$ "
let gm_4x = "4$ "
let gm_prodb1 = "PRODUCTION BONUSES"
let gm_prodb2 = "APPLY TO INDUSTRY;"
let gm_prodb3 = "SHIPS AND DEFENSE"
let gm_gmap = "Galaxy Map"
let gm_mapkey = "Map Key"

let bs_line1 = "How many missile"
let bs_line2 = "bases to eliminate?"
let bs_base = "Base"
let bs_bases = "Bases"

let gv_target = "How many missile bases to build here?"
let gv_adjust = "Readjust all governed planets"
let gv_resta = "All planets spend rest on"
let gv_thispl = "This planet"
let gv_rest = "Spend rest on"
let tbl_gv_rest = [
    "research"; "ships"; "reserve"
]
let gv_reserve = "Reserve boost"
let tbl_gv_reserve = [
    "none"; "buildup"; "always"
]
let gv_allpl = "All planets"
let gv_starg = "Build stargates"
let gv_ecom = "Eco mode"
let tbl_gv_ecom = [
    "Grow pop before Def";
    "Grow pop before last";
    "Never grow pop";
    "Do not decrease Eco";
    "Do not touch Eco"
]
let gv_buildup = "Boost mode"
let tbl_gv_buildup = [
    "economic (to 1/2 capacity)";
    "military (until bases are up)";
    "complete"
]

let get_te_field = function
  | Tech_field_computer -> "Computer"
  | Tech_field_construction -> "Construction"
  | Tech_field_force_field -> "Force Field"
  | Tech_field_planetology -> "Planetology"
  | Tech_field_propulsion -> "Propulsion"
  | Tech_field_weapon -> "Weapon"

let te_adv = "Advanced"
let te_tech = "Tech"
let te_techno = "Technology"
let te_techno2 = "technology"
let te_genimp = "General improvements of existing"
let te_nmis = "Missiles tipped with nuclear warheads that explode for 4 points of damage and move at a speed of 2."
let te_nbomb = "Bombs that explode for 3-12 points of damage on ground targets only."
let te_scrange = "SCANNER RANGE"
let te_rctrl = "Robot Controls"
let te_col = "col"
let te_fwaste = "FACTORY WASTE"
let te_gcombat = "GROUND COMBAT"
let te_tform = "TERRAFORM"
let te_wasteel = "WASTE ELIMINATION"
let te_shrange = "SHIP RANGE"
let te_max = "MAX"
let te_rp = "RP"

let nt_achieve = "Scientists Achieve A"
let nt_break = "Breakthrough"
let nt_infil = "Spies Infiltrate The Research Center At"
let nt_ruins = "Troopers At The Ruins Of"
let nt_discover = "Discover"
let nt_orion = "Troopers Landing on Orion Discover"
let nt_scouts = "Scouts Exploring The Ruins Of"
let nt_choose = "Choose the area of research our scientists now focus on"
let nt_reveal = "Scientists Reveal Their"
let nt_secrets = "Secrets"
let nt_frame = "Your spies managed to frame another race for the theft"
let nt_victim = "Choose the victim race:"
let nt_doyou = "Do you want to "
let nt_inc = "increase the "
let nt_redueco = "reduce the ecology ratio of all of your colonies to the minimum amount necessary to keep them clean?"
let nt_ind = "industry ratios of all of your colonies to upgrade your factory controls?"
let nt_ecoall = "ecology ratios of all of your colonies"
let nt_terra = " in order to begin terraforming your planets?"
let nt_def = "defense ratio of all of your colonies to build the new planetary shields?"
let nt_ecostd = "ecology ratio of your colonies with standard environments"
let nt_ecohost = "ecology ratio of your colonies with hostile environments"
let tbl_nt_adj = [
    "NO"; "+25%"; "+50%"; "+75%"
]

let ra_nocont = "No Contact"
let ra_notpres = "Not Present"
let ra_secline1 = "SECURITY INCREASES THE CHANCE OF"
let ra_secline2 = "CATCHING ALL ENEMY SPIES. "
let ra_alloc = "Allocations"
let ra_planres = "Planetary Resources"
let ra_diplo = "Diplomat"
let ra_gone = "Gone"
let ra_nospies = "NO SPIES"
let ra_spy = "SPY"
let ra_spies = "SPIES"
let tbl_ra_treaty = [
    "No Treaty"; "Non-Aggression Pact"; "Alliance"; "War"; "Final War"
]
let ra_trade = "Trade"
let ra_notrade = "No Trade"
let tbl_ra_relat = [
    "FEUD"; "HATE"; "DISCORD"; "TROUBLED"; "TENSE"; "RESTLESS"; "WARY"; "UNEASE";
    "NEUTRAL"; "RELAXED"; "AMIABLE"; "CALM"; "AFFABLE"; "PEACEFUL"; "FRIENDLY"; "UNITY";
    "HARMONY"
]
let ra_stats = "Racial Stats"

let re_reportis = "REPORT IS"
let re_current = "CURRENT"
let re_yearsold = "years old"
let re_alliance = "Alliances"
let re_wars = "Wars"
let re_environ = "ENVIRON"

let sc_caught = "Spies Caught  Yours  Theirs"

let bp_scombat = "Space Combat"
let bp_attack = "attack"
let bp_attacks = "attacks"
let bp_won = "won"
let bt_auto_move = "AUTO MOVE"
let bt_pop = "POPULATION"
let bt_ind = "INDUSTRY"
let bt_bases = "MISSILE BASES"
let bt_subint = "SUBSPACE INTERDICTOR"
let bt_launch = "LAUNCHERS"
let bt_coldest = "Colony Was Destroyed!"

let es_youresp1 = "YOUR SPIES HAVE INFILTRATED A"
let es_youresp2 = "BASE"
let es_youresp3 = "CHOOSE THE TYPE OF TECHNOLOGY TO STEAL"
let es_thesp1 = "Espionage"
let es_thesp2 = "spies steal the plans for:"
let es_unkn = "Unknown"

let sb_choose = "Choose target for sabotage"
let sb_lastrep = "Last Report:"
let sb_pop = "Population:"
let sb_fact = "Factories:"
let sb_bases = "Missile Bases:"
let sb_unkn = "Unknown spy"
let sb_your = "Your"
let sb_spies = "spies"
let sb_increv = "incited a revolt!"
let sb_inc1 = "incited"
let sb_inc2 = "rebels. Unrest now at"
let sb_destr = "destroyed"
let sb_fact2 = "factory"
let sb_facts = "factories"
let sb_mbase = "missile base"
let sb_mbases = "missile bases"
let sb_failed = "failed!"
let sb_nofact = "No factories to sabotage"
let sb_nobases = "No missile bases to sabotage"
let sb_noinc = "failed to incite any rebels"
let sb_frame = "Your spies managed to frame another race for the sabotage"

let ex_planeta = "Planetary"
let ex_scanner = "scanners"
let ex_scout = "Scout ships"
let ex_explore = "explore a new"
let ex_starsys = "star system"
let ex_build = "Build a"
let ex_colony = "new colony?"
let ex_popgr = "POPULATION GROWTH"
let ex_resopnt = "RESOURCE POINTS"
let ex_fromind = "FROM INDUSTRY ARE"
let ex_techpnt = "TECHNOLOGY POINTS"
let ex_fromres = "FROM RESEARCH"
let ex_aredbl = "ARE DOUBLED."
let ex_arequad = "ARE QUADRUPLED."
let ex_pg1 = [
    "Hostile"; "Ecologicaly"; "Ecological"
]
let ex_pg2 = [
    "Environment"; "Fertile"; "Gaia"
]
let ex_pg3 = [
    "IS HALVED."; "IS +50% NORMAL."; "IS DOUBLED."
]
let ex_ps1 = [
    "Ultra Poor"; "Mineral Poor"; "Artifacts"; "Mineral Rich"; "Ultra Rich"
]
let ex_ps2 = [
    "CUT TO ONE-THIRD."; "HALVED."; "DOUBLED."; "TRIPLED."
]

let la_colony = "Colony Name..."
let la_inyear = "In the year"
let la_the = "the"
let la_formnew = "s form a new colony"

let gr_carmor = "Combat Armor"
let gr_outof = "out of"
let gr_transs = "transports"
let gr_reclaim = "transports land to reclaim the colony"
let gr_penetr = "penetrate"
let gr_defenss = "defenses"
let gr_troops = "Troops"
let gr_rebel = "Rebel"
let gr_gcon = "Ground Combat On"
let gr_scapt = "s Capture"
let gr_itroops = "Imperial Troops Recapture"
let gr_succd = "s Successfully Defend"
let gr_fcapt = "factories captured"
let gr_tsteal = "technology stolen"
let gr_tnew = "new tech found"

let el_no = "no"
let el_vote = "vote"
let el_votes = "votes"
let el_total = "total"
let el_start = "The High Council has convened to elect one leader to be Emperor of the Galaxy..."
let el_emperor = "Emperor"
let el_ofthe = "of the"
let el_and = "and"
let el_for = "for"
let el_nomin = "have been nominated."
let el_abs1 = "The"
let el_abs2 = "abstain ("
let el_dots = ")..."
let el_your = "Your choice ("
let el_bull = "["
let el_self = "Yourself"
let el_abs = "Abstain"
let el_chose1 = "In the year"
let el_chose2 = "; the Council has chosen"
let el_chose3 = "to be the High Master of the New Republic."
let el_neither = "Neither leader has a two thirds majority..."
let el_accept = "Do you accept the ruling?"
let el_yes = "Yes"
let el_no2 = "No"
let el_sobeit = "So be it. You defy the ruling of the council. Now you will feel the wrath of the New Republic!"
let el_isnow = "is now High Master."

let au_facts = "factories"
let au_bases = "missile bases"
let au_treaty = "treaty"
let au_allian = "Alliance"
let au_nonagg = "Non-Aggression Pact"
let au_tradea = "Trade Agreement"
let au_amreca = "(ambassador recalled)"
let au_tech = "tech"
let au_framed = "(you were framed)"
let au_bull = "["
let au_inxchng = "In exchange you will receive:"
let au_whatif1 = "What if we were to also offer"
let au_whatif2 = "as an incentive"
let au_perrec1 = "Perhaps you would reconsider if we also provided"
let au_ques = "?"
let au_howmay = "How may our empire serve you:"
let au_youprte = "You propose a treaty:"
let au_youprta = "You propose a trade agreement for:"
let au_youract = "Your actions:"
let au_whatech = "What type of technology interests you?"
let au_whatrad = "What will you trade for it?"
let au_whatoff = "What do you offer as tribute?"
let au_perthr1 = "Perhaps if you were to throw in"
let au_perthr2 = "we could deal."
let au_alsoof1 = "If you also offer us"
let au_alsoof2 = "we would accept."
let au_whowar = "Who should we declare war on?"
let au_whobrk = "Who should we break our treaty with?"
let au_bcpery = "BC / year"
let au_whattr = "What do you offer as tribute?"
let au_techn = "[ Technology"
let au_nextp = "[ Next page"
let au_back = "[ Back"

let au_opts_main = [
    "[ Propose Treaty";
    "[ Form Trade Agreement";
    "[ Threaten/Break Treaty and Trade";
    "[ Offer Tribute";
    "[ Exchange Technology";
    "[ Good Bye"
]
let au_opts_treaty = [
    "[ Non-Aggression Pact";
    "[ Alliance";
    "[ Peace Treaty";
    "[ Declaration of War on Another Race";
    "[ Break Alliance With Another Race";
    "[ Forget It"
]
let au_opts_agree = [
    "[ Agree";
    "[ Forget It"
]
let au_opts_accept = [
    "[ Accept";
    "[ Reject"
]
let au_opts_threaten = [
    "[ Break Non-Aggression Pact";
    "[ Break Alliance";
    "[ Break Trade Agreement";
    "[ Threaten To Attack";
    "[ Forget It"
]
let au_optsmp1 = [
    "[ Agree";
    "[ Forget It";
    "[ Demand BC";
    "[ Demand Technology"
]

let tr_cont1 = "Contact has been broken with the"
let tr_cont2 = "empire!"
let tr_fuel1 = "The fleet orbiting"
let tr_fuel2 = "has been cut off from refueling supply lines and has been lost."

let sv_envir = "environment"
let sv_stargt = "Star Gate"
let sv_shild1 = "CLASS"
let sv_shild2 = "SHIELD"
let sv_psize = "PLANET SIZE:"
let sv_fact = "FACTORIES:"
let sv_waste = "WASTE:"
let sv_pop = "POPULATION:"
let sv_growth = "GROWTH:"
let sv_techp = "Technology points from research are"
let sv_resp = "Resource points from industry are"
let sv_1_3x = "cut to one-third."
let sv_1_2x = "halved."
let sv_2x = "doubled."
let sv_3x = "tripled."
let sv_4x = "quadrupled."
let sv_popgr = "Population growth is"
let sv_pg1 = [
    "Hostile"; "Fertile"; "Gaia"
]
let sv_pg2 = [
    "halved."; "50% greater."; "doubled."
]

let in_loading = "Loading Master of Orion..."
let wl_won_1 = "Escorted by Honor Guard; High Master "
let wl_won_2 = "returns to Orion; throne world of the Ancients."
let wl_won_3 = "The Galactic Imperium has been reformed..."
let wl_3_good_1 = "A new era has dawned. We must set aside our past"
let wl_3_good_2 = "conflicts and greet a new millenium as a united galaxy"
let wl_3_tyrant_1 = "The universe is mine and all shall bow before the"
let wl_3_tyrant_2 = "might of "
let wl_3_tyrant_3 = "; Master of Orion."
let wl_3_tyrant_4 = "Master of the Universe..."
let wl_exile_1 = "Exiled from the known galaxy; Emperor "
let wl_exile_2 = "sets forth to conquer new worlds."
let wl_exile_3 = "vowing to return and claim the renowned title of"
let wl_exile_4 = " Master of Orion..."

let gnn_end_good = "And that's the way it is..."
let gnn_end_tyrant = "Oh well; another millenium serving under a ruthless tyrant..."
let gnn_also = "Also in the news..."

let mf_title = "Show messages:"
let tbl_mf = [
    "Max factories";  "Max population"; "Better growth"; "Stargate"; "Shield"; ""; "Terraformed"; "for governed Planets"
]

let tbl_xtramenu = [
    "Scrap bases";
    "Caught spies";
    "Governor settings";
    "Message filter";
    "Readjust Eco";
    "Select ship everywhere";
    "Relocate relocated";
    "Relocate all";
    "Unrelocate all";
    "Cancel"
]
