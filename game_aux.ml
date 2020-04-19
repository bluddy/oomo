open Types

(* Aux game data, not stored in saves *)

type game_aux = {
  human_killer: Player.t;
  local_players: int;
  flag_cheat_galaxy: bool;
  flag_cheat_events: bool;
  initialized: bool;
}
