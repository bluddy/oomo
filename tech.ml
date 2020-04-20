open Types

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

let get_base_cost_mod_armor g player percent =
  0

