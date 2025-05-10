(** Board representation for Settlers of Catan. *)

type resource =
  | Sheep
  | Wood
  | Ore
  | Brick
  | Wheat
  | Desert

type player = Player.p

type tile = {
  resource : resource;
  num : int;
  mutable player : player list;
}

type board = tile array

let print_tile x =
  let num_str = string_of_int x.num in
  let padding = if x.num >= 10 then "" else " " in
  match x.resource with
  | Sheep -> "Sheep " ^ padding ^ num_str
  | Wood -> " Wood " ^ padding ^ num_str
  | Ore -> "  Ore " ^ padding ^ num_str
  | Brick -> "Brick " ^ padding ^ num_str
  | Wheat -> "Wheat " ^ padding ^ num_str
  | Desert -> " Desert "

let create () =
  (* Create the board with resources in a fixed pattern *)
  let resources =
    [|
      Sheep;
      Wood;
      Brick;
      Wheat;
      Sheep;
      Ore;
      Wood;
      Brick;
      Desert;
      Wheat;
      Wheat;
      Wood;
      Sheep;
      Ore;
      Brick;
      Ore;
      Sheep;
      Wood;
      Wheat;
    |]
  in

  (* Create number tokens in a fixed pattern *)
  let numbers =
    [| 5; 2; 6; 8; 10; 9; 12; 3; 0; 11; 4; 8; 4; 9; 5; 10; 11; 3; 6 |]
  in

  (* Create the board with resource/number thruple *)
  Array.init 19 (fun i ->
      { resource = resources.(i); num = numbers.(i); player = [] })

let get_adjacent_tiles place =
  match place with
  | 0 -> [ 0 ]
  | 1 -> [ 0 ]
  | 2 -> [ 1 ]
  | 3 -> [ 0; 1 ]
  | 4 -> [ 0; 2 ]
  | 5 -> [ 2 ]
  | 6 -> [ 1 ]
  | 7 -> [ 1; 3 ]
  | 8 -> [ 0; 1; 4 ]
  | 9 -> [ 0; 2; 4 ]
  | 10 -> [ 2; 4; 5 ]
  | 11 -> [ 3 ]
  | 12 -> [ 3; 6 ]
  | 13 -> [ 3; 6; 7 ]
  | 14 -> [ 4; 7 ]
  | 15 -> [ 4; 7; 8 ]
  | 16 -> [ 5; 8 ]
  | 17 -> [ 5; 8; 9 ]
  | 18 -> [ 6 ]
  | 19 -> [ 6; 10 ]
  | 20 -> [ 6; 10; 11 ]
  | 21 -> [ 7; 11 ]
  | 22 -> [ 7; 11; 12 ]
  | 23 -> [ 8; 12 ]
  | 24 -> [ 8; 12; 13 ]
  | 25 -> [ 9; 13 ]
  | 26 -> [ 9; 13; 14 ]
  | 27 -> [ 10 ]
  | 28 -> [ 10; 15 ]
  | 29 -> [ 10; 15; 16 ]
  | 30 -> [ 11; 16 ]
  | 31 -> [ 11; 16; 17 ]
  | 32 -> [ 12; 17 ]
  | 33 -> [ 12; 17; 18 ]
  | 34 -> [ 13; 18 ]
  | 35 -> [ 13; 18; 19 ]
  | 36 -> [ 14 ]
  | 37 -> [ 14; 20 ]
  | 38 -> [ 15 ]
  | 39 -> [ 15; 21 ]
  | 40 -> [ 15; 21; 22 ]
  | 41 -> [ 16; 22 ]
  | 42 -> [ 16; 22; 23 ]
  | 43 -> [ 17; 23 ]
  | 44 -> [ 17; 23; 24 ]
  | 45 -> [ 18; 24 ]
  | 46 -> [ 18; 24; 25 ]
  | 47 -> [ 19 ]
  | 48 -> [ 19; 26 ]
  | 49 -> [ 20 ]
  | 50 -> [ 21 ]
  | 51 -> [ 22 ]
  | 52 -> [ 18 ]
  | 53 -> [ 18 ]
  | _ -> [] (* Invalid place number *)

let places = Array.make 54 "A"

let add_resource resource player =
  match resource with
  | Sheep ->
      Player.increment_resource "sheep" player;
      Player.print_hand player
  | Wood ->
      Player.increment_resource "wood" player;
      Player.print_hand player
  | Ore ->
      Player.increment_resource "ore" player;
      Player.print_hand player
  | Brick ->
      Player.increment_resource "brick" player;
      Player.print_hand player
  | Wheat ->
      Player.increment_resource "wheat" player;
      Player.print_hand player
  | Desert -> ()

let distribute_resources board tile_indices player =
  List.iter
    (fun tile_index ->
      let tile = board.(tile_index) in
      add_resource tile.resource player)
    tile_indices

let add_player_to_place board player place =
  let adjacent_tiles = get_adjacent_tiles place in

  List.iter
    (fun tile_index ->
      board.(tile_index).player <- player :: board.(tile_index).player)
    adjacent_tiles

let place_village board player place_num =
  places.(place_num) <- "v";
  let adjacent_tiles = get_adjacent_tiles place_num in
  distribute_resources board adjacent_tiles player

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n                         " ^ places.(0) ^ "______" ^ places.(1)
   ^ "\n\
     \                        /       \\\n\
     \                       /         \\\n\
     \              " ^ places.(2) ^ "______" ^ places.(3) ^ "/ "
    ^ print_tile board.(0)
    ^ "  \\" ^ places.(4) ^ "______" ^ places.(5)
    ^ "\n\
      \              /       \\           /       \\\n\
      \             /         \\         /         \\\n\
      \    " ^ places.(6) ^ "______" ^ places.(7) ^ "/ "
    ^ print_tile board.(1)
    ^ "  \\" ^ places.(8) ^ "_____" ^ places.(9) ^ "/ "
    ^ print_tile board.(2)
    ^ "  \\" ^ places.(10) ^ "______" ^ places.(11)
    ^ "\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ " ^ places.(12) ^ "/ "
    ^ print_tile board.(3)
    ^ "  \\" ^ places.(13) ^ "_____" ^ places.(14) ^ "/ "
    ^ print_tile board.(4)
    ^ "  \\" ^ places.(15) ^ "_____" ^ places.(16) ^ "/ "
    ^ print_tile board.(5)
    ^ "  \\" ^ places.(17)
    ^ "\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    " ^ places.(18) ^ "______" ^ places.(19) ^ "/ "
    ^ print_tile board.(6)
    ^ "  \\" ^ places.(20) ^ "_____" ^ places.(21) ^ "/ "
    ^ print_tile board.(7)
    ^ "  \\" ^ places.(22) ^ "______" ^ places.(23)
    ^ "\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ " ^ places.(24) ^ "/ "
    ^ print_tile board.(8)
    ^ "  \\" ^ places.(25) ^ "_____" ^ places.(26) ^ "/ "
    ^ print_tile board.(9)
    ^ "  \\" ^ places.(27) ^ "_____" ^ places.(28) ^ "/ "
    ^ print_tile board.(10)
    ^ "  \\" ^ places.(29)
    ^ "\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    " ^ places.(30) ^ "______" ^ places.(31) ^ "/ "
    ^ print_tile board.(11)
    ^ "  \\" ^ places.(32) ^ "_____" ^ places.(33) ^ "/ "
    ^ print_tile board.(12)
    ^ "  \\" ^ places.(34) ^ "______" ^ places.(35)
    ^ "\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ " ^ places.(36) ^ "/ "
    ^ print_tile board.(13)
    ^ "  \\" ^ places.(37) ^ "_____" ^ places.(38) ^ "/ "
    ^ print_tile board.(14)
    ^ "  \\" ^ places.(39) ^ "_____" ^ places.(40) ^ "/ "
    ^ print_tile board.(15)
    ^ "  \\" ^ places.(41)
    ^ "\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    " ^ places.(42) ^ "______" ^ places.(43) ^ "/ "
    ^ print_tile board.(16)
    ^ "  \\" ^ places.(44) ^ "_____" ^ places.(45) ^ "/ "
    ^ print_tile board.(17)
    ^ "  \\" ^ places.(46) ^ "______" ^ places.(47)
    ^ "\n\
      \            \\           /       \\           /\n\
      \             \\         /         \\         /\n\
      \              " ^ places.(48) ^ "______" ^ places.(49) ^ "/ "
    ^ print_tile board.(18)
    ^ "  \\" ^ places.(50) ^ "______" ^ places.(51)
    ^ "\n\
      \                      \\           /\n\
      \                       \\         /\n\
      \                        " ^ places.(52) ^ "_______" ^ places.(53)
    ^ "\n\n \n")
