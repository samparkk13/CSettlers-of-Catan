open ANSITerminal
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
  | 10 -> [ 2; 5 ]
  | 11 -> [ 5 ]
  | 12 -> [ 3 ]
  | 13 -> [ 1; 3; 6 ]
  | 14 -> [ 1; 4; 6 ]
  | 15 -> [ 2; 4; 7 ]
  | 16 -> [ 2; 5; 7 ]
  | 17 -> [ 5 ]
  | 18 -> [ 3; 8 ]
  | 19 -> [ 3; 6; 8 ]
  | 20 -> [ 4; 6; 9 ]
  | 21 -> [ 4; 7; 9 ]
  | 22 -> [ 5; 7; 10 ]
  | 23 -> [ 5; 10 ]
  | 24 -> [ 8 ]
  | 25 -> [ 6; 8; 11 ]
  | 26 -> [ 6; 9; 11 ]
  | 27 -> [ 7; 9; 12 ]
  | 28 -> [ 7; 10; 12 ]
  | 29 -> [ 10 ]
  | 30 -> [ 8; 13 ]
  | 31 -> [ 8; 11; 13 ]
  | 32 -> [ 9; 11; 14 ]
  | 33 -> [ 9; 12; 14 ]
  | 34 -> [ 10; 12; 15 ]
  | 35 -> [ 10; 15 ]
  | 36 -> [ 13 ]
  | 37 -> [ 11; 13; 16 ]
  | 38 -> [ 11; 14; 16 ]
  | 39 -> [ 12; 14; 17 ]
  | 40 -> [ 12; 15; 17 ]
  | 41 -> [ 15 ]
  | 42 -> [ 13 ]
  | 43 -> [ 13; 16 ]
  | 44 -> [ 14; 16; 18 ]
  | 45 -> [ 14; 17; 18 ]
  | 46 -> [ 15; 17 ]
  | 47 -> [ 115 ]
  | 48 -> [ 16 ]
  | 49 -> [ 16; 18 ]
  | 50 -> [ 17; 18 ]
  | 51 -> [ 17 ]
  | 52 -> [ 18 ]
  | 53 -> [ 18 ]
  | _ -> [] (* Invalid place number *)

let places = Array.make 54 ("A", ANSITerminal.default)

let add_resource resource player =
  match resource with
  | Sheep -> Player.increment_resource "sheep" player
  | Wood -> Player.increment_resource "wood" player
  | Ore -> Player.increment_resource "ore" player
  | Brick -> Player.increment_resource "brick" player
  | Wheat -> Player.increment_resource "wheat" player
  | Desert -> ()

let distribute_resources board tile_indices player =
  List.iter
    (fun tile_index ->
      let tile = board.(tile_index) in
      add_resource tile.resource player)
    tile_indices

let color_of_player int =
  match int with
  | 0 -> ANSITerminal.red
  | 1 -> ANSITerminal.blue
  | 2 -> ANSITerminal.yellow
  | 3 -> ANSITerminal.green
  | _ -> ANSITerminal.default

let place_settlement board player place i bool =
  let adjacent_tiles = get_adjacent_tiles place in

  List.iter
    (fun tile_index ->
      board.(tile_index).player <- player :: board.(tile_index).player)
    adjacent_tiles;
  places.(place) <- ("s", color_of_player i);
  if bool then distribute_resources board adjacent_tiles player

let roads = Array.make 72 ANSITerminal.default

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n                         "
    ^ ANSITerminal.sprintf [ snd places.(0) ] "%s" (fst places.(0))
    ^ ANSITerminal.sprintf [ roads.(0) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(1) ] "%s" (fst places.(1))
    ^ "\n                        "
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ "\n              "
    ^ ANSITerminal.sprintf [ snd places.(2) ] "%s" (fst places.(2))
    ^ ANSITerminal.sprintf [ roads.(3) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(3) ] "%s" (fst places.(3))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ " "
    ^ print_tile board.(0)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(4) ] "%s" (fst places.(4))
    ^ ANSITerminal.sprintf [ roads.(4) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(5) ] "%s" (fst places.(5))
    ^ "\n              "
    ^ ANSITerminal.sprintf [ roads.(5) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(6) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(7) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(8) ] "\\"
    ^ "\n             "
    ^ ANSITerminal.sprintf [ roads.(5) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(6) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(7) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(8) ] "\\"
    ^ "\n    "
    ^ ANSITerminal.sprintf [ snd places.(6) ] "%s" (fst places.(6))
    ^ ANSITerminal.sprintf [ roads.(9) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(7) ] "%s" (fst places.(7))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(5) ] "/"
    ^ " "
    ^ print_tile board.(1)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(6) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(8) ] "%s" (fst places.(8))
    ^ ANSITerminal.sprintf [ roads.(10) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(9) ] "%s" (fst places.(9))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(7) ] "/"
    ^ " "
    ^ print_tile board.(2)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(8) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(10) ] "%s" (fst places.(10))
    ^ ANSITerminal.sprintf [ roads.(11) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(11) ] "%s" (fst places.(11))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ roads.(12) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(13) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(14) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(15) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(16) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(17) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(12) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(13) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(14) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(15) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(16) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(17) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf [ snd places.(12) ] "%s" (fst places.(12))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(12) ] "/"
    ^ " "
    ^ print_tile board.(3)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(13) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(13) ] "%s" (fst places.(13))
    ^ ANSITerminal.sprintf [ roads.(18) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(14) ] "%s" (fst places.(14))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(14) ] "/"
    ^ " "
    ^ print_tile board.(4)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(15) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(15) ] "%s" (fst places.(15))
    ^ ANSITerminal.sprintf [ roads.(19) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(16) ] "%s" (fst places.(16))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(16) ] "/"
    ^ " "
    ^ print_tile board.(5)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(17) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(17) ] "%s" (fst places.(17))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ roads.(20) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(21) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(22) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(23) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(24) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(25) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(20) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(21) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(22) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(23) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(24) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(25) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf [ snd places.(18) ] "%s" (fst places.(18))
    ^ ANSITerminal.sprintf [ roads.(26) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(19) ] "%s" (fst places.(19))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(21) ] "/"
    ^ " "
    ^ print_tile board.(6)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(22) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(20) ] "%s" (fst places.(20))
    ^ ANSITerminal.sprintf [ roads.(27) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(21) ] "%s" (fst places.(21))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(23) ] "/"
    ^ " "
    ^ print_tile board.(7)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(24) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(22) ] "%s" (fst places.(22))
    ^ ANSITerminal.sprintf [ roads.(28) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(23) ] "%s" (fst places.(23))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ roads.(29) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(30) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(31) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(32) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(33) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(34) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(29) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(30) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(31) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(32) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(33) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(34) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf [ snd places.(24) ] "%s" (fst places.(24))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(29) ] "/"
    ^ " "
    ^ print_tile board.(8)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(30) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(25) ] "%s" (fst places.(25))
    ^ ANSITerminal.sprintf [ roads.(35) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(26) ] "%s" (fst places.(26))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(31) ] "/"
    ^ " "
    ^ print_tile board.(9)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(32) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(27) ] "%s" (fst places.(27))
    ^ ANSITerminal.sprintf [ roads.(36) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(28) ] "%s" (fst places.(28))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(33) ] "/"
    ^ " "
    ^ print_tile board.(10)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(34) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(29) ] "%s" (fst places.(29))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ roads.(37) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(38) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(39) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(40) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(41) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(42) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(37) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(38) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(39) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(40) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(41) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(42) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf [ snd places.(30) ] "%s" (fst places.(30))
    ^ ANSITerminal.sprintf [ roads.(43) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(31) ] "%s" (fst places.(31))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(38) ] "/"
    ^ " "
    ^ print_tile board.(11)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(39) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(32) ] "%s" (fst places.(32))
    ^ ANSITerminal.sprintf [ roads.(44) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(33) ] "%s" (fst places.(33))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(40) ] "/"
    ^ " "
    ^ print_tile board.(12)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(41) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(34) ] "%s" (fst places.(34))
    ^ ANSITerminal.sprintf [ roads.(45) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(35) ] "%s" (fst places.(35))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ roads.(46) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(47) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(48) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(49) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(50) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(51) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(46) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(47) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(48) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(49) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(50) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(51) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf [ snd places.(36) ] "%s" (fst places.(36))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(46) ] "/"
    ^ " "
    ^ print_tile board.(13)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(47) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(37) ] "%s" (fst places.(37))
    ^ ANSITerminal.sprintf [ roads.(52) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(38) ] "%s" (fst places.(38))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(48) ] "/"
    ^ " "
    ^ print_tile board.(14)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(49) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(39) ] "%s" (fst places.(39))
    ^ ANSITerminal.sprintf [ roads.(53) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(40) ] "%s" (fst places.(40))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(50) ] "/"
    ^ " "
    ^ print_tile board.(15)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(51) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(41) ] "%s" (fst places.(41))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ roads.(54) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(55) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(56) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(57) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(58) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(59) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ roads.(54) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(55) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(56) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(57) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(58) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(59) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf [ snd places.(42) ] "%s" (fst places.(42))
    ^ ANSITerminal.sprintf [ roads.(60) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(43) ] "%s" (fst places.(43))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(55) ] "/"
    ^ " "
    ^ print_tile board.(16)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(56) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(44) ] "%s" (fst places.(44))
    ^ ANSITerminal.sprintf [ roads.(61) ] "_____"
    ^ ANSITerminal.sprintf [ snd places.(45) ] "%s" (fst places.(45))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(57) ] "/"
    ^ " "
    ^ print_tile board.(17)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(58) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(46) ] "%s" (fst places.(46))
    ^ ANSITerminal.sprintf [ roads.(62) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(47) ] "%s" (fst places.(47))
    ^ "\n            "
    ^ ANSITerminal.sprintf [ roads.(63) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(64) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(65) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(66) ] "/"
    ^ "\n             "
    ^ ANSITerminal.sprintf [ roads.(63) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(64) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(65) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(66) ] "/"
    ^ "\n              "
    ^ ANSITerminal.sprintf [ snd places.(48) ] "%s" (fst places.(48))
    ^ ANSITerminal.sprintf [ roads.(67) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(49) ] "%s" (fst places.(49))
    ^ ""
    ^ ANSITerminal.sprintf [ roads.(64) ] "/"
    ^ " "
    ^ print_tile board.(18)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(65) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf [ snd places.(50) ] "%s" (fst places.(50))
    ^ ANSITerminal.sprintf [ roads.(68) ] "______"
    ^ ANSITerminal.sprintf [ snd places.(51) ] "%s" (fst places.(51))
    ^ "\n                      "
    ^ ANSITerminal.sprintf [ roads.(69) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(70) ] "/"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ roads.(69) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(70) ] "/"
    ^ "\n                        "
    ^ ANSITerminal.sprintf [ snd places.(52) ] "%s" (fst places.(52))
    ^ ANSITerminal.sprintf [ roads.(71) ] "_______"
    ^ ANSITerminal.sprintf [ snd places.(53) ] "%s" (fst places.(53))
    ^ "\n\n \n")
