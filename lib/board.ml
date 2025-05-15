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

let place_village board player place bool =
  let adjacent_tiles = get_adjacent_tiles place in

  List.iter
    (fun tile_index ->
      board.(tile_index).player <- player :: board.(tile_index).player)
    adjacent_tiles;
  places.(place) <- "v";
  if bool then distribute_resources board adjacent_tiles player

let roads =
  Array.init 72 (fun i ->
      if i mod 2 = 0 then ANSITerminal.red else ANSITerminal.blue)

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n                         " ^ places.(0)
    ^ ANSITerminal.sprintf [ roads.(0) ] "______"
    ^ places.(1) ^ "\n                        "
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ "\n              " ^ places.(2)
    ^ ANSITerminal.sprintf [ roads.(3) ] "______"
    ^ places.(3) ^ ""
    ^ ANSITerminal.sprintf [ roads.(1) ] "/"
    ^ " "
    ^ print_tile board.(0)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(2) ] "\\"
    ^ "" ^ places.(4)
    ^ ANSITerminal.sprintf [ roads.(4) ] "______"
    ^ places.(5) ^ "\n              "
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
    ^ "\n    " ^ places.(6)
    ^ ANSITerminal.sprintf [ roads.(9) ] "______"
    ^ places.(7) ^ ""
    ^ ANSITerminal.sprintf [ roads.(5) ] "/"
    ^ " "
    ^ print_tile board.(1)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(6) ] "\\"
    ^ "" ^ places.(8)
    ^ ANSITerminal.sprintf [ roads.(10) ] "_____"
    ^ places.(9) ^ ""
    ^ ANSITerminal.sprintf [ roads.(7) ] "/"
    ^ " "
    ^ print_tile board.(2)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(8) ] "\\"
    ^ "" ^ places.(10)
    ^ ANSITerminal.sprintf [ roads.(11) ] "______"
    ^ places.(11) ^ "\n    "
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
    ^ "\n " ^ places.(12) ^ ""
    ^ ANSITerminal.sprintf [ roads.(12) ] "/"
    ^ " "
    ^ print_tile board.(3)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(13) ] "\\"
    ^ "" ^ places.(13)
    ^ ANSITerminal.sprintf [ roads.(18) ] "_____"
    ^ places.(14) ^ ""
    ^ ANSITerminal.sprintf [ roads.(14) ] "/"
    ^ " "
    ^ print_tile board.(4)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(15) ] "\\"
    ^ "" ^ places.(15)
    ^ ANSITerminal.sprintf [ roads.(19) ] "_____"
    ^ places.(16) ^ ""
    ^ ANSITerminal.sprintf [ roads.(16) ] "/"
    ^ " "
    ^ print_tile board.(5)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(17) ] "\\"
    ^ "" ^ places.(17) ^ "\n  "
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
    ^ "\n    " ^ places.(18)
    ^ ANSITerminal.sprintf [ roads.(26) ] "______"
    ^ places.(19) ^ ""
    ^ ANSITerminal.sprintf [ roads.(21) ] "/"
    ^ " "
    ^ print_tile board.(6)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(22) ] "\\"
    ^ "" ^ places.(20)
    ^ ANSITerminal.sprintf [ roads.(27) ] "_____"
    ^ places.(21) ^ ""
    ^ ANSITerminal.sprintf [ roads.(23) ] "/"
    ^ " "
    ^ print_tile board.(7)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(24) ] "\\"
    ^ "" ^ places.(22)
    ^ ANSITerminal.sprintf [ roads.(28) ] "______"
    ^ places.(23) ^ "\n    "
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
    ^ "\n " ^ places.(24) ^ ""
    ^ ANSITerminal.sprintf [ roads.(29) ] "/"
    ^ " "
    ^ print_tile board.(8)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(30) ] "\\"
    ^ "" ^ places.(25)
    ^ ANSITerminal.sprintf [ roads.(35) ] "_____"
    ^ places.(26) ^ ""
    ^ ANSITerminal.sprintf [ roads.(31) ] "/"
    ^ " "
    ^ print_tile board.(9)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(32) ] "\\"
    ^ "" ^ places.(27)
    ^ ANSITerminal.sprintf [ roads.(36) ] "_____"
    ^ places.(28) ^ ""
    ^ ANSITerminal.sprintf [ roads.(33) ] "/"
    ^ " "
    ^ print_tile board.(10)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(34) ] "\\"
    ^ "" ^ places.(29) ^ "\n  "
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
    ^ "\n    " ^ places.(30)
    ^ ANSITerminal.sprintf [ roads.(43) ] "______"
    ^ places.(31) ^ ""
    ^ ANSITerminal.sprintf [ roads.(38) ] "/"
    ^ " "
    ^ print_tile board.(11)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(39) ] "\\"
    ^ "" ^ places.(32)
    ^ ANSITerminal.sprintf [ roads.(44) ] "_____"
    ^ places.(33) ^ ""
    ^ ANSITerminal.sprintf [ roads.(40) ] "/"
    ^ " "
    ^ print_tile board.(12)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(41) ] "\\"
    ^ "" ^ places.(34)
    ^ ANSITerminal.sprintf [ roads.(45) ] "______"
    ^ places.(35) ^ "\n    "
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
    ^ "\n " ^ places.(36) ^ ""
    ^ ANSITerminal.sprintf [ roads.(46) ] "/"
    ^ " "
    ^ print_tile board.(13)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(47) ] "\\"
    ^ "" ^ places.(37)
    ^ ANSITerminal.sprintf [ roads.(52) ] "_____"
    ^ places.(38) ^ ""
    ^ ANSITerminal.sprintf [ roads.(48) ] "/"
    ^ " "
    ^ print_tile board.(14)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(49) ] "\\"
    ^ "" ^ places.(39)
    ^ ANSITerminal.sprintf [ roads.(53) ] "_____"
    ^ places.(40) ^ ""
    ^ ANSITerminal.sprintf [ roads.(50) ] "/"
    ^ " "
    ^ print_tile board.(15)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(51) ] "\\"
    ^ "" ^ places.(41) ^ "\n  "
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
    ^ "\n    " ^ places.(42)
    ^ ANSITerminal.sprintf [ roads.(60) ] "______"
    ^ places.(43) ^ ""
    ^ ANSITerminal.sprintf [ roads.(55) ] "/"
    ^ " "
    ^ print_tile board.(16)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(56) ] "\\"
    ^ "" ^ places.(44)
    ^ ANSITerminal.sprintf [ roads.(61) ] "_____"
    ^ places.(45) ^ ""
    ^ ANSITerminal.sprintf [ roads.(57) ] "/"
    ^ " "
    ^ print_tile board.(17)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(58) ] "\\"
    ^ "" ^ places.(46)
    ^ ANSITerminal.sprintf [ roads.(62) ] "______"
    ^ places.(47) ^ "\n            "
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
    ^ "\n              " ^ places.(48)
    ^ ANSITerminal.sprintf [ roads.(67) ] "______"
    ^ places.(49) ^ ""
    ^ ANSITerminal.sprintf [ roads.(64) ] "/"
    ^ " "
    ^ print_tile board.(18)
    ^ "  "
    ^ ANSITerminal.sprintf [ roads.(65) ] "\\"
    ^ "" ^ places.(50)
    ^ ANSITerminal.sprintf [ roads.(68) ] "______"
    ^ places.(51) ^ "\n                      "
    ^ ANSITerminal.sprintf [ roads.(69) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ roads.(70) ] "/"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ roads.(69) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ roads.(70) ] "/"
    ^ "\n                        " ^ places.(52)
    ^ ANSITerminal.sprintf [ roads.(71) ] "_______"
    ^ places.(53) ^ "\n\n \n")
