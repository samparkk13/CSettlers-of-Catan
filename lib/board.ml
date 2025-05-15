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

let tiles (x, _, _) = x
let places (_, x, _) = x
let roads (_, _, x) = x

type board = tile array * (string * style) array * style array

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
  ( Array.init 19 (fun i ->
        { resource = resources.(i); num = numbers.(i); player = [] }),
    Array.make 54 ("o", ANSITerminal.default),
    Array.make 72 ANSITerminal.default )

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
      let tile = (tiles board).(tile_index) in
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
      (tiles board).(tile_index).player <-
        player :: (tiles board).(tile_index).player)
    adjacent_tiles;
  (places board).(place) <- ("s", color_of_player i);
  if bool then distribute_resources board adjacent_tiles player

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n                         "
    ^ ANSITerminal.sprintf
        [ snd (places board).(0) ]
        "%s"
        (fst (places board).(0))
    ^ ANSITerminal.sprintf [ (roads board).(0) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(1) ]
        "%s"
        (fst (places board).(1))
    ^ "\n                        "
    ^ ANSITerminal.sprintf [ (roads board).(1) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(2) ] "\\"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ (roads board).(1) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(2) ] "\\"
    ^ "\n              "
    ^ ANSITerminal.sprintf
        [ snd (places board).(2) ]
        "%s"
        (fst (places board).(2))
    ^ ANSITerminal.sprintf [ (roads board).(3) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(3) ]
        "%s"
        (fst (places board).(3))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(1) ] "/"
    ^ " "
    ^ print_tile (tiles board).(0)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(2) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(4) ]
        "%s"
        (fst (places board).(4))
    ^ ANSITerminal.sprintf [ (roads board).(4) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(5) ]
        "%s"
        (fst (places board).(5))
    ^ "\n              "
    ^ ANSITerminal.sprintf [ (roads board).(5) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(6) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(7) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(8) ] "\\"
    ^ "\n             "
    ^ ANSITerminal.sprintf [ (roads board).(5) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(6) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(7) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(8) ] "\\"
    ^ "\n    "
    ^ ANSITerminal.sprintf
        [ snd (places board).(6) ]
        "%s"
        (fst (places board).(6))
    ^ ANSITerminal.sprintf [ (roads board).(9) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(7) ]
        "%s"
        (fst (places board).(7))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(5) ] "/"
    ^ " "
    ^ print_tile (tiles board).(1)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(6) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(8) ]
        "%s"
        (fst (places board).(8))
    ^ ANSITerminal.sprintf [ (roads board).(10) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(9) ]
        "%s"
        (fst (places board).(9))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(7) ] "/"
    ^ " "
    ^ print_tile (tiles board).(2)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(8) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(10) ]
        "%s"
        (fst (places board).(10))
    ^ ANSITerminal.sprintf [ (roads board).(11) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(11) ]
        "%s"
        (fst (places board).(11))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ (roads board).(12) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(13) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(14) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(15) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(16) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(17) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(12) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(13) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(14) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(15) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(16) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(17) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf
        [ snd (places board).(12) ]
        "%s"
        (fst (places board).(12))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(12) ] "/"
    ^ " "
    ^ print_tile (tiles board).(3)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(13) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(13) ]
        "%s"
        (fst (places board).(13))
    ^ ANSITerminal.sprintf [ (roads board).(18) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(14) ]
        "%s"
        (fst (places board).(14))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(14) ] "/"
    ^ " "
    ^ print_tile (tiles board).(4)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(15) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(15) ]
        "%s"
        (fst (places board).(15))
    ^ ANSITerminal.sprintf [ (roads board).(19) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(16) ]
        "%s"
        (fst (places board).(16))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(16) ] "/"
    ^ " "
    ^ print_tile (tiles board).(5)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(17) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(17) ]
        "%s"
        (fst (places board).(17))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ (roads board).(20) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(21) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(22) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(23) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(24) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(25) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(20) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(21) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(22) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(23) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(24) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(25) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf
        [ snd (places board).(18) ]
        "%s"
        (fst (places board).(18))
    ^ ANSITerminal.sprintf [ (roads board).(26) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(19) ]
        "%s"
        (fst (places board).(19))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(21) ] "/"
    ^ " "
    ^ print_tile (tiles board).(6)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(22) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(20) ]
        "%s"
        (fst (places board).(20))
    ^ ANSITerminal.sprintf [ (roads board).(27) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(21) ]
        "%s"
        (fst (places board).(21))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(23) ] "/"
    ^ " "
    ^ print_tile (tiles board).(7)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(24) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(22) ]
        "%s"
        (fst (places board).(22))
    ^ ANSITerminal.sprintf [ (roads board).(28) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(23) ]
        "%s"
        (fst (places board).(23))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ (roads board).(29) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(30) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(31) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(32) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(33) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(34) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(29) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(30) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(31) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(32) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(33) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(34) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf
        [ snd (places board).(24) ]
        "%s"
        (fst (places board).(24))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(29) ] "/"
    ^ " "
    ^ print_tile (tiles board).(8)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(30) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(25) ]
        "%s"
        (fst (places board).(25))
    ^ ANSITerminal.sprintf [ (roads board).(35) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(26) ]
        "%s"
        (fst (places board).(26))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(31) ] "/"
    ^ " "
    ^ print_tile (tiles board).(9)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(32) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(27) ]
        "%s"
        (fst (places board).(27))
    ^ ANSITerminal.sprintf [ (roads board).(36) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(28) ]
        "%s"
        (fst (places board).(28))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(33) ] "/"
    ^ " "
    ^ print_tile (tiles board).(10)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(34) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(29) ]
        "%s"
        (fst (places board).(29))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ (roads board).(37) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(38) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(39) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(40) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(41) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(42) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(37) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(38) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(39) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(40) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(41) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(42) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf
        [ snd (places board).(30) ]
        "%s"
        (fst (places board).(30))
    ^ ANSITerminal.sprintf [ (roads board).(43) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(31) ]
        "%s"
        (fst (places board).(31))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(38) ] "/"
    ^ " "
    ^ print_tile (tiles board).(11)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(39) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(32) ]
        "%s"
        (fst (places board).(32))
    ^ ANSITerminal.sprintf [ (roads board).(44) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(33) ]
        "%s"
        (fst (places board).(33))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(40) ] "/"
    ^ " "
    ^ print_tile (tiles board).(12)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(41) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(34) ]
        "%s"
        (fst (places board).(34))
    ^ ANSITerminal.sprintf [ (roads board).(45) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(35) ]
        "%s"
        (fst (places board).(35))
    ^ "\n    "
    ^ ANSITerminal.sprintf [ (roads board).(46) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(47) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(48) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(49) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(50) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(51) ] "\\"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(46) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(47) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(48) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(49) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(50) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(51) ] "\\"
    ^ "\n "
    ^ ANSITerminal.sprintf
        [ snd (places board).(36) ]
        "%s"
        (fst (places board).(36))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(46) ] "/"
    ^ " "
    ^ print_tile (tiles board).(13)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(47) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(37) ]
        "%s"
        (fst (places board).(37))
    ^ ANSITerminal.sprintf [ (roads board).(52) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(38) ]
        "%s"
        (fst (places board).(38))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(48) ] "/"
    ^ " "
    ^ print_tile (tiles board).(14)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(49) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(39) ]
        "%s"
        (fst (places board).(39))
    ^ ANSITerminal.sprintf [ (roads board).(53) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(40) ]
        "%s"
        (fst (places board).(40))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(50) ] "/"
    ^ " "
    ^ print_tile (tiles board).(15)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(51) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(41) ]
        "%s"
        (fst (places board).(41))
    ^ "\n  "
    ^ ANSITerminal.sprintf [ (roads board).(54) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(55) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(56) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(57) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(58) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(59) ] "/"
    ^ "\n   "
    ^ ANSITerminal.sprintf [ (roads board).(54) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(55) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(56) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(57) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(58) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(59) ] "/"
    ^ "\n    "
    ^ ANSITerminal.sprintf
        [ snd (places board).(42) ]
        "%s"
        (fst (places board).(42))
    ^ ANSITerminal.sprintf [ (roads board).(60) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(43) ]
        "%s"
        (fst (places board).(43))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(55) ] "/"
    ^ " "
    ^ print_tile (tiles board).(16)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(56) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(44) ]
        "%s"
        (fst (places board).(44))
    ^ ANSITerminal.sprintf [ (roads board).(61) ] "_____"
    ^ ANSITerminal.sprintf
        [ snd (places board).(45) ]
        "%s"
        (fst (places board).(45))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(57) ] "/"
    ^ " "
    ^ print_tile (tiles board).(17)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(58) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(46) ]
        "%s"
        (fst (places board).(46))
    ^ ANSITerminal.sprintf [ (roads board).(62) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(47) ]
        "%s"
        (fst (places board).(47))
    ^ "\n            "
    ^ ANSITerminal.sprintf [ (roads board).(63) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(64) ] "/"
    ^ "       "
    ^ ANSITerminal.sprintf [ (roads board).(65) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(66) ] "/"
    ^ "\n             "
    ^ ANSITerminal.sprintf [ (roads board).(63) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(64) ] "/"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(65) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(66) ] "/"
    ^ "\n              "
    ^ ANSITerminal.sprintf
        [ snd (places board).(48) ]
        "%s"
        (fst (places board).(48))
    ^ ANSITerminal.sprintf [ (roads board).(67) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(49) ]
        "%s"
        (fst (places board).(49))
    ^ ""
    ^ ANSITerminal.sprintf [ (roads board).(64) ] "/"
    ^ " "
    ^ print_tile (tiles board).(18)
    ^ "  "
    ^ ANSITerminal.sprintf [ (roads board).(65) ] "\\"
    ^ ""
    ^ ANSITerminal.sprintf
        [ snd (places board).(50) ]
        "%s"
        (fst (places board).(50))
    ^ ANSITerminal.sprintf [ (roads board).(68) ] "______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(51) ]
        "%s"
        (fst (places board).(51))
    ^ "\n                      "
    ^ ANSITerminal.sprintf [ (roads board).(69) ] "\\"
    ^ "           "
    ^ ANSITerminal.sprintf [ (roads board).(70) ] "/"
    ^ "\n                       "
    ^ ANSITerminal.sprintf [ (roads board).(69) ] "\\"
    ^ "         "
    ^ ANSITerminal.sprintf [ (roads board).(70) ] "/"
    ^ "\n                        "
    ^ ANSITerminal.sprintf
        [ snd (places board).(52) ]
        "%s"
        (fst (places board).(52))
    ^ ANSITerminal.sprintf [ (roads board).(71) ] "_______"
    ^ ANSITerminal.sprintf
        [ snd (places board).(53) ]
        "%s"
        (fst (places board).(53))
    ^ "\n\n \n")
