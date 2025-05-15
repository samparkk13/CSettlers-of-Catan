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

(** [get_adjacent_vertices vertex] returns a list of vertices that are directly
    connected to the given vertex by a single road. *)
let get_adjacent_vertices vertex =
  match vertex with
  | 0 -> [ 1; 3 ]
  | 1 -> [ 0; 4 ]
  | 2 -> [ 3; 7 ]
  | 3 -> [ 0; 2; 8 ]
  | 4 -> [ 1; 5; 9 ]
  | 5 -> [ 4; 10 ]
  | 6 -> [ 7; 12 ]
  | 7 -> [ 2; 6; 13 ]
  | 8 -> [ 3; 9; 14 ]
  | 9 -> [ 4; 8; 15 ]
  | 10 -> [ 5; 11; 16 ]
  | 11 -> [ 10; 17 ]
  | 12 -> [ 6; 13 ]
  | 13 -> [ 7; 12; 19 ]
  | 14 -> [ 8; 15; 20 ]
  | 15 -> [ 9; 14; 21 ]
  | 16 -> [ 10; 17; 22 ]
  | 17 -> [ 11; 16; 23 ]
  | 18 -> [ 19; 24 ]
  | 19 -> [ 13; 18; 25 ]
  | 20 -> [ 14; 21; 26 ]
  | 21 -> [ 15; 20; 27 ]
  | 22 -> [ 16; 23; 28 ]
  | 23 -> [ 17; 22; 29 ]
  | 24 -> [ 18; 25 ]
  | 25 -> [ 19; 24; 31 ]
  | 26 -> [ 20; 27; 32 ]
  | 27 -> [ 21; 26; 33 ]
  | 28 -> [ 22; 29; 34 ]
  | 29 -> [ 23; 28; 35 ]
  | 30 -> [ 31; 36 ]
  | 31 -> [ 25; 30; 37 ]
  | 32 -> [ 26; 33; 38 ]
  | 33 -> [ 27; 32; 39 ]
  | 34 -> [ 28; 35; 40 ]
  | 35 -> [ 29; 34; 41 ]
  | 36 -> [ 30; 37 ]
  | 37 -> [ 31; 36; 43 ]
  | 38 -> [ 32; 39; 44 ]
  | 39 -> [ 33; 38; 45 ]
  | 40 -> [ 34; 41; 46 ]
  | 41 -> [ 35; 40; 47 ]
  | 42 -> [ 43; 48 ]
  | 43 -> [ 37; 42; 49 ]
  | 44 -> [ 38; 45; 50 ]
  | 45 -> [ 39; 44; 51 ]
  | 46 -> [ 40; 47; 52 ]
  | 47 -> [ 41; 46; 53 ]
  | 48 -> [ 42; 49 ]
  | 49 -> [ 43; 48 ]
  | 50 -> [ 44; 51 ]
  | 51 -> [ 45; 50 ]
  | 52 -> [ 46; 53 ]
  | 53 -> [ 47; 52 ]
  | _ -> []

(** [is_vertex_occupied board vertex] checks if a given vertex already has a
    settlement. *)
let is_vertex_occupied board vertex =
  let current_place = fst (places board).(vertex) in
  current_place = "s" || current_place = "c"

(** [has_adjacent_settlement board vertex] checks if any vertex adjacent to the
    given vertex has a settlement or city. *)
let has_adjacent_settlement board vertex =
  let adjacent_vertices = get_adjacent_vertices vertex in
  List.exists (fun v -> is_vertex_occupied board v) adjacent_vertices

(** [can_place_settlement board vertex] checks if a settlement can be placed at
    the given vertex according to game rules: 1. The vertex must be empty 2. No
    adjacent vertex can have a settlement (2-road distance rule) *)
let can_place_settlement board vertex =
  (not (is_vertex_occupied board vertex))
  && not (has_adjacent_settlement board vertex)

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

(** [place_settlement board player place i bool] places a settlement at the
    given vertex if it's allowed by the game rules. Returns true if successful,
    false otherwise. *)
let place_settlement board player place i bool =
  if can_place_settlement board place then begin
    let adjacent_tiles = get_adjacent_tiles place in

    List.iter
      (fun tile_index ->
        (tiles board).(tile_index).player <-
          player :: (tiles board).(tile_index).player)
      adjacent_tiles;
    (places board).(place) <- ("s", color_of_player i);
    if bool then distribute_resources board adjacent_tiles player;
    true
  end
  else false

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
