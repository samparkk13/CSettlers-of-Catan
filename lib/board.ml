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

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n\
     \                         A______A\n\
     \                        /       \\\n\
     \                       /         \\\n\
     \              A______A/ "
    ^ print_tile board.(0)
    ^ "  \\A______A\n\
      \              /       \\           /       \\\n\
      \             /         \\         /         \\\n\
      \    A______A/ "
    ^ print_tile board.(1)
    ^ "  \\A_____A/ "
    ^ print_tile board.(2)
    ^ "  \\A______A\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ A/ "
    ^ print_tile board.(3)
    ^ "  \\A_____A/ "
    ^ print_tile board.(4)
    ^ "  \\A_____A/ "
    ^ print_tile board.(5)
    ^ "  \\A\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    A______A/ "
    ^ print_tile board.(6)
    ^ "  \\A_____A/ "
    ^ print_tile board.(7)
    ^ "  \\A______A\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ A/ "
    ^ print_tile board.(8)
    ^ "  \\A_____A/ "
    ^ print_tile board.(9)
    ^ "  \\A_____A/ "
    ^ print_tile board.(10)
    ^ "  \\A\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    A______A/ "
    ^ print_tile board.(11)
    ^ "  \\A_____A/ "
    ^ print_tile board.(12)
    ^ "  \\A______A\n\
      \    /       \\           /       \\           /       \\\n\
      \   /         \\         /         \\         /         \\\n\
      \ A/ "
    ^ print_tile board.(13)
    ^ "  \\A_____A/ "
    ^ print_tile board.(14)
    ^ "  \\A_____A/ "
    ^ print_tile board.(15)
    ^ "  \\A\n\
      \  \\           /       \\           /       \\           /\n\
      \   \\         /         \\         /         \\         /\n\
      \    A______A/ "
    ^ print_tile board.(16)
    ^ "  \\A_____A/ "
    ^ print_tile board.(17)
    ^ "  \\A______A\n\
      \            \\           /       \\           /\n\
      \             \\         /         \\         /\n\
      \              A______A/ "
    ^ print_tile board.(18)
    ^ "  \\A______A\n\
      \                      \\           /\n\
      \                       \\         /\n\
      \                        A_______A\n\n\
      \ \n")
