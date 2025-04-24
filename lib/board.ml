(** Board representation for Settlers of Catan. *)

type resource =
  | Sheep
  | Wood
  | Ore
  | Brick
  | Wheat
  | Desert

type tile = resource * int
type board = tile array

let print_tile (x, y) =
  match x with
  | Sheep -> "Sheep " ^ string_of_int y ^ " "
  | Wood -> " Wood " ^ string_of_int y ^ " "
  | Ore -> "  Ore " ^ string_of_int y ^ " "
  | Brick -> "Brick " ^ string_of_int y ^ " "
  | Wheat -> "Wheat " ^ string_of_int y ^ " "
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

  (* Create the board with resource/number pairs *)
  Array.init 19 (fun i -> (resources.(i), numbers.(i)))

let print board =
  print_endline "============================================";
  print_endline "Here is your generated Catan board:";
  print_endline
    ("\n\
     \                        _______\n\
     \                       /       \\\n\
     \                      /         \\\n\
     \              _______/ "
    ^ print_tile board.(0)
    ^ "  \\_______\n\
      \             /       \\           /       \\\n\
      \            /         \\         /         \\\n\
      \    _______/ "
    ^ print_tile board.(1)
    ^ "  \\_______/ "
    ^ print_tile board.(2)
    ^ "  \\_______\n\
      \   /       \\           /       \\           /       \\\n\
      \  /         \\         /         \\         /         \\\n\
      \ / "
    ^ print_tile board.(3)
    ^ "  \\_______/ "
    ^ print_tile board.(4)
    ^ "  \\_______/ "
    ^ print_tile board.(5)
    ^ "  \\\n\
      \ \\           /       \\           /       \\           /\n\
      \  \\         /         \\         /         \\         /\n\
      \   \\_______/ "
    ^ print_tile board.(6)
    ^ "  \\_______/ "
    ^ print_tile board.(7)
    ^ "  \\_______/\n\
      \   /       \\           /       \\           /       \\\n\
      \  /         \\         /         \\         /         \\\n\
      \ / "
    ^ print_tile board.(8)
    ^ "  \\_______/ "
    ^ print_tile board.(9)
    ^ "  \\_______/ "
    ^ print_tile board.(10)
    ^ "  \\\n\
      \ \\           /       \\           /       \\           /\n\
      \  \\         /         \\         /         \\         /\n\
      \   \\_______/ "
    ^ print_tile board.(11)
    ^ "  \\_______/ "
    ^ print_tile board.(12)
    ^ "  \\_______/\n\
      \   /       \\           /       \\           /       \\\n\
      \  /         \\         /         \\         /         \\\n\
      \ / "
    ^ print_tile board.(13)
    ^ "  \\_______/ "
    ^ print_tile board.(14)
    ^ "  \\_______/ "
    ^ print_tile board.(15)
    ^ "  \\\n\
      \ \\           /       \\           /       \\           /\n\
      \  \\         /         \\         /         \\         /\n\
      \   \\_______/ "
    ^ print_tile board.(16)
    ^ "  \\_______/ "
    ^ print_tile board.(17)
    ^ "  \\_______/\n\
      \           \\           /       \\           /\n\
      \            \\         /         \\         /\n\
      \             \\_______/ "
    ^ print_tile board.(18)
    ^ "  \\_______/ \n\
      \                     \\           /\n\
      \                      \\         /\n\
      \                       \\_______/\n\n\
      \ \n")
