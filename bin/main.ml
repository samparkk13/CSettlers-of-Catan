type resource =
  | Wool
  | Lumber
  | Ore
  | Brick
  | Grain

let print_tile (x, y) =
  match x with
  | Wool -> " Wool " ^ string_of_int y ^ " "
  | Lumber -> "Lumber " ^ string_of_int y
  | Ore -> "  Ore " ^ string_of_int y ^ " "
  | Brick -> " Brick " ^ string_of_int y
  | Grain -> " Grain " ^ string_of_int y

let () =
  let board = Array.make 19 (Wool, 7) in
  board.(2) <- (Lumber, 2);
  board.(5) <- (Ore, 4);
  board.(8) <- (Brick, 1);
  board.(11) <- (Grain, 6);
  board.(16) <- (Wool, 10);

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
