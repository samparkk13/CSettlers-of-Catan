type resource =
  | Sheep 
  | Wood
  | Ore
  | Brick
  | Wheat
  | Desert

let print_tile (x, y) =
  match x with
  | Sheep -> "Sheep " ^ string_of_int y ^ " "
  | Wood -> " Wood " ^ string_of_int y ^ " "
  | Ore -> "  Ore " ^ string_of_int y ^ " "
  | Brick -> "Brick " ^ string_of_int y ^ " "
  | Wheat -> "Wheat " ^ string_of_int y ^ " "
  | Desert -> " Desert "

let create_board () =
  (* Create the board with resources in a fixed pattern *)
  let resources = [|
    Sheep; Wood; Brick;
    Wheat; Sheep; Ore; Wood;
    Brick; Desert; Wheat; Wheat;
    Wood; Sheep; Ore; Brick;
    Ore; Sheep; Wood; Wheat
  |] in
  
  (* Create number tokens in a fixed pattern *)
  let numbers = [|
    5; 2; 6;
    8; 10; 9; 12;
    3; 0; 11; 4;
    8; 4; 9; 5;
    10; 11; 3; 6
  |] in
  
  (* Create the board with resource/number pairs *)
  Array.init 19 (fun i -> (resources.(i), numbers.(i)))

let print_introduction () =
  print_endline "\n=== SETTLERS OF CATAN ===";
  print_endline "Welcome to the Settlers of Catan!";
  print_endline "";
  print_endline "About the Game:";
  print_endline "Settlers of Catan is a popular board game where players collect resources";
  print_endline "to build settlements, cities, and roads on a hexagonal board.";
  print_endline "";
  print_endline "Board Information:";
  print_endline "- The board consists of 19 hexagonal tiles representing different resources";
  print_endline "- Resources: Sheep, Wood, Ore, Brick, Wheat, and one Desert";
  print_endline "- Each tile (except Desert) has a number token (2-12)";
  print_endline "- When a number is rolled, all tiles with that number produce resources";
  print_endline ""

let print_board board =
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

let get_user_input () =
  print_endline "Press Enter to play...";
  let _ = read_line () in
  ()

let main_menu () =
  print_endline "\n=== MAIN MENU ===";
  print_endline "1. Play";
  print_endline "2. Quit";
  print_endline "";
  print_endline "Enter your choice (1-2): ";
  
  let rec get_choice () =
    match read_line () with
    | "1" -> true
    | "2" -> false
    | _ -> 
        print_endline "Invalid choice. Please enter 1 or 2: ";
        get_choice ()
  in
  get_choice ()

let rec game_loop () =
  print_introduction ();
  
  if main_menu () then begin
    print_endline "";
    let board = create_board () in
    print_board board;
    
    print_endline "\nWhat would you like to do next?";
    print_endline "1. Play again";
    print_endline "2. Quit";
    print_endline "";
    print_endline "Enter your choice (1-2): ";
    
    match read_line () with
    | "1" -> 
        print_endline "\n\n";
        game_loop ()
    | _ -> 
        print_endline "\nThank you for playing Settlers of Catan Board!";
        print_endline "Goodbye!\n"
  end else
    print_endline "\nThank you for playing Settlers of Catan!"

let () =
  game_loop ()