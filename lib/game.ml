(** Game interface for Settlers of Catan. *)

let print_introduction () =
  print_endline "\n=== SETTLERS OF CATAN ===";
  print_endline "Welcome to the Settlers of Catan!";
  print_endline "";
  print_endline "About the Game:";
  print_endline
    "Settlers of Catan is a popular board game where players collect resources";
  print_endline "to build settlements, cities, and roads on a hexagonal board.";
  print_endline "";
  print_endline "Board Information:";
  print_endline
    "- The board consists of 19 hexagonal tiles representing different \
     resources";
  print_endline "- Resources: Sheep, Wood, Ore, Brick, Wheat, and one Desert";
  print_endline "- Each tile (except Desert) has a number token (2-12)";
  print_endline
    "- When a number is rolled, all tiles with that number produce resources";
  print_endline ""

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
    let board = Board.create () in
    Board.print board;

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
  end
  else print_endline "\nThank you for playing Settlers of Catan!"

(* logic for rolling dice *)
let () = Random.self_init ()
let roll_x x = 1 + Random.int x

let add_resource resource player =
  match resource with
  | Board.Sheep -> Player.increment_resource "sheep" player
  | Board.Wood -> Player.increment_resource "wood" player
  | Board.Ore -> Player.increment_resource "ore" player
  | Board.Brick -> Player.increment_resource "brick" player
  | Board.Wheat -> Player.increment_resource "wheat" player
  | Board.Desert -> ()

let roll_dice board players =
  let res = roll_x 6 + roll_x 6 in
  Array.iter
    (fun (a, b) ->
      if b = res then List.iter (fun x -> add_resource a x) players)
    board
