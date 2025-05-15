(** Main entry point for the Settlers of Catan board generator application. *)

open Final_project

(** [main] starts the game loop. *)

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

(** [display_player_resources player] prints the resources for the player. *)
let display_player_resources player =
  print_endline "\nYour resources:";
  Player.print_hand player

let build_road_action player =
  if
    Player.amt_resource "brick" player < 1
    || Player.amt_resource "wood" player < 1
  then begin
    print_endline "\nYou don't have enough resources to build a road.";
    print_endline "A road requires 1 brick and 1 wood.";
    false
  end
  else begin
    print_endline "\nSelect a location to build a road (enter any number):";
    let _ = read_line () in

    (* Deduct resources for road building - using let _ to ignore the boolean
       return value *)
    let _ = Player.remove_resource "brick" player 1 in
    let _ = Player.remove_resource "wood" player 1 in

    print_endline "\nRoad built successfully!";
    display_player_resources player;
    true
  end

(** [trade_with_bank player] handles bank trading action. *)
let trade_with_bank player =
  let resource_options = [ "sheep"; "wood"; "ore"; "brick"; "wheat" ] in

  print_endline "\n=== TRADE WITH BANK ===";
  print_endline
    "The bank requires 4 of the same resource for 1 of any other resource.";
  display_player_resources player;

  print_endline
    "\n\
     Which resource would you like to give to the bank? \
     (sheep/wood/ore/brick/wheat)";
  let give_resource = read_line () in

  if not (List.mem give_resource resource_options) then begin
    print_endline "Invalid resource name. Trade cancelled.";
    false
  end
  else if Player.amt_resource give_resource player < 4 then begin
    print_endline
      ("You don't have enough " ^ give_resource
     ^ " to trade. You need at least 4.");
    false
  end
  else begin
    print_endline
      "\n\
       Which resource would you like to receive from the bank? \
       (sheep/wood/ore/brick/wheat)";
    let receive_resource = read_line () in

    if not (List.mem receive_resource resource_options) then begin
      print_endline "Invalid resource name. Trade cancelled.";
      false
    end
    else begin
      let _ = Player.remove_resource give_resource player 4 in
      Player.increment_resource receive_resource player;

      print_endline
        ("\nTrade completed: Gave 4 " ^ give_resource ^ " to the bank for 1 "
       ^ receive_resource ^ ".");
      display_player_resources player;
      true
    end
  end

let color_of_player int =
  match int with
  | 0 -> "RED"
  | 1 -> "BLUE"
  | 2 -> "YELLOW"
  | 3 -> "GREEN"
  | _ -> ""

(** [player_turn board player_idx] handles the turn for a specific player. *)
let player_turn board player_idx =
  let player = List.nth Game.players player_idx in

  print_endline
    ("\n\n=== PLAYER "
    ^ string_of_int (player_idx + 1)
    ^ "'S TURN ===" ^ "(" ^ color_of_player player_idx ^ ")");
  display_player_resources player;

  print_endline "\nRolling dice...";
  Game.roll_dice board;
  display_player_resources player;

  let rec action_phase () =
    print_endline "\nWhat would you like to do?";
    print_endline "1. Build a road (costs 1 brick, 1 wood)";
    print_endline "2. Trade with the bank (4:1 ratio)";
    print_endline "3. View board";
    print_endline "4. End turn";
    print_endline "\nEnter your choice (1-4): ";

    match read_line () with
    | "1" ->
        let _ = build_road_action player in
        action_phase ()
    | "2" ->
        let _ = trade_with_bank player in
        action_phase ()
    | "3" ->
        Board.print board;
        action_phase ()
    | "4" ->
        print_endline
          ("\nPlayer " ^ string_of_int (player_idx + 1) ^ "'s turn ended.");
        ()
    | _ ->
        print_endline "Invalid choice. Please enter a number between 1 and 4.";
        action_phase ()
  in

  action_phase ()

let game_round board num_players current_round =
  print_endline ("\n\n====== ROUND " ^ string_of_int current_round ^ " ======");

  let rec process_player_turns player_idx =
    if player_idx < num_players then begin
      player_turn board player_idx;
      process_player_turns (player_idx + 1)
    end
  in

  process_player_turns 0

(** [play_game board] runs the main game loop with multiple rounds. *)
let play_game board =
  let num_players = List.length Game.players in

  print_endline "\nPlacing initial settlements...";
  Game.initialize_game board;

  let rec game_rounds round =
    game_round board num_players round;

    print_endline "\nContinue to the next round? (y/n): ";
    match read_line () with
    | "y" | "Y" -> game_rounds (round + 1)
    | _ -> print_endline "\nGame ended."
  in

  game_rounds 1

let rec game_loop () =
  print_introduction ();

  if main_menu () then begin
    print_endline "";
    let board = Board.create () in
    Board.print board;
    play_game board;

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
        print_endline "\nThank you for playing Settlers of Catan!";
        print_endline "Goodbye!\n"
  end
  else print_endline "\nThank you for playing Settlers of Catan!"

(** Entry point of the program. *)
let () =
  try game_loop () with
  | Failure msg ->
      print_endline ("\nError: " ^ msg);
      print_endline "Game terminated due to an error."
  | _ ->
      print_endline "\nAn unexpected error occurred.";
      print_endline "Game terminated."
