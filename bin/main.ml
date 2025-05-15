(** Main entry point for the Settlers of Catan board generator application. *)

open Final_project

(** Game interface for Settlers of Catan. *)

(** [print_introduction ()] prints the game introduction and rules to the
    console. This includes a welcome message, game description, and board
    information. *)
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

(** [get_user_input ()] waits for the user to press Enter to continue. This is
    used as a pause mechanism in the game flow. *)
let get_user_input () =
  print_endline "Press Enter to play...";
  let _ = read_line () in
  ()

(** [main_menu ()] displays the main menu and gets the user's choice. *)
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

(** [display_player_resources player] prints the resources for the given player.
*)
let display_player_resources player =
  print_endline "\nYour resources:";
  Player.print_hand player

(** [build_road_action player] handles the road building action for a player.
    Checks if the player has enough resources, deducts them if successful, and
    provides feedback. *)
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

(** [build_settlement_action board player player_idx] handles settlement
    building action. Validates resource availability, prompts for valid
    location, and handles placement with the 2-road distance rule. *)
let build_settlement_action board player player_idx =
  if
    Player.amt_resource "brick" player < 1
    || Player.amt_resource "wood" player < 1
    || Player.amt_resource "sheep" player < 1
    || Player.amt_resource "wheat" player < 1
  then begin
    print_endline "\nYou don't have enough resources to build a settlement.";
    print_endline "A settlement requires 1 brick, 1 wood, 1 sheep, and 1 wheat.";
    false
  end
  else begin
    print_endline "\nSelect a vertex to build a settlement:";

    let rec get_valid_place () =
      let place =
        try int_of_string (read_line ())
        with Failure _ ->
          print_endline "Please enter a valid number:";
          get_valid_place ()
      in

      (* Check if the vertex is valid *)
      if place < 0 || place >= Array.length (Board.places board) then begin
        print_endline "Invalid vertex number. Please choose a valid location:";
        get_valid_place ()
      end
      else place
    in

    let place = get_valid_place () in

    (* Deduct resources for settlement building *)
    let _ = Player.remove_resource "brick" player 1 in
    let _ = Player.remove_resource "wood" player 1 in
    let _ = Player.remove_resource "sheep" player 1 in
    let _ = Player.remove_resource "wheat" player 1 in

    (* Place the settlement - Board.place_settlement now returns a boolean *)
    let success = Board.place_settlement board player place player_idx false in

    if success then begin
      print_endline
        ("\nSettlement built successfully at vertex " ^ string_of_int place
       ^ "!");
      Board.print board;
      display_player_resources player;
      true
    end
    else begin
      (* If placement failed, refund the resources *)
      Player.increment_resource "brick" player;
      Player.increment_resource "wood" player;
      Player.increment_resource "sheep" player;
      Player.increment_resource "wheat" player;
      print_endline
        "\n\
         Cannot place settlement there! Settlements must be at least 2 roads \
         apart.";
      false
    end
  end

(** [trade_with_bank player] handles bank trading action at a 4:1 ratio.
    Validates resource availability and handles the exchange. *)
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

(** [color_of_player int] returns the string representation of the player's
    color. *)
let color_of_player int =
  match int with
  | 0 -> "RED"
  | 1 -> "BLUE"
  | 2 -> "YELLOW"
  | 3 -> "GREEN"
  | _ -> ""

(** [player_turn board player_idx players] handles the turn for a specific
    player. Rolls dice, distributes resources, and presents action options. *)
let player_turn board player_idx players =
  let player = List.nth players player_idx in

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
    print_endline
      "2. Build a settlement (costs 1 brick, 1 wood, 1 sheep, 1 wheat)";
    print_endline "3. Trade with the bank (4:1 ratio)";
    print_endline "4. View board";
    print_endline "5. End turn";
    print_endline "\nEnter your choice (1-5): ";

    match read_line () with
    | "1" ->
        let _ = build_road_action player in
        action_phase ()
    | "2" ->
        let _ = build_settlement_action board player player_idx in
        action_phase ()
    | "3" ->
        let _ = trade_with_bank player in
        action_phase ()
    | "4" ->
        Board.print board;
        action_phase ()
    | "5" ->
        print_endline
          ("\nPlayer " ^ string_of_int (player_idx + 1) ^ "'s turn ended.");
        ()
    | _ ->
        print_endline "Invalid choice. Please enter a number between 1 and 5.";
        action_phase ()
  in

  action_phase ()

(** [game_round board num_players current_round players] runs one round of the
    game. Each player takes their turn in order.*)
let game_round board num_players current_round players =
  print_endline ("\n\n====== ROUND " ^ string_of_int current_round ^ " ======");

  let rec process_player_turns player_idx players =
    if player_idx < num_players then begin
      player_turn board player_idx players;
      process_player_turns (player_idx + 1) players
    end
  in

  process_player_turns 0 players

(** [play_game board players] runs the main game loop with multiple rounds.
    Handles initial settlement placement and game round management. *)
let play_game board players =
  let num_players = List.length players in

  print_endline "\nPlacing initial settlements...";
  Game.initialize_game board players;

  let rec game_rounds round =
    game_round board num_players round players;

    let rec ask_continue () =
      print_endline "\nContinue to the next round? (y/n): ";
      match read_line () with
      | "y" | "Y" -> game_rounds (round + 1)
      | "n" | "N" -> print_endline "\nGame ended."
      | _ ->
          print_endline "Invalid input. Please enter 'y' for yes or 'n' for no.";
          ask_continue ()
    in

    ask_continue ()
  in

  game_rounds 1

(** [game_loop ()] runs the main game loop, handling menus and board display.
    This is the main entry point for running the game. *)
let rec game_loop () =
  print_introduction ();

  if main_menu () then begin
    print_endline "";
    let board = Board.create () in
    let players = Game.players () in
    Board.print board;
    play_game board players;

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

(** Entry point of the program. Starts the game loop with error handling. *)
let () =
  try game_loop () with
  | Failure msg ->
      print_endline ("\nError: " ^ msg);
      print_endline "Game terminated due to an error."
  | _ ->
      print_endline "\nAn unexpected error occurred.";
      print_endline "Game terminated."
