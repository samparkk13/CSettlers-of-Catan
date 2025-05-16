type player = Player.p
(** [player] is a type alias for a player representation in the game.

    AF: Same as Player.p - represents a player's state in the game.

    RI: Same as Player.p. *)

let players () =
  [ Player.create (); Player.create (); Player.create (); Player.create () ]

(* logic for rolling dice *)
let () = Random.self_init ()
let roll_x x = 1 + Random.int x

let initialize_game board players =
  let rec get_valid_number min max prompt =
    print_endline prompt;
    try
      let num = read_int () in
      if num < min || num > max then begin
        print_endline
          ("Invalid number. Please choose a number between " ^ string_of_int min
         ^ " and " ^ string_of_int max ^ ":");
        get_valid_number min max prompt
      end
      else num
    with
    | Failure _ ->
        print_endline "Please enter a valid number:";
        get_valid_number min max prompt
    | _ ->
        print_endline "Invalid input. Please enter a number:";
        get_valid_number min max prompt
  in

  let rec place_village_with_retry board player player_idx distribute_resources
      =
    print_endline
      ("Player "
      ^ string_of_int (player_idx + 1)
      ^ ", choose where to place your village:");

    let place = get_valid_number 0 53 "Enter a valid vertex number (0-53):" in

    let success =
      Board.place_settlement board player place player_idx distribute_resources
    in
    if success then begin
      print_endline ("Village placed at " ^ string_of_int place ^ ".");
      Board.print board;
      if distribute_resources then
        print_endline
          ("Resources from adjacent tiles distributed to Player "
          ^ string_of_int (player_idx + 1)
          ^ ".");
      let road_place =
        get_valid_number 0 71 "Enter where you want to place your road: (0-71)"
      in
      Board.place_road player_idx road_place board players;
      Board.print board
    end
    else begin
      print_endline
        "Cannot place settlement there! Settlements must be at least 2 roads \
         apart.";
      print_endline "Please try again.";
      place_village_with_retry board player player_idx distribute_resources
    end
  in

  (* First round - all players place their first settlement *)
  List.iteri
    (fun i player -> place_village_with_retry board player i false)
    players;

  (* Second round - all players place their second settlement in reverse
     order *)
  List.iteri
    (fun i player ->
      let player_idx = 3 - i in
      place_village_with_retry board player player_idx true)
    (List.rev players);

  print_endline "All players have placed their initial villages."

let roll_dice (board : Board.board) =
  let res = roll_x 6 + roll_x 6 in
  Printf.printf "Rolled: %d\n" res;

  Array.iter
    (fun tile ->
      if tile.Board.num = res then
        List.iter
          (fun player -> Board.add_resource tile.Board.resource player)
          tile.Board.player)
    (Board.tiles board)
