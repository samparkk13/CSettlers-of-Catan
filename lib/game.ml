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
  let rec get_valid_vertex () =
    try
      let place = read_int () in
      (* Check if the vertex is within valid range (0-53 based on your board) *)
      if place < 0 || place > 53 then begin
        print_endline
          "Invalid vertex number. Please choose a number between 0 and 53:";
        get_valid_vertex ()
      end
      else place
    with
    | Failure _ ->
        print_endline "Please enter a valid number:";
        get_valid_vertex ()
    | _ ->
        print_endline "Invalid input. Please enter a number:";
        get_valid_vertex ()
  in

  let rec place_village_with_retry board player player_idx distribute_resources
      =
    print_endline
      ("Player "
      ^ string_of_int (player_idx + 1)
      ^ ", choose where to place your village:");

    let place = get_valid_vertex () in

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
      print_endline "Enter where you want to place your road: (0-71)";
      let v = read_int () in
      Board.place_road player_idx v board players;
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
