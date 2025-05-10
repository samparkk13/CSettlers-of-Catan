let players =
  [ Player.create (); Player.create (); Player.create (); Player.create () ]

(* logic for rolling dice *)
let () = Random.self_init ()
let roll_x x = 1 + Random.int x

let initialize_game () =
  List.iteri (fun i player ->
      print_endline ("Player " ^ string_of_int (i + 1));
      print_endline "Choose where to place village:")

let initialize_game board =
  List.iteri
    (fun i player ->
      print_endline
        ("Player "
        ^ string_of_int (i + 1)
        ^ ", choose where to place your village:");
      let place = read_int () in
      Board.add_player_to_place board player place;
      print_endline ("Village placed at " ^ string_of_int place ^ ".");
      Board.place_village board player place;
      Board.print board;
      print_endline
        ("Resources from adjacent tiles distributed to Player "
        ^ string_of_int (i + 1)
        ^ "."))
    players;
  print_endline "All players have placed their initial villages."

let roll_dice board =
  let res = roll_x 6 + roll_x 6 in
  (* let res = 6 in *)
  Printf.printf "Rolled: %d\n" res;

  Array.iter
    (fun tile ->
      if tile.Board.num = res then
        List.iter
          (fun player -> Board.add_resource tile.Board.resource player)
          tile.Board.player)
    board
