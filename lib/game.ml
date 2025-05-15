type player = Player.p

let players =
  [ Player.create (); Player.create (); Player.create (); Player.create () ]

(* logic for rolling dice *)
let () = Random.self_init ()
let roll_x x = 1 + Random.int x

let initialize_game () =
  List.iteri (fun i player ->
      print_endline ("Player " ^ string_of_int (i + 1));
      print_endline "Choose where to place settlement:")

let initialize_game board =
  let rec place_settlement player i reverse =
    let player_num = if reverse then 4 - i else i + 1 in
    print_endline
      ("Player " ^ string_of_int player_num
     ^ ", choose a settlement location (0-53):");
    let place = read_int () in
    if place < 0 || place > 53 then (
      print_endline "Invalid location. Please choose a number between 0 and 53.";
      place_settlement player i reverse)
    else (
      Board.place_settlement board player place
        (if reverse then 3 - i else i)
        reverse;
      print_endline ("Settlement placed at " ^ string_of_int place ^ ".");
      Board.print board;
      print_endline
        ("Resources from adjacent tiles distributed to Player "
       ^ string_of_int player_num ^ "."))
  in
  List.iteri (fun i player -> place_settlement player i false) players;
  List.iteri (fun i player -> place_settlement player i true) (List.rev players);
  print_endline "All players have placed their initial settlements."

let roll_dice board =
  let res = roll_x 6 + roll_x 6 in
  Printf.printf "Rolled: %d\n" res;

  Array.iter
    (fun tile ->
      if tile.Board.num = res then
        List.iter
          (fun player -> Board.add_resource tile.Board.resource player)
          tile.Board.player)
    board
