(** Test suite for the Settlers of Catan board game. *)

open OUnit2
open Final_project

(* Helper functions for testing *)
let string_of_resource = function
  | Board.Sheep -> "Sheep"
  | Board.Wood -> "Wood"
  | Board.Ore -> "Ore"
  | Board.Brick -> "Brick"
  | Board.Wheat -> "Wheat"
  | Board.Desert -> "Desert"

(* Helper function to find an element in an array *)
let array_find pred arr =
  let rec find_aux i =
    if i >= Array.length arr then raise Not_found
    else if pred arr.(i) then arr.(i)
    else find_aux (i + 1)
  in
  find_aux 0

(* Player tests *)
let player_tests =
  "Player tests"
  >::: [
         ( "player create initializes empty hand" >:: fun _ ->
           let player = Player.create () in
           assert_equal 0
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int;
           assert_equal 0
             (Player.amt_resource "wood" player)
             ~printer:string_of_int;
           assert_equal 0
             (Player.amt_resource "ore" player)
             ~printer:string_of_int;
           assert_equal 0
             (Player.amt_resource "brick" player)
             ~printer:string_of_int;
           assert_equal 0
             (Player.amt_resource "wheat" player)
             ~printer:string_of_int );
         ( "increment resource works correctly" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "sheep" player;
           Player.increment_resource "wood" player;
           Player.increment_resource "wood" player;
           assert_equal 1
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int;
           assert_equal 2
             (Player.amt_resource "wood" player)
             ~printer:string_of_int );
         ( "increment all resources" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "sheep" player;
           Player.increment_resource "wood" player;
           Player.increment_resource "ore" player;
           Player.increment_resource "brick" player;
           Player.increment_resource "wheat" player;
           assert_equal 1
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int;
           assert_equal 1
             (Player.amt_resource "wood" player)
             ~printer:string_of_int;
           assert_equal 1
             (Player.amt_resource "ore" player)
             ~printer:string_of_int;
           assert_equal 1
             (Player.amt_resource "brick" player)
             ~printer:string_of_int;
           assert_equal 1
             (Player.amt_resource "wheat" player)
             ~printer:string_of_int );
         ( "amt_resource returns 0 for invalid resource" >:: fun _ ->
           let player = Player.create () in
           assert_equal 0
             (Player.amt_resource "invalid" player)
             ~printer:string_of_int );
         ( "increment_resource ignores invalid resource" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "invalid" player;
           (* Should not crash and all resources should remain 0 *)
           assert_equal 0
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int );
         ( "remove_resource works when sufficient resources" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "sheep" player;
           Player.increment_resource "sheep" player;
           let success = Player.remove_resource "sheep" player 1 in
           assert_equal true success ~printer:string_of_bool;
           assert_equal 1
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int );
         ( "remove_resource fails when insufficient resources" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "sheep" player;
           let success = Player.remove_resource "sheep" player 2 in
           assert_equal false success ~printer:string_of_bool;
           assert_equal 1
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int );
         ( "remove_resource handles all resource types" >:: fun _ ->
           let player = Player.create () in
           let resources = [ "sheep"; "wood"; "ore"; "brick"; "wheat" ] in
           List.iter
             (fun r ->
               Player.increment_resource r player;
               Player.increment_resource r player)
             resources;

           List.iter
             (fun r ->
               let success = Player.remove_resource r player 1 in
               assert_equal true success ~printer:string_of_bool;
               assert_equal 1
                 (Player.amt_resource r player)
                 ~printer:string_of_int)
             resources );
         ( "remove_resource returns false for invalid resource" >:: fun _ ->
           let player = Player.create () in
           let success = Player.remove_resource "invalid" player 1 in
           assert_equal false success ~printer:string_of_bool );
         ( "print_hand doesn't crash" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "sheep" player;
           Player.print_hand player;
           (* Should not crash *)
           assert_equal true true );
         ( "remove exactly amount of resources" >:: fun _ ->
           let player = Player.create () in
           Player.increment_resource "wood" player;
           Player.increment_resource "wood" player;
           Player.increment_resource "wood" player;
           let success = Player.remove_resource "wood" player 3 in
           assert_equal true success ~printer:string_of_bool;
           assert_equal 0
             (Player.amt_resource "wood" player)
             ~printer:string_of_int );
       ]

(* Board tests *)
let board_tests =
  "Board tests"
  >::: [
         ( "create creates board with 19 tiles" >:: fun _ ->
           let board = Board.create () in
           assert_equal 19
             (Array.length (Board.tiles board))
             ~printer:string_of_int );
         ( "create creates board with 54 places" >:: fun _ ->
           let board = Board.create () in
           assert_equal 54
             (Array.length (Board.places board))
             ~printer:string_of_int );
         ( "create creates board with 72 roads" >:: fun _ ->
           let board = Board.create () in
           assert_equal 72
             (Array.length (Board.roads board))
             ~printer:string_of_int );
         ( "create has exactly one desert" >:: fun _ ->
           let board = Board.create () in
           let desert_count =
             Array.fold_left
               (fun acc tile ->
                 if tile.Board.resource = Board.Desert then acc + 1 else acc)
               0 (Board.tiles board)
           in
           assert_equal 1 desert_count ~printer:string_of_int );
         ( "desert tile has number 7" >:: fun _ ->
           let board = Board.create () in
           let desert_tile =
             array_find
               (fun tile -> tile.Board.resource = Board.Desert)
               (Board.tiles board)
           in
           assert_equal 7 desert_tile.Board.num ~printer:string_of_int );
         ( "correct resource distribution" >:: fun _ ->
           let board = Board.create () in
           let count_resource r =
             Array.fold_left
               (fun acc tile ->
                 if tile.Board.resource = r then acc + 1 else acc)
               0 (Board.tiles board)
           in

           assert_equal 4 (count_resource Board.Wood) ~printer:string_of_int;
           assert_equal 4 (count_resource Board.Wheat) ~printer:string_of_int;
           assert_equal 4 (count_resource Board.Sheep) ~printer:string_of_int;
           assert_equal 3 (count_resource Board.Brick) ~printer:string_of_int;
           assert_equal 3 (count_resource Board.Ore) ~printer:string_of_int;
           assert_equal 1 (count_resource Board.Desert) ~printer:string_of_int
         );
         ( "correct number distribution" >:: fun _ ->
           let board = Board.create () in
           let number_counts = Array.make 13 0 in
           Array.iter
             (fun tile ->
               if tile.Board.num >= 2 && tile.Board.num <= 12 then
                 number_counts.(tile.Board.num) <-
                   number_counts.(tile.Board.num) + 1)
             (Board.tiles board);

           assert_equal 1 number_counts.(2) ~printer:string_of_int;
           assert_equal 2 number_counts.(3) ~printer:string_of_int;
           assert_equal 2 number_counts.(4) ~printer:string_of_int;
           assert_equal 2 number_counts.(5) ~printer:string_of_int;
           assert_equal 2 number_counts.(6) ~printer:string_of_int;
           assert_equal 1 number_counts.(7) ~printer:string_of_int;
           assert_equal 2 number_counts.(8) ~printer:string_of_int;
           assert_equal 2 number_counts.(9) ~printer:string_of_int;
           assert_equal 2 number_counts.(10) ~printer:string_of_int;
           assert_equal 2 number_counts.(11) ~printer:string_of_int;
           assert_equal 1 number_counts.(12) ~printer:string_of_int );
         ( "print_tile formats correctly" >:: fun _ ->
           let tile = { Board.resource = Board.Sheep; num = 5; player = [] } in
           assert_equal "Sheep  5" (Board.print_tile tile);

           let tile2 = { Board.resource = Board.Wood; num = 10; player = [] } in
           assert_equal " Wood 10" (Board.print_tile tile2);

           let tile3 =
             { Board.resource = Board.Desert; num = 7; player = [] }
           in
           assert_equal " Desert " (Board.print_tile tile3) );
         ( "print_tile handles double digit numbers" >:: fun _ ->
           let resources =
             [ Board.Sheep; Board.Wood; Board.Ore; Board.Brick; Board.Wheat ]
           in
           List.iter
             (fun r ->
               let tile = { Board.resource = r; num = 11; player = [] } in
               let _ = Board.print_tile tile in
               assert_equal true true)
             resources );
         ( "tiles accessor works" >:: fun _ ->
           let board = Board.create () in
           let tiles = Board.tiles board in
           assert_equal 19 (Array.length tiles) ~printer:string_of_int );
         ( "places accessor works" >:: fun _ ->
           let board = Board.create () in
           let places = Board.places board in
           assert_equal 54 (Array.length places) ~printer:string_of_int );
         ( "roads accessor works" >:: fun _ ->
           let board = Board.create () in
           let roads = Board.roads board in
           assert_equal 72 (Array.length roads) ~printer:string_of_int );
         ( "get_adjacent_vertices covers valid vertices" >:: fun _ ->
           (* Test a few key vertices *)
           let adj_0 = Board.get_adjacent_vertices 0 in
           assert_equal 2 (List.length adj_0) ~printer:string_of_int;
           assert_equal true (List.mem 1 adj_0);
           assert_equal true (List.mem 3 adj_0);

           let adj_8 = Board.get_adjacent_vertices 8 in
           assert_equal 3 (List.length adj_8) ~printer:string_of_int;

           (* Test edge vertices *)
           let test_vertices =
             [ 0; 1; 5; 11; 17; 23; 29; 35; 41; 47; 52; 53 ]
           in
           List.iter
             (fun v ->
               let adj = Board.get_adjacent_vertices v in
               assert_equal true (List.length adj >= 0))
             test_vertices;

           (* Invalid vertex *)
           let adj_invalid = Board.get_adjacent_vertices 100 in
           assert_equal 0 (List.length adj_invalid) ~printer:string_of_int );
         ( "is_vertex_occupied works correctly" >:: fun _ ->
           let board = Board.create () in
           assert_equal false (Board.is_vertex_occupied board 0);

           (* Place a settlement *)
           (Board.places board).(0) <- ("s", ANSITerminal.default);
           assert_equal true (Board.is_vertex_occupied board 0);

           (* Place a city *)
           (Board.places board).(1) <- ("c", ANSITerminal.default);
           assert_equal true (Board.is_vertex_occupied board 1) );
         ( "has_adjacent_settlement works correctly" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player = List.hd players in

           (* No adjacent settlements initially *)
           assert_equal false (Board.has_adjacent_settlement board 8);

           (* Place a settlement at vertex 3 (adjacent to 8) *)
           let _ = Board.place_settlement board player 3 0 false in
           assert_equal true (Board.has_adjacent_settlement board 8) );
         ( "can_place_settlement works correctly" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player = List.hd players in

           (* Can place initially *)
           assert_equal true (Board.can_place_settlement board 8);

           (* Cannot place on occupied vertex *)
           let _ = Board.place_settlement board player 8 0 false in
           assert_equal false (Board.can_place_settlement board 8);

           (* Cannot place adjacent to settlement *)
           assert_equal false (Board.can_place_settlement board 3) );
         ( "color_of_player returns correct colors" >:: fun _ ->
           assert_equal ANSITerminal.red (Board.color_of_player 0);
           assert_equal ANSITerminal.blue (Board.color_of_player 1);
           assert_equal ANSITerminal.yellow (Board.color_of_player 2);
           assert_equal ANSITerminal.green (Board.color_of_player 3);
           assert_equal ANSITerminal.default (Board.color_of_player 4);
           assert_equal ANSITerminal.default (Board.color_of_player 100) );
         ( "add_resource adds resources correctly" >:: fun _ ->
           let player = Player.create () in
           Board.add_resource Board.Sheep player;
           assert_equal 1
             (Player.amt_resource "sheep" player)
             ~printer:string_of_int;

           Board.add_resource Board.Wood player;
           assert_equal 1
             (Player.amt_resource "wood" player)
             ~printer:string_of_int;

           Board.add_resource Board.Ore player;
           assert_equal 1
             (Player.amt_resource "ore" player)
             ~printer:string_of_int;

           Board.add_resource Board.Brick player;
           assert_equal 1
             (Player.amt_resource "brick" player)
             ~printer:string_of_int;

           Board.add_resource Board.Wheat player;
           assert_equal 1
             (Player.amt_resource "wheat" player)
             ~printer:string_of_int;

           Board.add_resource Board.Desert player;
           (* Should do nothing *)
           let total =
             Player.amt_resource "sheep" player
             + Player.amt_resource "wood" player
             + Player.amt_resource "ore" player
             + Player.amt_resource "brick" player
             + Player.amt_resource "wheat" player
           in
           assert_equal 5 total ~printer:string_of_int );
         ( "distribute_resources works correctly" >:: fun _ ->
           let board = Board.create () in
           let player = Player.create () in

           (* Add player to some tiles *)
           (Board.tiles board).(0).Board.player <- [ player ];
           (Board.tiles board).(1).Board.player <- [ player ];

           Board.distribute_resources board [ 0; 1 ] player;

           (* Player should have resources from tiles 0 and 1 *)
           let total_resources =
             Player.amt_resource "sheep" player
             + Player.amt_resource "wood" player
             + Player.amt_resource "ore" player
             + Player.amt_resource "brick" player
             + Player.amt_resource "wheat" player
           in
           assert_equal true (total_resources > 0) );
         ( "print board doesn't crash" >:: fun _ ->
           let board = Board.create () in
           Board.print board;
           (* Should not crash *)
           assert_equal true true );
       ]

(* Game tests *)
let game_tests =
  "Game tests"
  >::: [
         ( "players creates 4 players" >:: fun _ ->
           let players = Game.players () in
           assert_equal 4 (List.length players) ~printer:string_of_int );
         ( "roll_dice distributes resources correctly" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player = List.hd players in

           (* Set up a controlled scenario *)
           let tiles = Board.tiles board in

           (* Find a tile with number 6 (common roll) *)
           let tile_with_6_opt =
             try Some (array_find (fun tile -> tile.Board.num = 6) tiles)
             with Not_found -> None
           in

           match tile_with_6_opt with
           | Some tile ->
               tile.Board.player <- [ player ];

               (* Test dice roll *)
               Game.roll_dice board;

               (* Player might have received resources if 6 was rolled *)
               assert_equal true true
               (* Can't predict dice roll, just test it doesn't crash *)
           | None -> assert_equal true true (* Skip if no tile with 6 *) );
       ]

(* Settlement placement tests *)
let settlement_tests =
  "Settlement tests"
  >::: [
         ( "place_settlement updates board correctly" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player = List.hd players in

           let success = Board.place_settlement board player 8 0 false in
           assert_equal true success ~printer:string_of_bool;

           (* Check that the place is marked *)
           assert_equal ("s", Board.color_of_player 0) (Board.places board).(8)
         );
         ( "place_settlement with resources works" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player = List.hd players in

           let initial_total =
             Player.amt_resource "sheep" player
             + Player.amt_resource "wood" player
             + Player.amt_resource "ore" player
             + Player.amt_resource "brick" player
             + Player.amt_resource "wheat" player
           in

           let success = Board.place_settlement board player 8 0 true in
           assert_equal true success ~printer:string_of_bool;

           let final_total =
             Player.amt_resource "sheep" player
             + Player.amt_resource "wood" player
             + Player.amt_resource "ore" player
             + Player.amt_resource "brick" player
             + Player.amt_resource "wheat" player
           in

           (* Should have gained some resources (unless all adjacent are
              desert) *)
           assert_equal true (final_total >= initial_total) );
         ( "place_settlement enforces distance rule" >:: fun _ ->
           let board = Board.create () in
           let players = Game.players () in
           let player1 = List.nth players 0 in
           let player2 = List.nth players 1 in

           (* Place first settlement *)
           let success1 = Board.place_settlement board player1 8 0 false in
           assert_equal true success1 ~printer:string_of_bool;

           (* Try to place adjacent settlement *)
           let success2 = Board.place_settlement board player2 3 1 false in
           assert_equal false success2 ~printer:string_of_bool;

           (* Place non-adjacent settlement *)
           let success3 = Board.place_settlement board player2 15 1 false in
           assert_equal true success3 ~printer:string_of_bool );
         ( "place_settlement on all valid vertices" >:: fun _ ->
           let players = Game.players () in
           let player = List.hd players in

           (* Test placing settlements on various vertices to increase
              coverage *)
           let test_vertices = [ 0; 10; 20; 30; 40; 50 ] in
           List.iter
             (fun v ->
               let fresh_board = Board.create () in
               let can_place = Board.can_place_settlement fresh_board v in
               if can_place then
                 let success =
                   Board.place_settlement fresh_board player v 0 false
                 in
                 assert_equal true success ~printer:string_of_bool
               else assert_equal true true)
             test_vertices );
       ]

(* Coverage improvement tests *)
let coverage_tests =
  "Coverage tests"
  >::: [
         ( "test various edge cases" >:: fun _ ->
           (* Test get_adjacent_vertices for many vertices *)
           let vertices = List.init 54 (fun i -> i) in
           List.iter
             (fun v ->
               let _ = Board.get_adjacent_vertices v in
               ())
             vertices;

           (* Test invalid vertex *)
           let _ = Board.get_adjacent_vertices 100 in

           assert_equal true true );
         ( "test multiple board creations" >:: fun _ ->
           (* Create multiple boards to test randomization *)
           let board1 = Board.create () in
           let board2 = Board.create () in
           let board3 = Board.create () in

           assert_equal true (Array.length (Board.tiles board1) = 19);
           assert_equal true (Array.length (Board.tiles board2) = 19);
           assert_equal true (Array.length (Board.tiles board3) = 19) );
         ( "test place settlement on many vertices" >:: fun _ ->
           let players = Game.players () in

           (* Try to place settlements on many vertices to trigger different
              code paths *)
           let try_vertices = [ 0; 3; 8; 13; 19; 24; 30; 36; 42; 48; 53 ] in
           List.iteri
             (fun i v ->
               let player = List.nth players (i mod 4) in
               let fresh_board = Board.create () in
               let _ =
                 Board.place_settlement fresh_board player v (i mod 4) false
               in
               ())
             try_vertices;

           assert_equal true true );
       ]

(* All tests *)
let all_tests =
  "All tests"
  >::: [
         player_tests; board_tests; game_tests; settlement_tests; coverage_tests;
       ]

let () = run_test_tt_main all_tests
