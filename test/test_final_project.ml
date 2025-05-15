(** Test suite for the Settlers of Catan board generator. *)

open OUnit2
open Final_project
open Board
open Player
(** [string_of_resource r] converts a resource to a string. *)
let string_of_resource = function
  | Board.Sheep -> "Sheep"
  | Board.Wood -> "Wood"
  | Board.Ore -> "Ore"
  | Board.Brick -> "Brick"
  | Board.Wheat -> "Wheat"
  | Board.Desert -> "Desert"

(** [string_of_tile (r, n)] converts a tile to a string. *)
let string_of_tile (r, n) = string_of_resource r ^ ", " ^ string_of_int n

(** [string_of_board b] converts a board to a string. *)
let string_of_board board =
  let tiles = Array.to_list board in
  String.concat "; " (List.map string_of_tile tiles)

  let roll_and_collect_resources_tests = [
    "rolling dice gives resources only to players on corresponding tiles" >:: fun _ ->
      (* Step 1: Create two players *)
      let player1 = Player.create () in
      let player2 = Player.create () in
  
      (* Step 2: Set up a fake board with tiles, and player lists *)
      let fake_board = [|
        {resource = Board.Sheep; num = 8; player = [player1]};
        {resource = Board.Wood; num = 5; player = [player2]};
        {resource = Board.Ore; num = 8; player = [player1; player2]};
        {resource = Board.Brick; num = 4; player = []};
        {resource = Board.Wheat; num = 10; player = []};
      |] in
  
      (* Step 3: Simulate a dice roll of 8 *)
      let roll = 8 in
  
      (* Step 4: Distribute resources to players on matching tiles *)
      Array.iter
        (fun tile ->
          if tile.num = roll then
            List.iter
              (fun player ->
                Player.increment_resource
                  (match tile.resource with
                   | Board.Sheep -> "sheep"
                   | Board.Wood -> "wood"
                   | Board.Ore -> "ore"
                   | Board.Brick -> "brick"
                   | Board.Wheat -> "wheat"
                   | Board.Desert -> "")
                  player)
              tile.player)
        fake_board;
  
      (* Step 5: Check player resource counts *)
      assert_equal 1 (Player.amt_resource "sheep" player1) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "wood" player1) ~printer:string_of_int;
      assert_equal 1 (Player.amt_resource "ore" player1) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "brick" player1) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "wheat" player1) ~printer:string_of_int;
  
      assert_equal 0 (Player.amt_resource "sheep" player2) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "wood" player2) ~printer:string_of_int;
      assert_equal 1 (Player.amt_resource "ore" player2) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "brick" player2) ~printer:string_of_int;
      assert_equal 0 (Player.amt_resource "wheat" player2) ~printer:string_of_int;
  ]
    

(** Test cases for the [Board.create] function. *)
let create_tests =
  [
    ( "create creates a board with 19 tiles" >:: fun _ ->
      let board = Board.create () in
      assert_equal 19 (Array.length board) ~printer:string_of_int );
    ( "create has exactly one Desert tile" >:: fun _ ->
      let board = Board.create () in
      let desert_count =
        Array.fold_left
          (fun count x -> if x.resource = Board.Desert then count + 1 else count)
          0 board
      in
      assert_equal 1 desert_count ~printer:string_of_int );
    ( "create places Desert at index 8 with number 0" >:: fun _ ->
      let board = Board.create () in
      let resource = board.(8).resource in
      let number = board.(8).num in
      assert_equal Board.Desert resource ~printer:string_of_resource;
      assert_equal 0 number ~printer:string_of_int );
    ( "create has the correct distribution of resources" >:: fun _ ->
      let board = Board.create () in
      let count_resource r =
        Array.fold_left
          (fun count x -> if x.resource = r then count + 1 else count)
          0 board
      in
      assert_equal 4
        (count_resource Board.Sheep)
        ~printer:string_of_int ~msg:"Sheep count";
      assert_equal 4
        (count_resource Board.Wood)
        ~printer:string_of_int ~msg:"Wood count";
      assert_equal 3 (count_resource Board.Ore) ~printer:string_of_int
        ~msg:"Ore count";
      assert_equal 3
        (count_resource Board.Brick)
        ~printer:string_of_int ~msg:"Brick count";
      assert_equal 4
        (count_resource Board.Wheat)
        ~printer:string_of_int ~msg:"Wheat count";
      assert_equal 1
        (count_resource Board.Desert)
        ~printer:string_of_int ~msg:"Desert count" );
    ( "create assigns proper number tokens" >:: fun _ ->
      let board = Board.create () in
      let number_counts = Array.make 13 0 in
      Array.iter
        (fun x ->
          if x.num > 0 && x.num < 13 then number_counts.(x.num) <- number_counts.(x.num) + 1)
        board;
      (* Check specific numbers that should appear exactly once *)
      assert_equal 1 number_counts.(2) ~printer:string_of_int
        ~msg:"Number 2 count";
      assert_equal 1 number_counts.(12) ~printer:string_of_int
        ~msg:"Number 12 count";
      (* Check other numbers that should appear exactly twice *)
      assert_equal 2 number_counts.(3) ~printer:string_of_int
        ~msg:"Number 3 count";
      assert_equal 2 number_counts.(4) ~printer:string_of_int
        ~msg:"Number 4 count";
      assert_equal 2 number_counts.(5) ~printer:string_of_int
        ~msg:"Number 5 count";
      assert_equal 2 number_counts.(6) ~printer:string_of_int
        ~msg:"Number 6 count";
      assert_equal 0 number_counts.(7) ~printer:string_of_int
        ~msg:"Number 7 count";
      (* No 7s in Catan *)
      assert_equal 2 number_counts.(8) ~printer:string_of_int
        ~msg:"Number 8 count";
      assert_equal 2 number_counts.(9) ~printer:string_of_int
        ~msg:"Number 9 count";
      assert_equal 2 number_counts.(10) ~printer:string_of_int
        ~msg:"Number 10 count";
      assert_equal 2 number_counts.(11) ~printer:string_of_int
        ~msg:"Number 11 count" );
  ]

(** Test cases for the [Board.print_tile] function. *)
let print_tile_tests =
  [
    ( "print_tile formats Sheep correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Sheep; num = 5; player = []} in
      assert_equal "Sheep  5" result ~printer:(fun s -> s) );
    ( "print_tile formats Wood correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Wood; num = 3; player = []} in
      assert_equal " Wood  3" result ~printer:(fun s -> s) );
    ( "print_tile formats Ore correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Ore; num = 8; player = []} in
      assert_equal "  Ore  8" result ~printer:(fun s -> s) );
    ( "print_tile formats Brick correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Brick; num = 4; player = []} in
      assert_equal "Brick  4" result ~printer:(fun s -> s) );
    ( "print_tile formats Wheat correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Wheat; num = 10; player = []} in
      assert_equal "Wheat 10" result ~printer:(fun s -> s) );
    ( "print_tile formats Desert correctly" >:: fun _ ->
      let result = Board.print_tile {resource = Board.Desert; num = 7; player = []} in
      assert_equal " Desert " result ~printer:(fun s -> s) );
  ]

(** All tests for the Board module. *)
let board_tests = List.flatten [ create_tests; print_tile_tests; roll_and_collect_resources_tests ]

(** Run all tests. *)
let tests = "test suite for Settlers of Catan" >::: board_tests

let _ = run_test_tt_main tests
