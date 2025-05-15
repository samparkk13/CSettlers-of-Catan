(** Game interface for Settlers of Catan. This module handles the game interface
    and user interaction. *)

type player = Player.p
(** [player] is a type alias for a player representation in the game. *)

val players : player list
(** [players] is the list of players in the game. *)

val roll_dice : Board.board -> unit
(** [roll_dice board] simulates a dice roll and distributes resources based on
    the result. *)

val initialize_game : Board.board -> unit
(** [initialize_game board] sets up the game with the given board, initializing
    players and starting resources. *)
