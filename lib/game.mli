(** Game interface for Settlers of Catan. This module handles the game interface
    and user interaction. *)

(** Game interface for Settlers of Catan. *)

type player = Player.p

val players : player list
(** [players] is the list of players in the game *)

val roll_dice : Board.tile array -> unit
val initialize_game : Board.tile array -> unit
