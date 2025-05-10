(** Game interface for Settlers of Catan. This module handles the game interface
    and user interaction. *)

(* val print_introduction : unit -> unit (** [print_introduction ()] prints the
   introduction and rules of the game to the console. *)

   val get_user_input : unit -> unit (** [get_user_input ()] waits for the user
   to press Enter to continue. @return unit *)

   val main_menu : unit -> bool (** [main_menu ()] displays the main menu and
   gets the user's choice. @return true if the user chooses to play, false if
   they choose to quit *)

   val game_loop : unit -> unit (** [game_loop ()] runs the main game loop,
   handling menus and board display. This is the main entry point for running
   the game. *) *)

val roll_dice : Board.tile array -> unit
val initialize_game : Board.tile array -> unit
