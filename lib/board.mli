(** Board representation for Settlers of Catan. This module handles the creation
    and representation of the Catan game board. *)

(** [resource] represents the different types of resources in the game. *)
type resource =
  | Sheep
  | Wood
  | Ore
  | Brick
  | Wheat
  | Desert

type player = Player.p

type tile = {
  resource : resource;
  num : int;
  mutable player : player list;
}
(** [tile] represents a single hexagonal tile on the board with a resource and
    number. *)

type board = tile array
(** [board] represents the entire Catan game board. *)

val create : unit -> board
(** [create ()] creates a new Catan board with the default resource and number
    distribution.
    @return A new board with resources and numbers assigned to each tile. *)

val print_tile : tile -> string
(** [print_tile t] converts a tile to a string representation.
    @param t The tile to convert
    @return A string representation of the tile *)

val print : board -> unit
(** [print board] prints a visual representation of the board to the console.
    @param board The board to print *)

val distribute_resources : tile array -> int list -> player -> unit
val place_village : tile array -> player -> int -> bool -> unit
val add_resource : resource -> player -> unit
