(** Board representation for Settlers of Catan. This module handles the creation
    and representation of the Catan game board. *)

open ANSITerminal

(** [resource] represents the different types of resources in the game. *)
type resource =
  | Sheep
  | Wood
  | Ore
  | Brick
  | Wheat
  | Desert

type player = Player.p
(** [player] is a type alias for a player representation in the game. *)

type tile = {
  resource : resource;
  num : int;
  mutable player : player list;
}
(** [tile] represents a single hexagonal tile on the board with a resource and
    number. *)

type board = tile array * (string * style) array * style array
(** [board] represents the entire Catan game board, consisting of:
    - an array of tiles
    - an array of places represented as a pair of string and style
    - an array of road styles *)

val tiles : board -> tile array
(** [tiles board] returns the array of tiles from the given board. *)

val places : board -> (string * style) array
(** [places board] returns the array of places from the given board. *)

val roads : board -> style array
(** [roads board] returns the array of road styles from the given board. *)

val create : unit -> board
(** [create ()] creates a new Catan board with the default resource and number
    distribution. *)

val print_tile : tile -> string
(** [print_tile t] converts a tile to a string representation. *)

val print : board -> unit
(** [print board] prints a visual representation of the board to the console. *)

val distribute_resources : board -> int list -> player -> unit
(** [distribute_resources board tile_indices player] distributes resources to
    the player based on the given tile indices that represent adjacent tiles. *)

val get_adjacent_vertices : int -> int list
(** [get_adjacent_vertices vertex] returns a list of vertices that are directly
    connected to the given vertex by a single road. *)

val is_vertex_occupied : board -> int -> bool
(** [is_vertex_occupied board vertex] checks if a given vertex already has a
    settlement. *)

val has_adjacent_settlement : board -> int -> bool
(** [has_adjacent_settlement board vertex] checks if any vertex adjacent to the
    given vertex has a settlement or city. *)

val can_place_settlement : board -> int -> bool
(** [can_place_settlement board vertex] checks if a settlement can be placed at
    the given vertex according to game rules: 1. The vertex must be empty 2. No
    adjacent vertex can have a settlement (2-road distance rule) *)

val place_settlement : board -> player -> int -> int -> bool -> bool
(** [place_settlement board player place player_id initial] places a settlement
    for the player at the given place. If initial is true, it also distributes
    initial resources. *)

val place_road : int -> int -> board -> player list -> unit
(** [place_road player place] places a road for the player at the given place.
*)

val add_resource : resource -> player -> unit
(** [add_resource resource player] increments the given resource count for the
    player. *)

val color_of_player : int -> style
(** [color_of_player id] returns the terminal color style associated with the
    given player id. *)
