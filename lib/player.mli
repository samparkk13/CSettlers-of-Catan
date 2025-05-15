type hand = {
  mutable sheep : int;
  mutable wood : int;
  mutable ore : int;
  mutable brick : int;
  mutable wheat : int;
}

type p = hand

val create : unit -> p
(** [create ()] creates a new player hand with all resources initialized to 0.
*)

val increment_resource : string -> p -> unit
(** [increment_resource resource player] increments the specified [resource] in
    the player's hand by 1.

    Requires: [resource] is one of "sheep", "wood", "ore", "brick", "wheat". *)

val amt_resource : string -> p -> int
(** [amt_resource resource player] returns the amount of the specified
    [resource] in the player's hand.

    Requires: [resource] is one of "sheep", "wood", "ore", "brick", "wheat". *)

val print_hand : p -> unit
(** [print_hand player] prints the current counts of all resources in the
    player's hand. *)

val remove_resource : string -> p -> int -> bool
(** [remove_resource resource player amount] removes [amount] of the specified
    [resource] from the player's hand.

    Returns: [true] if the removal is successful, [false] if there are
    insufficient resources.

    Requires:
    - [resource] is one of "sheep", "wood", "ore", "brick", "wheat".
    - [amount] is a non-negative integer. *)
