type hand
type p

val create : unit -> p
val increment_resource : string -> p -> unit
val amt_resource : string -> p -> int
val print_hand : p -> unit
val remove_resource : string -> p -> int -> bool
