type hand = {
  mutable sheep : int;
  mutable wood : int;
  mutable ore : int;
  mutable brick : int;
  mutable wheat : int;
}
(** [hand] represents a player's collection of resources in Settlers of Catan.
    
    AF: A hand {sheep; wood; ore; brick; wheat} represents a player's resources
    where sheep is the number of sheep cards, wood is the number of wood cards,
    ore is the number of ore cards, brick is the number of brick cards, and
    wheat is the number of wheat cards the player holds.
    
    RI: - All resource counts must be non-negative (>= 0)
        - The implementation enforces this through increment_resource (adds 1)
          and remove_resource (checks for sufficient resources before removing) *)

type p = hand
(** [p] represents a player in the game.

    AF: A value of type p represents a player's state in the game, which
    currently consists only of their hand of resources.

    RI: Same as the RI for hand. *)

let create () : p = { sheep = 0; wood = 0; ore = 0; brick = 0; wheat = 0 }

let increment_resource resource player =
  match resource with
  | "sheep" -> player.sheep <- player.sheep + 1
  | "wood" -> player.wood <- player.wood + 1
  | "ore" -> player.ore <- player.ore + 1
  | "brick" -> player.brick <- player.brick + 1
  | "wheat" -> player.wheat <- player.wheat + 1
  | _ -> ()

let amt_resource resource player =
  match resource with
  | "sheep" -> player.sheep
  | "wood" -> player.wood
  | "ore" -> player.ore
  | "brick" -> player.brick
  | "wheat" -> player.wheat
  | _ -> 0

let print_hand player =
  Printf.printf "Sheep: %d\n" player.sheep;
  Printf.printf "Wood: %d\n" player.wood;
  Printf.printf "Ore: %d\n" player.ore;
  Printf.printf "Brick: %d\n" player.brick;
  Printf.printf "Wheat: %d\n" player.wheat

let remove_resource res_type player amount =
  match res_type with
  | "wood" ->
      if player.wood >= amount then begin
        player.wood <- player.wood - amount;
        true
      end
      else false
  | "sheep" ->
      if player.sheep >= amount then begin
        player.sheep <- player.sheep - amount;
        true
      end
      else false
  | "ore" ->
      if player.ore >= amount then begin
        player.ore <- player.ore - amount;
        true
      end
      else false
  | "brick" ->
      if player.brick >= amount then begin
        player.brick <- player.brick - amount;
        true
      end
      else false
  | "wheat" ->
      if player.wheat >= amount then begin
        player.wheat <- player.wheat - amount;
        true
      end
      else false
  | _ -> false
