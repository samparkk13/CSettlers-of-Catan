type hand = {
  mutable sheep : int;
  mutable wood : int;
  mutable ore : int;
  mutable brick : int;
  mutable wheat : int;
}

type p = hand

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
