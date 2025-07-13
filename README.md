# CSettlers of Catan

**Authors:**  
Caleb Kim (ck586)  
Noah Lee (njl56)  
Samuel Park (sp994)

## Overview

CSettlers of Catan is a virtual implementation of the classic board game "Settlers of Catan," written in OCaml. The project features a board generator that creates a valid Catan game board with the correct distribution of resources and number tokens, and provides a playable game interface in the terminal.

## Features

- **Board Generation:**  
  Automatically generates a hexagonal board with 19 tiles, ensuring the correct distribution of resources (Sheep, Wood, Ore, Brick, Wheat, and one Desert) and number tokens (2–12, with proper frequency and placement rules).

- **Player Management:**  
  Supports up to 4 players, each with their own resource hand. Players can build settlements, cities, and roads, and trade with the bank.

- **Game Logic:**  
  Implements core Catan rules, including:
  - Rolling dice to produce resources
  - Enforcing settlement placement rules (e.g., 2-road distance)
  - Resource distribution based on dice rolls and adjacent settlements
  - Building and trading actions

- **Terminal Interface:**  
  Interactive command-line interface for playing the game, including menus, prompts, and board display.

- **Testing:**  
  Comprehensive OUnit test suite covering player actions, board generation, and game logic.

## File Structure

- `lib/board.ml` — Board representation, tile/resource logic, and placement rules.
- `lib/player.ml` — Player state, resource management, and player actions.
- `lib/game.ml` — Game setup, turn logic, dice rolling, and resource distribution.
- `bin/main.ml` — Main entry point, user interface, and game loop.
- `test/test_final_project.ml` — OUnit test suite for the project.
- `gallery.yaml` — Project metadata for the course gallery.

## How to Run

1. **Build the project:**  
   Use dune to build the project:
   ```zsh
   dune build
   ```

2. **Run the game:**  
   ```zsh
   dune exec bin/main.exe
   ```

3. **Run the tests:**  
   ```zsh
   dune test
   ```

## Rules & Gameplay

- Each player takes turns rolling dice, collecting resources, and building.
- Settlements and roads must follow Catan's placement rules.
- Players can trade with the bank at a 4:1 ratio.
- The game enforces the 2-road distance rule for settlements.

## Demo

A demo video is available at:  
https://www.youtube.com/watch?v=92pio3Kr0FA


