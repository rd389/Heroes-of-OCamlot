open Game
open Players

let rec run_game () =
  print_string "> ";
  let game_mode = read_line () in
  match game_mode with
  | "1" -> Game.play (module HumanPlayer) (module HumanPlayer)
  | "2" -> Game.play (module HumanPlayer) (module AIPlayer)
  | "3" -> print_endline "Goodbye!"
  | _ -> print_endline "Try again."; run_game()

let () =
  print_endline "\n\nWelcome to the Heroes of OCamlot game engine.\n";
  print_endline "------------ Game Menu -------------";
  print_endline "1. PvP Game";
  print_endline "2. PvAI Game";
  print_endline "3. Quit";
  run_game ()