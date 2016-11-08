let run_game:unit =
  ANSITerminal.(print_string [red]
    "\n\nWelcome to the Heroes of OCamlot game engine.\n");
  print_endline "------------ Game Menu -------------";
  print_endline "1. PvP Game";
  print_endline "2. PvAI Game";
  print_endline "3. Quit";
  print_string "> ";
  let game_mode = read_line () in
  match game_mode with
  | "1" -> Game.play Player.HumanPlayer Player.HumanPlayer
  | "2" -> Game.play Player.HumanPlayer Player.AIPlayer
  | "3" -> print_endline "Goodbye!"
