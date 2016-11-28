open Players
open State
exception Unimplemented

(* [deck_builder f] parses through the .json file specified by f in order to
  build a deck (card list) from the information in the file *)
val deck_builder : string -> card list
(* [play p1 p2] is what actually runs the game, turn by turn for each Player,
  until one of the players quits or loses *)
val play : (module Players.Player) -> (module Players.Player) -> unit