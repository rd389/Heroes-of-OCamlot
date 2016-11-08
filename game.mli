open Player

(* the different possible effects for a minion card *)
type min_effect = Armor | Heal | Attack
(* the different possible effects for a spell card *)
type sp_effect = Heal | Dmg | Buff | Mana
(* the different possible target(s) for a spell card *)
type target = All | Me | Them | Mine | Theirs | Any
(* the type of a card from the Minion class *)
type minion = {
  attack : int ref;
  hp : int ref;
  bonus : (min_effect * int) list
}
(* the type of a card from the Spell class *)
type spell = {
  target : target;
  effect : sp_effect;
  mag : int
}
(* the type of a card from the Weapon class*)
type weapon = {
  dmg : int;
  armor : int;
  durability : int ref;
}
(* the different classes that a card can be *)
type typ = Minion of minion | Spell of spell | Weapon of weapon
(* the type of a card *)
type card = {
  name : string;
  class : typ
}
(* the status of player's hero, which can be altered by minions, spells, or
  weapon cards *)
type hero = {
  hp : int ref;
  attack : int ref;
  armor : int ref
}
(* the game state, which can be altered by Player functions during each phase
  of each player's turn*)
type state = {
  turn : int;
  which_player : int ref;
  players : hero list;
  hands : (card list) list;
  deck : (card list) list;
  minions : (minion list) list
}
(* [deck_builder f] parses through the .json file at specified by f in order to
  build a deck (card list) from the information in the file *)
val deck_builder : string -> card list
(* [play p1 p2] is what actually runs the game, turn by turn for each Player,
  until one of the players quits or loses *)
val play : (module P1 : Player) -> (module P2 : Player) -> unit