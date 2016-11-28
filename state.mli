(* the different possible effects for a minion card *)
type min_effect = Armor | Heal | Attack
(* the different possible effects for a spell card *)
type sp_effect = Heal | Dmg | Mana
(* the different possible target(s) for a spell card *)
type target = All | Me | Them | Mine | Theirs | Any
(* the type of a card from the Minion class *)
type minion = {
  attack : int;
  hp : int;
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
  durability : int;
}
(* the different categories that a card can be *)
type typ = Minion of minion | Spell of spell | Weapon of weapon
(* the type of a card *)
type card = {
  name : string;
  desc : string;
  cost : int;
  cat : typ
}
(* the status of player's hero, which can be altered by minions, spells, or
  weapon cards *)
type hero = {
  hp : int;
  mana : int;
  weap : card option;
  armor : int;
  hand : card list;
  deck : card list;
  minions : card list;
}
(* the game state, which can be altered by Player functions during each phase
  of each player's turn*)
type state = {
  turn : int;
  first_player : bool;
  players : hero * hero;
}

val print_card : card -> unit
val print_card_list : card list -> unit
val print_state : state -> unit
