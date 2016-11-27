(* the different possible effects for a minion card *)
type min_effect = Armor | Heal | Attack
(* the different possible effects for a spell card *)
type sp_effect = Heal | Dmg | Buff | Mana
(* the different possible target(s) for a spell card *)
type target = All | Me | Them | Mine | Theirs | Any
(* the type of a card from the Minion class *)
type minion = {
  name : string;
  desc : string;
  attack : int ref;
  hp : int ref;
  bonus : (min_effect * int) list;
  cost : int
}
(* the type of a card from the Spell class *)
type spell = {
  name : string;
  desc : string;
  target : target;
  effect : sp_effect;
  mag : int;
  cost : int
}
(* the type of a card from the Weapon class*)
type weapon = {
  name : string;
  desc : string;
  dmg : int;
  armor : int;
  durability : int ref;
  cost : int
}
(* the different categories that a card can be *)
type typ = Minion of minion | Spell of spell | Weapon of weapon
(* the type of a card *)
type card = {
  name : string;
  cat : typ
}
(* the status of player's hero, which can be altered by minions, spells, or
  weapon cards *)
type hero = {
  hp : int ref;
  attack : int ref;
  armor : int ref;
  hand : card list;
  deck : card list;
  minions: minion list;
}
(* the game state, which can be altered by Player functions during each phase
  of each player's turn*)
type state = {
  turn : int;
  which_player : int ref;
  players : (hero ref) list;
}