(* the different possible effects for a minion card *)
type min_effect = Armor | Heal | Attack
(* the different possible effects for a spell card *)
type sp_effect = Heal | Dmg | Buff | Mana
(* the different possible target(s) for a spell card *)
type target = All | Me | Them | Mine | Theirs | Any
(* the type of a card from the Minion class *)
type minion = {
  mutable attack : int;
  mutable hp : int;
  bonus : (min_effect * int) list;
  cost : int
}
(* the type of a card from the Spell class *)
type spell = {
  target : target;
  effect : sp_effect;
  mag : int;
  cost : int
}
(* the type of a card from the Weapon class*)
type weapon = {
  dmg : int;
  mutable durability : int;
  cost : int
}
(* the different categories that a card can be *)
type typ = Minion of minion | Spell of spell | Weapon of weapon
(* the type of a card *)
type card = {
  name : string;
  desc : string;
  cat : typ
}
(* the status of player's hero, which can be altered by minions, spells, or
  weapon cards *)
type hero = {
  mutable hp : int;
  mutable attack : int;
  mutable armor : int;
  hand : card list;
  deck : card list;
  in_play : card list;
  minions: minion list;
}
(* the game state, which can be altered by Player functions during each phase
  of each player's turn*)
type state = {
  turn : int;
  mutable which_player : int;
  players : hero list;
}