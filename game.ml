open Player

type min_effect = Armor | Heal | Attack
type sp_effect = Heal | Dmg | Buff | Mana
type target = All | Me | Them | Mine | Theirs | Any
type minion = {
  attack : int ref;
  hp : int ref;
  bonus : (min_effect * int) list
}
type spell = {
  target : target;
  effect : sp_effect;
  mag : int
}
type weapon = {
  dmg : int;
  armor : int;
  durability : int ref
}
type typ = Minion of minion | Spell of spell | Weapon of weapon
type card = {
  name : string;
  class : typ
}
type hero = {
  hp : int ref;
  attack : int ref;
  armor : int ref
}
type state = {
  turn : int;
  which_player : int ref;
  players : hero list;
  hands : (card list) list;
  deck : (card list) list;
  minions : (minion list) list
}

let deck_builder filename =
  raise Unimplemented

let play p1 p2 =
  raise Unimplemented
