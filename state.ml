type min_effect = Armor | Heal | Attack

type sp_effect = Heal | Dmg | Buff | Mana

type target = All | Me | Them | Mine | Theirs | Any

type minion = {
  mutable attack : int;
  mutable hp : int;
  bonus : (min_effect * int) list;
  cost : int
}

type spell = {
  target : target;
  effect : sp_effect;
  mag : int;
  cost : int
}

type weapon = {
  dmg : int;
  mutable durability : int;
  cost : int
}

type typ = Minion of minion | Spell of spell | Weapon of weapon

type card = {
  name : string;
  desc : string;
  cat : typ
}

type hero = {
  mutable hp : int;
  mutable attack : int;
  mutable armor : int;
  hand : card list;
  deck : card list;
  in_play : card list;
  minions: minion list;
}

type state = {
  turn : int;
  mutable which_player : int;
  players : hero list;
}