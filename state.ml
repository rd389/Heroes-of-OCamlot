type min_effect = Armor | Heal | Attack

type sp_effect = Heal | Dmg | Buff | Mana

type target = All | Me | Them | Mine | Theirs | Any

type minion = {
  attack : int ref;
  hp : int ref;
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
  armor : int;
  durability : int ref;
  cost : int
}

type typ = Minion of minion | Spell of spell | Weapon of weapon

type card = {
  name : string;
  cat : typ
}

type hero = {
  hp : int ref;
  attack : int ref;
  armor : int ref;
  hand : card list;
  deck : card list;
  minions: minion list;
}

type state = {
  turn : int;
  which_player : int ref;
  players : (hero ref) list;
}