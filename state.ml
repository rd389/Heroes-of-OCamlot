type min_effect = Armor | Heal | Attack

type sp_effect = Heal | Dmg | Buff | Mana

type target = All | Me | Them | Mine | Theirs | Any

type minion = {
  attack : int;
  hp : int;
  bonus : (min_effect * int) list
}

type spell = {
  target : target;
  effect : sp_effect;
  mag : int
}

type weapon = {
  dmg : int;
  durability : int
}

type typ = Minion of minion | Spell of spell | Weapon of weapon

type card = {
  name : string;
  desc : string;
  cost : int;
  cat : typ
}

type hero = {
  hp : int;
  mana : int;
  weap : card option;
  armor : int;
  hand : card list;
  deck : card list;
  minions : card list;
}

type state = {
  turn : int;
  first_player : bool;
  players : hero * hero;
}

let print_card c =
  let typ = match c.cat with
  | Minion _ -> "Minion"
  | Spell _ -> "Spell"
  | Weapon _ -> "Weapon" in
  print_endline (typ ^ " : " ^ c.name ^ " - " ^ c.desc)

let rec print_card_list l =
  match l with
  | [] -> ()
  | h::t -> print_card h; print_card_list t

let print_state st =
  let player_num = if st.first_player then " 1" else " 2" in
  let plyr = if st.first_player
               then st.players |> fst
               else st.players |> snd in
  let other_plyr = if st.first_player
               then st.players |> snd
               else st.players |> fst in
  print_endline ("Player" ^ player_num ^ "'s turn.");
  print_endline "State of game:";
  print_endline "";
  print_endline "Your hand:";
  print_card_list plyr.hand;
  print_string "Your equipped weapon: ";
  match plyr.weap with
  | None -> print_endline "none";
  | Some w -> print_card w;
  print_string "Your minions in play: ";
  match plyr.minions with
  | [] -> print_endline "none";
  | ms -> print_endline ""; print_card_list ms;
  print_endline "";
  print_endline "Your opponent's hand:";
  print_card_list other_plyr.hand;
  print_string "Your opponent's equipped weapon: ";
  match other_plyr.weap with
  | None -> print_endline "none";
  | Some w -> print_card w;
  print_string "Your opponent's minions in play: ";
  match other_plyr.minions with
  | [] -> print_endline "none";
  | ms -> print_endline ""; print_card_list ms;
  print_endline ""; ()
