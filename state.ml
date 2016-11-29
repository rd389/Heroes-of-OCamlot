type min_effect = Armor | HealM | Attack

type sp_effect = Heal | Dmg | Mana

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
  let cost = string_of_int c.cost in
  print_endline (typ ^ " : " ^ c.name ^ ", costs " ^ cost ^ " - " ^ c.desc)

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
  print_endline ("Your HP: " ^ (string_of_int plyr.hp));
  print_endline ("Your Mana: " ^ (string_of_int plyr.mana));
  print_endline ("Your Armor: " ^ (string_of_int plyr.armor));
  print_string "Your equipped weapon: ";
  let () = match plyr.weap with
           | None -> print_endline "none"
           | Some w -> print_card w in
  print_endline "Your hand:";
  print_card_list plyr.hand;
  print_string "Your minions in play: ";
  let () = match plyr.minions with
           | [] -> print_endline "none"
           | ms -> print_endline ""; print_card_list ms in
  print_endline "\n";
  print_endline ("Oppenent's HP: " ^ (string_of_int other_plyr.hp));
  print_endline ("Oppenent's Armor: " ^ (string_of_int other_plyr.armor));
  print_string "Opponent's equipped weapon: ";
  let () = match other_plyr.weap with
           | None -> print_endline "none"
           | Some w -> print_card w in
  print_string "Opponent's minions in play: ";
  let () = match other_plyr.minions with
           | [] -> print_endline "none"
           | ms -> print_endline ""; print_card_list ms in
  print_endline ""; ()
