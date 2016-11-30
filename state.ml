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

let rec bonuses l =
  match l with
  | [] -> ""
  | (eff,mag)::t -> let b = (match eff with
                            | Armor -> ("Ar +" ^ (string_of_int mag))
                            | HealM -> ("Hp +" ^ (string_of_int mag))
                            | Attack -> ("Att " ^ (string_of_int mag))) in
                    b ^ (bonuses t)

let print_card c =
  let typ = match c.cat with
  | Minion _ -> "Minion"
  | Spell _ -> "Spell"
  | Weapon _ -> "Weapon" in
  let cost = string_of_int c.cost in
  let desc = match c.cat with
             | Minion m -> ("HP " ^ (string_of_int m.hp) ^ ", Att " ^
                           (string_of_int m.attack) ^ ", " ^
                           "Bonuses: [" ^ (bonuses m.bonus) ^ "]")
             | Spell sp -> let targ = match sp.target with
                                      | All -> "All" | Me -> "Me"
                                      | Them -> "Them" | Mine -> "Mine"
                                      | Theirs -> "Theirs" | Any -> "Any" in
                           let eff = match sp.effect with
                                     | Heal -> "Heal" | Dmg -> "Dmg"
                                     | Mana -> "Mana" in
                           ("Target " ^ targ ^ ", " ^ eff ^ " " ^(string_of_int sp.mag))
             | Weapon wp -> ("Dmg " ^ (string_of_int wp.dmg) ^ ", " ^ "Dur " ^ (string_of_int wp.durability))
             in
  print_endline (typ ^ " : " ^ c.name ^ ", costs " ^ cost ^ " - " ^ desc)

let print_weap_notype wc =
  let cost = string_of_int wc.cost in
  let wp = match wc.cat with Weapon w -> w | _ -> failwith "Sum Ting Wong" in
  let desc = ("Dmg " ^ (string_of_int wp.dmg) ^ ", " ^ "Dur " ^ (string_of_int wp.durability)) in
  print_endline (wc.name ^ ", costs " ^ cost ^ " - " ^ desc)

let rec print_card_list l =
  match l with
  | [] -> ()
  | h::t -> print_card h; print_card_list t

let rec print_bonuses l =
  match l with
  | [] -> ()
  | (eff,mag)::t -> let () = (match eff with
                    | Armor -> print_endline ("    Ar +" ^ (string_of_int mag))
                    | HealM -> print_endline ("    Hp +" ^ (string_of_int mag))
                    | Attack -> print_endline ("    Att " ^ (string_of_int mag))) in
                    print_bonuses t


let rec print_minionlist l =
  match l with
  | [] -> ()
  | c::t -> let m = (match c.cat with | Minion min -> min
                                      | _ -> failwith "Sum Ting Wong") in
            print_endline (c.name ^ ":");
            print_endline ("  HP = " ^ (string_of_int m.hp));
            print_endline ("  Attack = " ^ (string_of_int m.attack));
            print_endline ("  Bonuses:");
            print_bonuses m.bonus


let print_state st =
  let plyr = if st.first_player
               then st.players |> fst
               else st.players |> snd in
  let other_plyr = if st.first_player
               then st.players |> snd
               else st.players |> fst in
  print_endline "State of game:";
  print_endline "";
  print_endline ("Your HP: " ^ (string_of_int plyr.hp));
  print_endline ("Your Mana: " ^ (string_of_int plyr.mana));
  print_endline ("Your Armor: " ^ (string_of_int plyr.armor));
  print_string "Your equipped weapon: ";
  let () = match plyr.weap with
           | None -> print_endline "none"
           | Some w -> print_weap_notype w in
  print_endline "Your hand:";
  if List.length plyr.hand = 0 then print_endline "none"
    else print_card_list plyr.hand;
  print_endline "Your minions in play: ";
  if List.length plyr.minions = 0 then print_endline "none"
    else print_minionlist plyr.minions;
  print_endline "\n";
  print_endline ("Oppenent's HP: " ^ (string_of_int other_plyr.hp));
  print_endline ("Oppenent's Armor: " ^ (string_of_int other_plyr.armor));
  print_string "Opponent's equipped weapon: ";
  let () = match other_plyr.weap with
           | None -> print_endline "none"
           | Some w -> print_weap_notype w in
  print_endline "Opponent's minions in play: ";
  if List.length other_plyr.minions = 0 then print_endline "none"
    else print_minionlist other_plyr.minions;
  print_endline ""; ()
