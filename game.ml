open Players
open State
open Yojson.Basic.Util
exception Unimplemented

(* [get_int k j] is the int associated with the value mapped to key [k] in
 * JSON object j. *)
let get_int k j =
  j |> member k |> to_int

(* get_string k j] is the string associated with the value mapped to key [k] in
 * JSON object j. *)
let get_string k j =
  j |> member k |> to_string

(* [get_list k j] is the list associated with the value mapped to key [k] in
 * JSON object j. *)
let get_list k j =
  j |> member k |> to_list

let build_bonus j =
  let eff = match get_string "effect" j with
            | "armor" -> Armor | "heal" -> Heal | "attack" -> Attack
            | _ -> failwith "Invalid minion bonus effect" in
  (eff, get_int "amount" j)

let build_minion j =
  let min = { attack =  (get_int "attack" j);
  hp =  (get_int "health" j);
  bonus = get_list "bonus" j |> List.map build_bonus;
  cost = get_int "cost" j;
  } in

  { name = get_string "name" j;
  desc = get_string "desc" j;
  cat = Minion min
  }

let build_spell j =
  let targ = match get_string "targ" j with
             | "all" -> All | "me" -> Me | "them" -> Them
             | "mine" -> Mine | "theirs" -> Theirs | "any" -> Any
             | _ -> failwith "Invalid target" in
  let eff = match get_string "effect" j with
            | "heal" -> Heal | "dmg" -> Dmg | "buff" -> Buff | "mana" -> Mana
            | _ -> failwith "Invalid spell effect" in
  let sp = { target = targ;
    effect = eff;
    mag = get_int "mag" j;
    cost = get_int "cost" j;
    } in

  { name = get_string "name" j;
  desc = get_string "desc" j;
  cat = Spell sp;
  }

let build_weapon j =
  let weapon = { dmg = get_int "dmg" j;
  durability =  (get_int "dur" j);
  cost = get_int "cost" j;
  } in

  {name = get_string "name" j;
  desc = get_string "desc" j;
  cat = Weapon weapon;
  }

let shuffle deck =
  let nd = List.map (fun c -> (Random.bits (), c)) deck in
  let sond = List.sort compare nd in
  List.map snd sond

let deck_builder filename =
  let j =
    try (Yojson.Basic.from_file filename) with
    | Yojson.Json_error _ -> (print_endline "File is not of proper JSON type";
      exit 0)
  in
  let minions = get_list "minions" j |> List.map build_minion in
  let spells = get_list "spells" j |> List.map build_spell in
  let weapons = get_list "weapons" j |> List.map build_weapon in
  let deck = minions@spells@weapons in
  shuffle deck

let play (module P1 : Player) (module P2 : Player) =
  let deck1 () =
    print_endline "Player 1: Please enter a deck file:";
    print_string "> ";
    read_line () |> deck_builder in
  let deck2 () =
    ( if P2.is_human then print_endline "Player 2: Please enter a deck file:"
      else "Please enter the AI's deck file:" );
    print_string "> ";
    read_line () |> deck_builder in
  let hero1 () =
    {hp = 30; attack = 0; armor = 0; hand = [];
      deck = deck1 (); minions = []} in
  let hero2 () =
    {hp = 30; attack = 0; armor = 0; hand = [];
      deck = deck2 (); minions = []} in
  let start_state () =
    {turn = 0; which_player = 0; (hero1 (), hero2 ())} in
  let rec play_game st =
    try (
      ( if st.first_player then
          P1.pre_phase |> P1.attack_phase |> P1.post_phase |> P1.end_turn
        else P2.pre_phase |> P2.attack_phase |> P2. post_phase |> P2. end_turn )
      |> play_game )
    with _ -> st
  let end_game st =
    if st.first_player then
      print_endline "Player 1 wins! \nThank you for playing! Goodbye."
    else print_endline "Player 2 wins! \n Thank you for playing! Goodbye."
  start_state () |> play_game |> end_game
