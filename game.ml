open Players
open State
open Yojson.Basic.Util
exception Unimplemented

let deck_builder filename =

  (* [get_int k j] is the int associated with the value mapped to key [k] in
   * JSON object j. *)
  let get_int k j = j |> member k |> to_int in
  (* get_string k j] is the string associated with the value mapped to key [k]
   * in JSON object j. *)
  let get_string k j = j |> member k |> to_string in
  (* [get_list k j] is the list associated with the value mapped to key [k] in
   * JSON object j. *)
  let get_list k j = j |> member k |> to_list in
  let build_bonus j =
    let eff = match get_string "effect" j with
              | "armor" -> Armor | "heal" -> Heal | "attack" -> Attack
              | _ -> failwith "Invalid minion bonus effect" in
    (eff, get_int "amount" j) in
  let build_minion j =
    let min = {attack = (get_int "attack" j); hp = (get_int "health" j);
                bonus = get_list "bonus" j |> List.map build_bonus; } in
    {name = get_string "name" j; desc = get_string "desc" j;
      cost = get_int "cost" j; cat = Minion min} in
  let build_spell j =
    let targ = match get_string "targ" j with
               | "all" -> All | "me" -> Me | "them" -> Them
               | "mine" -> Mine | "theirs" -> Theirs | "any" -> Any
               | _ -> failwith "Invalid target" in
    let eff = match get_string "effect" j with
              | "heal" -> Heal | "dmg" -> Dmg | "mana" -> Mana
              | _ -> failwith "Invalid spell effect" in
    let sp = { target = targ; effect = eff; mag = get_int "mag" j} in
    {name = get_string "name" j;desc = get_string "desc" j;
      cost = get_int "cost" j; cat = Spell sp} in
  let build_weapon j =
    let weapon = {dmg = get_int "dmg" j; durability = (get_int "dur" j)} in
    {name = get_string "name" j; desc = get_string "desc" j;
      cost = get_int "cost" j; cat = Weapon weapon} in

  (* CITE DIS *)
  let shuffle deck =
    let nd = List.map (fun c -> (Random.bits (), c)) deck in
    let sond = List.sort compare nd in
    List.map snd sond in
  let rec get_file f =
    try (Yojson.Basic.from_file filename) with
    | _ -> (print_endline "Invalid file, try again."; print_string "> ";
      read_line () |> get_file) in
  let j = get_file filename in
  let minions = get_list "minions" j |> List.map build_minion in
  let spells = get_list "spells" j |> List.map build_spell in
  let weapons = get_list "weapons" j |> List.map build_weapon in
  let deck = minions@spells@weapons in
  shuffle deck

let play (module P1 : Player) (module P2 : Player) =
  let deck1 =
    print_endline "Player 1: Please enter a deck file:";
    print_string "> ";
    read_line () |> deck_builder in
  let deck2 =
    ( if P2.is_human then print_endline "Player 2: Please enter a deck file:"
      else print_endline "Please enter the AI's deck file:" );
    print_string "> ";
    read_line () |> deck_builder in
  let hero1 () =
    {hp = 30; mana = 0; weap = None; armor = 0; hand = [];
      deck = deck1; minions = []}
      |> P1.draw_card |> P1.draw_card |> P1.draw_card in
  let hero2 () =
    {hp = 30; mana = 0; weap = None; armor = 0; hand = [];
      deck = deck2; minions = []}
      |> P2.draw_card |> P2.draw_card |> P2.draw_card in
  let start_state () =
    {turn = 0; first_player = true; players = (hero1 (), hero2 ())} in
  let rec play_game st =
    try (
      ( if st.first_player then
          st |> P1.pre_phase |> P1.attack_phase |> P1.post_phase
        else st |> P2.pre_phase |> P2.attack_phase |> P2.post_phase )
      |> play_game )
    with _ -> st in
  let end_game st =
    if st.first_player then
      print_endline "Player 1 wins! \nThank you for playing! Goodbye."
    else print_endline "Player 2 wins! \n Thank you for playing! Goodbye." in
  start_state () |> play_game |> end_game