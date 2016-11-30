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
  (* [build_bonus j] is the minion effect associated with JSON object [j]. *)
  let build_bonus j =
    let eff = match get_string "effect" j with
              | "armor" -> Armor | "heal" -> HealM | "attack" -> Attack
              | _ -> failwith "Invalid minion bonus effect" in
    (eff, get_int "amount" j) in
  (* [build_minion j] is the minion associated with JSON object [j]. *)
  let build_minion j =
    let min = {attack = (get_int "attack" j); hp = (get_int "health" j);
                bonus = get_list "bonus" j |> List.map build_bonus; } in
    {name = get_string "name" j; desc = get_string "desc" j;
      cost = get_int "cost" j; cat = Minion min} in
  (* [build_spell j] is the spell associated with JSON object [j]. *)
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
  (* [build_weapon j] is the weapon associated with JSON object [j]. *)
  let build_weapon j =
    let weapon = {dmg = get_int "dmg" j; durability = (get_int "dur" j)} in
    {name = get_string "name" j; desc = get_string "desc" j;
      cost = get_int "cost" j; cat = Weapon weapon} in

  (* This function is based on the list-shuffling function on this StackOverflow
   * page: http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml
   * The purpose of this function is to shuffle the deck created from a JSON
   * file before gameplay begins. *)
  let shuffle deck =
    let nd = List.map (fun c -> (Random.bits (), c)) deck in
    let sond = List.sort compare nd in
    List.map snd sond in

  (* [get_file f] reads a JSON value from file [f], or prompts user to input a
   * file of the correct type if [f] is not a JSON file. *)
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

  (* [draw_card_noprompt plyr] draw's a card for player [plyr] without printing
   * the card that has been drawn; its purpose is to draw the starting hand for
   * a player. *)
  let draw_card_noprompt plyr =
    if (not (List.length plyr.deck = 0))
    then
      let card = List.hd plyr.deck in
      let new_deck = List.tl plyr.deck in
      let new_hand = card::plyr.hand in
      let new_plyr = {plyr with hand = new_hand; deck = new_deck} in
      new_plyr
    else plyr in

  (* [hero1 ()] and [hero2 ()] build the starting heros for players 1 and 2,
   * respectively, depending on whether the game mode is PvP or PvAI. *)
  let hero1 () =
    {hp = 30; mana = 1; weap = None; armor = 0; hand = [];
      deck = deck1; minions = []}
      |> draw_card_noprompt |> draw_card_noprompt |> draw_card_noprompt
    in
  let hero2 () =
    {hp = 30; mana = 1; weap = None; armor = 0; hand = [];
      deck = deck2; minions = []}
      |> draw_card_noprompt |> draw_card_noprompt |> draw_card_noprompt
    in

  (* [clear_terminal st] clears the terminal and returns the unchanged state
   * [st]. *)
  let clear_terminal st =
    let _ = Sys.command "clear" in st
    in

  (* [id st] Returns the state [st] without changing it after clearing the
   * terminal, after the user inputs a command in standard input. *)
  let id st = match read_line () with _ -> clear_terminal st in

  (* [start_state ()] Builds the starting state of the game. *)
  let start_state () =
    print_endline "Both players' decks have been shuffled, and starting hands have been drawn.\n";
    print_endline "Press Enter/Return.";
    id {turn = 1; first_player = true; players = (hero1 (), hero2 ())} in

  (* [finish_turn st] returns the state [st] at the end of a turn, prompting the
   * current user to pass the computer to the next player or to press Enter to
   * begin the AI's turn. *)
  let finish_turn st =
    if st.first_player then
      (if P1.is_human then (let _ = clear_terminal st in
                           print_endline "Pass computer to Player 1.";
                           print_endline "Player 1, press Enter/Return.";
                           id st)
      else (print_endline "Press Enter/Return to start AI's turn"; id st))
    else (if P2.is_human then (let _ = clear_terminal st in
                              print_endline "Pass computer to Player 2.";
                              print_endline "Player 2, press Enter/Return.";
                              id st)
         else (print_endline "Press Enter/Return to start AI's turn"; id st))
    in

  (* [play_game st] recursively plays the game from state [st] turn after turn
   * until an exception is raised, returning the state at the time when the
   * excpetion is raised. Its purpose is to wait for the GameOver exception,
   * indicating that a player has won, proceeding to end-game. *)
  let rec play_game st =
    try (
      ( if st.first_player then
          st |> P1.pre_phase |>
          P1.attack_phase |>
          P1.post_phase |> finish_turn |> clear_terminal
        else st |> P2.pre_phase |>
             P2.attack_phase |>
             P2.post_phase |> finish_turn |> clear_terminal
      )
      |> play_game )
    with _ -> st in

  (* [end_game st] ends the game in state st, and indicates which player has won.
   *)
  let end_game st =
    if st.first_player then
      print_endline "Player 1 wins! \nThank you for playing! Goodbye."
    else print_endline "Player 2 wins! \n Thank you for playing! Goodbye." in

  start_state () |> finish_turn |> play_game |> end_game