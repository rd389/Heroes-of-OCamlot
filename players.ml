open State

exception Unimplemented
exception GameOver

module type Player = sig
  val is_human : bool
  val draw_card : hero -> hero
  val start_turn : state -> state
  val pre_phase : state -> state
  val attack_phase : state -> state
  val post_phase : state -> state
  val end_turn : state -> state
end

module HumanPlayer : Player =
struct
  let is_human = true

  let draw_card plyr =
    print_endline "You drew: ";
    if (not (List.length plyr.deck = 0))
    then
      let card = List.hd plyr.deck in
      let new_deck = List.tl plyr.deck in
      let new_hand = card::plyr.hand in
      let new_plyr = {plyr with hand = new_hand; deck = new_deck} in
      print_card card; new_plyr
    else let () = print_endline "nothing. The deck is empty." in plyr

  let start_turn st =
    let (plyr1, plyr2) = st.players in
    let new_plyrs = if st.first_player
                    then (plyr1 |> draw_card, plyr2)
                    else (plyr1, plyr2 |> draw_card) in
    let new_st = {st with players = new_plyrs} in
    new_st |> print_state; new_st

  let end_turn st =
    match st.first_player with
    | true -> {st with turn = st.turn+1; first_player = false}
    | false -> {st with turn = st.turn+1; first_player = true}

  let rec pick_card p =
    let s = print_endline "Play a card in your hand"; read_line () in
    match s with
    | "end" -> None
    | _ -> (try Some (List.hd (List.filter (fun c -> s = c.name) p.hand)) with
           | _ -> (let () = print_endline "This card is not in your hand" in
                  pick_card p))

  let rec choose_card p =
    let c = pick_card p in
    match c with
    | None -> p
    | Some card when card.cost>p.mana -> print_endline "Not enough mana to play
                                         this card"; choose_card p
    | Some card -> (let new_mana = p.mana - card.cost in
                   let new_weap = match card.cat with
                                  | Minion _ | Spell _ -> None
                                  | Weapon _ -> Some card in
                   let new_hand = List.filter (fun c -> c <> card) p.hand in
                   let new_mins = match card.cat with
                                  | Spell _ | Weapon _ -> p.minions
                                  | Minion _ -> card::p.minions in
                   {p with mana = new_mana; weap = new_weap; hand = new_hand;
                   minions = new_mins} )

  let pre_phase st =
    let pre_st = start_turn st in
    let player = match pre_st.first_player with
                 | true -> fst pre_st.players
                 | false -> snd pre_st.players in
    let rec play_card p =
      let s = print_endline "Do you want to play a card?"; read_line () in
      match s with
      | "no" -> p
      | "yes" -> print_endline "(type \"end\" to stop)";
                  (choose_card p) |> play_card
      | _ -> print_endline "Command not understood. Type yes or no."; play_card p
    in
    let new_player = play_card player in
    if st.first_player then {st with players = (new_player, snd st.players)}
    else {st with players = (fst st.players, new_player)}

  let attack_phase st =
    raise Unimplemented

  let post_phase st =
    let player = match st.first_player with
                 | true -> fst st.players
                 | false -> snd st.players in
    let rec play_card p =
      let s = print_endline "Do you want to play a card?"; read_line () in
      match s with
      | "no" -> p
      | "yes" -> print_endline "(type \"end\" to stop)";
                  (choose_card p) |> play_card
      | _ -> print_endline "Command not understood. Type yes or no."; play_card p
    in
    let new_player = play_card player in
    if st.first_player then
      (end_turn {st with players = (new_player, snd st.players)})
    else (end_turn {st with players = (fst st.players, new_player)})
end

module AIPlayer : Player =
struct
  let is_human = false
  let draw_card st =
    raise Unimplemented
  let start_turn st =
    raise Unimplemented
  let pre_phase st =
    raise Unimplemented
  let attack_phase st =
    raise Unimplemented
  let post_phase st =
    raise Unimplemented
  let end_turn st =
    raise Unimplemented
end

