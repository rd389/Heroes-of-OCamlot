open State

exception Unimplemented
exception GameOver

module type Player = sig
  val is_human : bool
  val draw_card : state -> state
  val start_turn : state -> state
  val pre_phase : state -> state
  val attack_phase : state -> state
  val post_phase : state -> state
  val end_turn : state -> state
end

module HumanPlayer : Player =
struct
  let is_human = true
  let draw_card st =
    raise Unimplemented
  let start_turn st =
    raise Unimplemented

  let rec pick_card p =
    let s = print_endline "Play a card in your hand"; read_line () in
    match s with
    | "end" -> None
    | _ -> (try Some (List.hd (List.filter (fun c -> s = c.name) p.hand)) with
           | _ -> (let () = print_endline "This card is not in your hand" in
                  pick_card p))

  let choose_card p =
    let c = pick_card p in
    match c with
    | None -> p
    | Some card -> (let new_mana = if p.mana - card.cost < 0 then 0
                                  else p.mana - card.cost in
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
    raise Unimplemented

  let attack_phase st =
    raise Unimplemented

  let post_phase st =
    let player = match st.first_player with
                 | true -> fst st.players
                 | false -> snd st.players in
    let rec play_card p =
      if p.mana>0 then (let new_p = choose_card p in play_card new_p)
      else p
    in
    let new_player = play_card player in
    if st.first_player then {st with players = (new_player, snd st.players)}
    else {st with players = (fst st.players, new_player)}

  let end_turn st =
    match st.first_player with
    | true -> {st with first_player = false}
    | false -> {st with first_player = true}
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

