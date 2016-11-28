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
  let pre_phase st =
    raise Unimplemented
  let attack_phase st =
    raise Unimplemented
  let post_phase st =
    let player = match st.first_player with
                 | true -> fst st.players
                 | false -> snd st.players in
    let rec play_card p =
      match p.mana with
      | 0 ->
      | m when m<0 -> play_card {p with mana = 0}
      | m when m>0 -> let c = print_endline "Choose a card in your hand to play";
                              read_line ()
    in
    play_card player
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

