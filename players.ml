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

  let pre_phase st =
    raise Unimplemented
  let attack_phase st =
    raise Unimplemented
  let post_phase st =
    raise Unimplemented
  let end_turn st =
    raise Unimplemented
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

