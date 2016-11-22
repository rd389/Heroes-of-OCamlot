open State

exception Unimplemented

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

