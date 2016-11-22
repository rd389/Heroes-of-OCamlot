open State

exception Unimplemented

(* A [Player] runs each phase of a player's turn *)
module type Player = sig
  (* whether or not the player is a human *)
  val is_human : bool
  (* the player number that this player is in the game state *)
  val p_num : bool
  (* [draw_card st] returns the game state after the player has drawn the top
    card from their deck *)
  val draw_card : state -> state
  (* [start_turn st] returns the game state after the start of a turn *)
  val start_turn : state -> state
  (* [pre_phase st] returns the game state after the player's pre-phase *)
  val pre_phase : state -> state
  (* [attack_phase st] returns the game state after the player's attack phase *)
  val attack_phase : state -> state
  (* [post_phase st] returns the game state after the player's post-phase *)
  val post_phase : state -> state
end
(* A [HumanPlayer] is a Player whose functions are implemented for user-control
  of what choices are made at each phase of the turn *)
module HumanPlayer : Player
(* An [AIPlayer] is a Player whose functions are implemented with algorithms
  for determining the best possible move at each phase of the turn *)
module AIPlayer : Player
