open State

exception Unimplemented
exception GameOver
exception GameOverOpp

(* A [Player] runs each phase of a player's turn *)
module type Player = sig
  (* whether or not the player is a human *)
  val is_human : bool
  (* draws a card from the current player's deck and places it in that player's
   * hand. *)
  val draw_card : hero -> hero
  (* [start_turn st] returns the game state after the start of a turn. (Used in
   * pre_phase) *)
  val start_turn : state -> state
  (* [pre_phase st] returns the game state after the player's pre-phase *)
  val pre_phase : state -> state
  (* [attack_phase st] returns the game state after the player's attack phase *)
  val attack_phase : state -> state
  (* [post_phase st] summarizes the results of the attack_phase, checks for any
   * conditions under which the game should end (in which case the game ends),
   * and ends the turn of the current player. *)
  val post_phase : state -> state
  (* [end_turn st] ends the current player's turn. (Used in post_phase) *)
  val end_turn : state -> state
end
(* A [HumanPlayer] is a Player whose functions are implemented for user-control
 * of what choices are made at each phase of the turn *)
module HumanPlayer : Player
(* An [AIPlayer] is a Player whose functions are implemented with algorithms
 * for determining the best possible move at each phase of the turn *)
module AIPlayer : Player
