open Players
open State
open Yojson.Basic.Util
exception Unimplemented

(* get_int k j] is the int associated with the value mapped to key [k] in
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

let build_minion j =
  raise Unimplemented

let build_spell j =
  raise Unimplemented

let build_weapon j =
  raise Unimplemented

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
  raise Unimplemented
