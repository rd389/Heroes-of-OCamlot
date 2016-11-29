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

  let replenish_mana st =
    let player = match st.first_player with
                 | true -> fst st.players
                 | false -> snd st.players in
    let new_player = if st.turn > 10 then {player with mana = 10}
                     else {player with mana = st.turn} in
    if st.first_player then {st with players = (new_player, snd st.players)}
    else {st with players = (fst st.players, new_player)}

  let start_turn st =
    let start_state = replenish_mana st in
    let (plyr1, plyr2) = start_state.players in
    let new_plyrs = if start_state.first_player
                    then (plyr1 |> draw_card, plyr2)
                    else (plyr1, plyr2 |> draw_card) in
    let new_st = {start_state with players = new_plyrs} in
    new_st |> print_state; new_st

  let end_turn st =
    match st.first_player with
    | true -> {st with first_player = false}
    | false -> {st with turn = st.turn+1; first_player = true}

  let rec pick_minion p s : card =
    try List.hd (List.filter (fun c -> s = c.name) p.minions) with
    | _ -> print_endline "Not a minion in play"; pick_minion p s

  let rec play_spell st sp =
    let p = match st.first_player with
            | true -> fst st.players
            | false -> snd st.players in
    let op = match st.first_player with
             | true -> snd st.players
             | false -> fst st.players in
    match sp.target with
    | All -> (let inter_st1 = play_spell st {sp with target = Me} in
             let inter_st2 = play_spell inter_st1 {sp with target = Mine} in
             let inter_st3 = play_spell inter_st2 {sp with target = Them} in
             play_spell inter_st3 {sp with target = Theirs})
    | Me -> (match sp.effect with
            | Heal -> (let new_p = {p with hp = p.hp + sp.mag} in
                      match st.first_player with
                      | true -> {st with players = (new_p,op)}
                      | false -> {st with players = (op,new_p)})
            | Dmg -> (let new_p = {p with hp = p.hp - sp.mag} in
                     match st.first_player with
                     | true -> {st with players = (new_p,op)}
                     | false -> {st with players = (op,new_p)})
            | Mana -> (let new_p = {p with mana = p.mana + sp.mag} in
                      match st.first_player with
                      | true -> {st with players = (new_p,op)}
                      | false -> {st with players = (op,new_p)}))
    | Them -> (match sp.effect with
              | Heal -> (let new_op = {op with hp = op.hp + sp.mag} in
                        match st.first_player with
                        | true -> {st with players = (p,new_op)}
                        | false -> {st with players = (new_op,p)})
              | Dmg -> (let new_op = {op with hp = op.hp - sp.mag} in
                       match st.first_player with
                       | true -> {st with players = (p,new_op)}
                       | false -> {st with players = (new_op,p)})
              | Mana -> (let new_op = {op with mana = op.mana + sp.mag} in
                        match st.first_player with
                        | true -> {st with players = (p,new_op)}
                        | false -> {st with players = (new_op,p)}))
    | Mine -> (match sp.effect with
              | Heal -> (let s = print_endline
                                 "Choose one of your minions to heal";
                                 read_line () in
                        let c = pick_minion p s in
                        let m = match c.cat with Minion min -> min
                                | _ -> failwith "Sum Ting Wong" in
                        let new_m = {m with hp = m.hp + sp.mag} in
                        let new_c = {c with cat = Minion new_m} in
                        let new_mins = List.filter (fun card -> card <> c)
                                       p.minions in
                        let new_p = {p with minions = new_c::new_mins} in
                        match st.first_player with
                        | true -> {st with players = (new_p,op)}
                        | false -> {st with players = (op,new_p)})
              | Dmg -> (let s = print_endline
                                "Choose one of your minions to attack";
                                read_line () in
                       let c = pick_minion p s in
                       let m = match c.cat with Minion min -> min
                               | _ -> failwith "Sum Ting Wong" in
                       let new_m = {m with hp = m.hp - sp.mag} in
                       let new_c = {c with cat = Minion new_m} in
                       let new_mins = List.filter (fun card -> card <> c)
                                      p.minions in
                       let new_p = {p with minions = new_c::new_mins} in
                       match st.first_player with
                       | true -> {st with players = (new_p,op)}
                       | false -> {st with players = (op,new_p)})
              | Mana -> print_endline "No mana effect on minions"; st)
    | Theirs -> (match sp.effect with
                | Heal -> (let s = print_endline
                                   "Choose one of the opponent's minions to heal";
                                   read_line () in
                          let c = pick_minion op s in
                          let m = match c.cat with Minion min -> min
                                  | _ -> failwith "Sum Ting Wong" in
                          let new_m = {m with hp = m.hp + sp.mag} in
                          let new_c = {c with cat = Minion new_m} in
                          let new_mins = List.filter (fun card -> card <> c)
                                         op.minions in
                          let new_op = {op with minions = new_c::new_mins} in
                          match st.first_player with
                          | true -> {st with players = (p,new_op)}
                          | false -> {st with players = (new_op,p)})
                | Dmg -> (let s = print_endline
                                  "Choose one of the opponent's minions to attack";
                                  read_line () in
                         let c = pick_minion op s in
                         let m = match c.cat with Minion min -> min
                                 | _ -> failwith "Sum Ting Wong" in
                         let new_m = {m with hp = m.hp - sp.mag} in
                         let new_c = {c with cat = Minion new_m} in
                         let new_mins = List.filter (fun card -> card <> c)
                                        op.minions in
                         let new_op = {op with minions = new_c::new_mins} in
                         match st.first_player with
                         | true -> {st with players = (p,new_op)}
                         | false -> {st with players = (new_op,p)})
                | Mana -> print_endline "No mana effect on minions"; st)
    | Any -> (match (print_endline "Choose a target (Me, Them, Mine, or Theirs).";
                    read_line ()) with
              | "Me" -> (play_spell st {sp with target = Me})
              | "Them" -> (play_spell st {sp with target = Them})
              | "Mine" -> (play_spell st {sp with target = Mine})
              | "Theirs" -> (play_spell st {sp with target = Theirs})
              | _ -> (print_endline "Invalid target.";
                     play_spell st sp)
             )

  let rec pick_card p =
    let s = print_endline "Play a card in your hand"; read_line () in
    match s with
    | "end" -> None
    | _ -> (try Some (List.hd (List.filter (fun c -> s = c.name) p.hand)) with
           | _ -> (let () = print_endline "This card is not in your hand" in
                  pick_card p))

  let rec choose_card st =
    let p = match st.first_player with
            | true -> fst st.players
            | false -> snd st.players in
    let c = pick_card p in
    match c with
    | None -> st
    | Some card when card.cost>p.mana -> print_endline
                                         "Not enough mana to play this card";
                                         choose_card st
    | Some card -> (let new_mana = p.mana - card.cost in
                   let new_hand = List.filter (fun c -> c <> card) p.hand in
                   let new_weap = match card.cat with
                                  | Minion _ | Spell _ -> None
                                  | Weapon _ -> Some card in
                   let new_mins = match card.cat with
                                  | Spell _ | Weapon _ -> p.minions
                                  | Minion _ -> card::p.minions in
                   let new_p = {p with mana = new_mana; weap = new_weap;
                     hand = new_hand; minions = new_mins} in
                   let inter_st = match st.first_player with
                                  | true -> {st with players =
                                              (new_p, snd st.players)}
                                  | false -> {st with players =
                                               (fst st.players, new_p)} in
                   let new_st = match card.cat with
                                | Minion _ | Weapon _ -> inter_st
                                | Spell sp -> play_spell inter_st sp in
                   new_st)

  let pre_phase st =
    let pre_st = start_turn st in
    let rec play_card state =
      let s = print_endline "Do you want to play a card?"; read_line () in
      match s with
      | "no" -> state
      | "yes" -> print_endline "(type \"end\" to stop)";
                  (choose_card state) |> play_card
      | _ -> print_endline "Command not understood. Type yes, no, or end.";
             play_card state
    in
    play_card pre_st

  let attack_phase st =
    let player = if st.first_player then ref (fst st.players)
                 else ref (snd st.players) in
    let opp = if st.first_player then ref (snd st.players)
              else ref (fst st.players) in
    let hero_attack = match (!player).weap with
                      | None -> ref 0
                      | Some c -> match c.cat with
                                  | Weapon w -> ref w.dmg
                                  | _ -> print_endline "Sum ting wong"; ref 0 in
    let get_bonus c =
      match c.cat with
      | Minion c -> c.bonus
      | _ -> print_endline "Sum ting wong"; [] in
    let add_bonus p m =
      let bonus = get_bonus m in
      let new_armor = try(p.armor + List.assoc Armor bonus) with
                      | _ -> p.armor in
      let add_hp = try(p.hp + List.assoc HealM bonus) with
                       | _ -> p.hp in
      let new_hp = if add_hp > 30 then 30 else add_hp in
      let add_attack = try(List.assoc Attack bonus) with
                       | _ -> 0 in
      hero_attack := (!hero_attack) + add_attack;
      {p with armor = new_armor; hp = new_hp} in
    let new_state =
      player := List.fold_left add_bonus (!player) (!player).minions;
      if st.first_player then
                      {st with players = (!player, snd st.players)}
                    else {st with players = (fst st.players, !player)} in
    let them = if st.first_player then "P2" else "P1" in
    let theirs = ref (!opp).minions in
    let rec get_ans () =
      match read_line () with
      | "y" -> true
      | "n" -> false
      | _ -> print_endline "You a dumb slut?"; print_string "> "; get_ans () in
    let format_minion c =
      match c.cat with
      | Minion m -> ((string_of_int m.attack) ^ "/" ^ (string_of_int m.hp) ^ " "
                      ^ c.name)
      | _ -> "Sum ting wong" in
    let rec target ms =
      let rec get_target a =
        let rec attack_minion targ ts =
          match ts with
          | [] -> []
          | h::t -> if(h.name = targ) then
                     (let msg ="Is "^(format_minion h)^" your target?(y/n)" in
                      let health = match h.cat with Minion c -> c.hp | _ -> 0 in
                      let new_cat = match h.cat with
                                    | Minion c -> Minion({c with hp = c.hp - a})
                                    | _ -> h.cat in
                      let new_min = {h with cat = new_cat} in
                      print_endline msg; print_string "> ";
                      if(get_ans ()) then
                       (if(health <= a) then
                          (print_endline ((format_minion h) ^ " has been slain."); t)
                        else (print_endline ((format_minion h) ^ " is now " ^ (format_minion new_min));
                          new_min::t))
                      else h::(attack_minion targ t))
                    else(h::(attack_minion targ t)) in
        match read_line () with
        | "" -> ()
        | x when x = them -> (if((!opp).hp <= a) then
                                raise GameOver
                              else ( let new_hp = (!opp).hp - a in
                                opp := {!opp with hp = new_hp};
                                print_endline (x ^ " hp: " ^
                                              (string_of_int new_hp))))
        | y -> (if (List.exists (fun c -> c.name = y) (!theirs)) then
                  ( let mins = attack_minion y (!theirs) in
                    theirs := mins)
                else (print_string "Invalid target, try again.\n> ";
                  get_target a)) in
      match ms with
      | [] -> print_string "Pick a target for your hero:\n> ";
              get_target (!hero_attack);
      | h::t -> ( print_endline ("Pick a target for your " ^
                                  (format_minion h) ^ "\n> ");
                match h.cat with
                | Minion c -> get_target c.attack; target t;
                |  _ -> print_endline "Sum ting wong"; );
      opp := {!opp with minions = (!theirs)} in
    let end_state s =
      target (!player).minions;
      if s.first_player then {s with players = (!player, !opp)}
        else {s with players = (!opp, !player)} in
    let start_attack s =
      let a= string_of_int (!hero_attack) in
      ignore (Sys.command "clear"); print_state s;
      print_endline ("Your hero has " ^ a ^ " attack."); s in
    new_state |> start_attack |> end_state

  let post_phase st =
    let rec play_card state =
      let s = print_endline "Do you want to play a card?"; read_line () in
      match s with
      | "no" -> state
      | "yes" -> print_endline "(type \"end\" to stop)";
                  (choose_card state) |> play_card
      | _ -> print_endline "Command not understood. Type yes, no, or end.";
             play_card state
    in
    let new_state = play_card st in
    let new_player = match new_state.first_player with
                     | true -> fst new_state.players
                     | false -> snd new_state.players in
    if (new_player.weap = None && new_player.hand = [] && new_player.deck = []
    && new_player.minions = []) then raise GameOver
    else end_turn new_state
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

