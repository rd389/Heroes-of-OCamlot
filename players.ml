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
                      print_endline ("You have healed! Now your health is " ^
                                    (string_of_int new_p.hp));
                      match st.first_player with
                      | true -> {st with players = (new_p,op)}
                      | false -> {st with players = (op,new_p)})
            | Dmg -> (let new_p = if p.armor=0 then {p with hp = p.hp - sp.mag}
                                  else if p.armor-sp.mag >= 0 then
                                    {p with armor = p.armor-sp.mag}
                                  else let new_att = sp.mag-p.armor in
                                    {p with armor=0; hp= p.hp - new_att}
                                  in
                     print_endline ("You have hurt yourself! Now your health is " ^
                                   (string_of_int new_p.hp));
                     match st.first_player with
                     | true -> {st with players = (new_p,op)}
                     | false -> {st with players = (op,new_p)})
            | Mana -> (let new_p = {p with mana = p.mana + sp.mag} in
                      print_endline ("You boosted your mana! Now your mana is " ^
                                    (string_of_int new_p.mana));
                      match st.first_player with
                      | true -> {st with players = (new_p,op)}
                      | false -> {st with players = (op,new_p)}))
    | Them -> (match sp.effect with
              | Heal -> (let new_op = {op with hp = op.hp + sp.mag} in
                        print_endline ("You have healed the opponent! Now their health is " ^
                                      (string_of_int new_op.hp));
                        match st.first_player with
                        | true -> {st with players = (p,new_op)}
                        | false -> {st with players = (new_op,p)})
              | Dmg -> (let new_op = if op.armor=0 then {op with hp = op.hp-sp.mag}
                                     else if op.armor-sp.mag>=0 then
                                       {op with armor = op.armor-sp.mag}
                                     else let new_att = sp.mag - op.armor in
                                       {op with armor=0; hp= op.hp-new_att}
                                     in
                       print_endline ("You have hurt the opponent! Now their health is " ^
                                      (string_of_int new_op.hp));
                       match st.first_player with
                       | true -> {st with players = (p,new_op)}
                       | false -> {st with players = (new_op,p)})
              | Mana -> (let new_op = {op with mana = op.mana + sp.mag} in
                        print_endline ("You boosted the opponent's mana! Now their mana is " ^
                                      (string_of_int new_op.mana));
                        match st.first_player with
                        | true -> {st with players = (p,new_op)}
                        | false -> {st with players = (new_op,p)}))
    | Mine -> (match sp.effect with
              | Heal -> print_string "Choose one of your minions to heal\n> ";
                       (let s = read_line () in
                        let c = pick_minion p s in
                        let m = match c.cat with Minion min -> min
                                | _ -> failwith "Sum Ting Wong" in
                        let new_m = {m with hp = m.hp + sp.mag} in
                        let new_c = {c with cat = Minion new_m} in
                        print_endline ("You have healed your minion" ^ new_c.name);
                        let new_mins = List.filter (fun card -> card <> c)
                                       p.minions in
                        let new_p = {p with minions = new_c::new_mins} in
                        match st.first_player with
                        | true -> {st with players = (new_p,op)}
                        | false -> {st with players = (op,new_p)})
              | Dmg -> print_string "Choose one of your minions to damage\n> ";
                      (let s = read_line () in
                       let c = pick_minion p s in
                       let m = match c.cat with Minion min -> min
                               | _ -> failwith "Sum Ting Wong" in
                       let new_m = {m with hp = m.hp - sp.mag} in
                       let new_c = {c with cat = Minion new_m} in
                       print_endline ("You have hurt your minion" ^ new_c.name);
                       let new_mins = List.filter (fun card -> card <> c)
                                      p.minions in
                       let new_p = {p with minions = new_c::new_mins} in
                       match st.first_player with
                       | true -> {st with players = (new_p,op)}
                       | false -> {st with players = (op,new_p)})
              | Mana -> print_endline "No mana effect on minions"; st)
    | Theirs -> (match sp.effect with
                | Heal -> print_string "Choose one of the opponent's minions to heal\n> ";
                         (let s = read_line () in
                          let c = pick_minion op s in
                          let m = match c.cat with Minion min -> min
                                  | _ -> failwith "Sum Ting Wong" in
                          let new_m = {m with hp = m.hp + sp.mag} in
                          let new_c = {c with cat = Minion new_m} in
                          print_endline ("You have healed the opponent's minion" ^ new_c.name);
                          let new_mins = List.filter (fun card -> card <> c)
                                         op.minions in
                          let new_op = {op with minions = new_c::new_mins} in
                          match st.first_player with
                          | true -> {st with players = (p,new_op)}
                          | false -> {st with players = (new_op,p)})
                | Dmg -> print_string "Choose one of the opponent's minions to damage\n> ";
                        (let s = read_line () in
                         let c = pick_minion op s in
                         let m = match c.cat with Minion min -> min
                                 | _ -> failwith "Sum Ting Wong" in
                         let new_m = {m with hp = m.hp - sp.mag} in
                         let new_c = {c with cat = Minion new_m} in
                         print_endline ("You have hurt the opponent's minion" ^ new_c.name);
                         let new_mins = List.filter (fun card -> card <> c)
                                        op.minions in
                         let new_op = {op with minions = new_c::new_mins} in
                         match st.first_player with
                         | true -> {st with players = (p,new_op)}
                         | false -> {st with players = (new_op,p)})
                | Mana -> print_endline "No mana effect on minions"; st)
    | Any ->  print_string "Choose a target (Me, Them, Mine, or Theirs).\n> ";
              (match (read_line ()) with
              | "Me" -> (play_spell st {sp with target = Me})
              | "Them" -> (play_spell st {sp with target = Them})
              | "Mine" -> (play_spell st {sp with target = Mine})
              | "Theirs" -> (play_spell st {sp with target = Theirs})
              | _ -> (print_endline "Invalid target.";
                     play_spell st sp)
             )

  let rec pick_card p =
    let s = print_string "Play a card in your hand\n> "; read_line () in
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
                   match card.cat with
                   | Minion _ -> print_endline ("You have played the minion " ^
                                 card.name); inter_st
                   | Weapon _ -> print_endline ("You have equipped the weapon " ^
                                 card.name); inter_st
                   | Spell sp -> print_endline ("You have used the spell " ^
                                 card.name); play_spell inter_st sp
                   )

  let rec play_card state =
    let s = print_string "Do you want to play a card?(y/n)\n> ";
      read_line () in
    match s with
    | "n" -> state
    | "y" -> print_endline "(type \"end\" to stop)";
                (choose_card state) |> play_card
    | _ -> print_endline "Command not understood. Please type y or n.";
           play_card state

  let pre_phase st =
    let pre_st = start_turn st in
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
        let rec attack_hero at =
          if(!opp.armor <> 0) then
            ( let new_armor = (!opp).armor - a in
              if new_armor <= 0 then
                (opp := {!opp with armor = 0};
                 print_endline (them ^ "'s Armor: 0"); attack_hero (~- new_armor)
                )
              else(opp := {!opp with armor = new_armor};
                   print_endline (them ^ "'s Armor: " ^ (string_of_int new_armor)));
                   print_endline (them ^ "'s HP: " ^(string_of_int (!opp).hp))
              )
          else (let new_hp = (!opp).hp - a in
                if new_hp <= 0 then raise GameOver
                else (opp := {!opp with hp = new_hp};
                  print_endline (them ^ "'s HP: " ^ (string_of_int new_hp)))) in
        match read_line () with
        | "" -> ()
        | x when x = them -> attack_hero a
        | y -> (if (List.exists (fun c -> c.name = y) (!theirs)) then
                  ( let mins = attack_minion y (!theirs) in
                    theirs := mins)
                else (print_string "Invalid target, try again.\n> ";
                  get_target a)) in
      let alter_weap () =
        match (!player).weap with
        | None -> ()
        | Some c ->(match c.cat with
                    | Weapon w -> if w.durability = 1 then
                                   (print_endline (c.name ^ " broke!");
                                    player := {!player with weap = None})
                                  else
                                   (let new_dur = w.durability - 1 in
                                    let new_weap = Weapon {w with durability = new_dur} in
                                    let new_c = {c with cat = new_weap} in
                                    print_endline("Weapon durability -> " ^ (string_of_int new_dur));
                                    player := {!player with weap = Some new_c})
                    | _ -> print_endline "Sum ting wong" ) in
      match ms with
      | [] -> if(!hero_attack <> 0) then
               (print_string "Pick a target for your hero:\n> ";
                get_target (!hero_attack); alter_weap (); )
              else ();
              print_endline "Press Enter/Return"; ignore (read_line ());
      | h::t -> ( print_string ("Pick a target for your " ^
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
      print_state s;
      print_endline ("Your hero has " ^ a ^ " attack."); s in
    let _ = Sys.command "clear" in
    new_state |> start_attack |> end_state

  let post_phase st =
    let _ = Sys.command "clear" in
    print_state st;
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

