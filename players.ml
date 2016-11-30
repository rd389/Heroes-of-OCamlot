open State

exception Unimplemented
exception GameOver
exception GameOverOpp

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
    else let () = print_endline "nothing. The deck is empty" in plyr

  (* [replenish_mana st] Returns the state after replenishing the current
   * player's mana in state [st] based on the turn number in that state. *)
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

  (* [pick_minion p s] returns player [p]'s minion card indicated by string [s].
   * Prompts player to pick a minion in play is input [s] is not a minion in
   * play. *)
  let rec pick_minion p s : card =
    try List.hd (List.filter (fun c -> s = c.name) p.minions) with
    | _ -> print_endline "Not a minion in play"; pick_minion p (read_line ())

  (* [play_spell st sp] returns the state after a player plays the spell [sp] in
   * state [st], based on the target and effect of the spell. The necessary
   * prompts are used when there is a choice in who the player wishes to use
   * the spell on. *)
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
                     if new_p.hp<=0 then raise GameOverOpp
                     else
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
                       if new_op.hp<=0 then raise GameOver
                       else
                       match st.first_player with
                       | true -> {st with players = (p,new_op)}
                       | false -> {st with players = (new_op,p)})
              | Mana -> (let new_op = {op with mana = op.mana + sp.mag} in
                        print_endline ("You boosted the opponent's mana! Now their mana is " ^
                                      (string_of_int new_op.mana));
                        match st.first_player with
                        | true -> {st with players = (p,new_op)}
                        | false -> {st with players = (new_op,p)}))
    | Mine -> if p.minions = []
              then (print_endline "Can't use spell; you have no minions in play"; st)
              else
              (match sp.effect with
              | Heal -> print_string "Choose one of your minions to heal\n> ";
                       (let s = read_line () in
                        let c = pick_minion p s in
                        let m = match c.cat with Minion min -> min
                                | _ -> failwith "Error" in
                        let new_m = {m with hp = m.hp + sp.mag} in
                        let new_c = {c with cat = Minion new_m} in
                        print_endline ("You have healed your minion " ^ new_c.name);
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
                               | _ -> failwith "Error" in
                       let new_m = {m with hp = m.hp - sp.mag} in
                       let new_c = {c with cat = Minion new_m} in
                       print_endline ("You have hurt your minion " ^ new_c.name);
                       let new_mins = List.filter (fun card -> card <> c)
                                      p.minions in
                       let new_p = if new_m.hp<=0 then
                         (print_endline
                         ("You have slain your minion " ^ new_c.name);
                         {p with minions = new_mins})
                         else {p with minions = new_c::new_mins} in
                       match st.first_player with
                       | true -> {st with players = (new_p,op)}
                       | false -> {st with players = (op,new_p)})
              | Mana -> print_endline "No mana effect on minions"; st)
    | Theirs -> if op.minions = []
                then (print_endline "Can't use spell; opponent has no minions in play"; st)
                else
                (match sp.effect with
                | Heal -> print_string "Choose one of the opponent's minions to heal\n> ";
                         (let s = read_line () in
                          let c = pick_minion op s in
                          let m = match c.cat with Minion min -> min
                                  | _ -> failwith "Error" in
                          let new_m = {m with hp = m.hp + sp.mag} in
                          let new_c = {c with cat = Minion new_m} in
                          print_endline ("You have healed the opponent's minion " ^ new_c.name);
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
                                 | _ -> failwith "Error" in
                         let new_m = {m with hp = m.hp - sp.mag} in
                         let new_c = {c with cat = Minion new_m} in
                         print_endline ("You have hurt the opponent's minion " ^ new_c.name);
                         let new_mins = List.filter (fun card -> card <> c)
                                        op.minions in
                         let new_op = if new_m.hp<=0 then
                           (print_endline ("You have slain the opponent's minion " ^ new_c.name);
                           {op with minions = new_mins})
                           else {op with minions = new_c::new_mins} in
                         match st.first_player with
                         | true -> {st with players = (p,new_op)}
                         | false -> {st with players = (new_op,p)})
                | Mana -> print_endline "No mana effect on minions"; st)
    | Any ->  print_string "Choose a target (Me, Them, Mine, or Theirs)\n> ";
              (match (read_line ()) with
              | "Me" -> (play_spell st {sp with target = Me})
              | "Them" -> (play_spell st {sp with target = Them})
              | "Mine" -> (play_spell st {sp with target = Mine})
              | "Theirs" -> (play_spell st {sp with target = Theirs})
              | _ -> (print_endline "Invalid target";
                     play_spell st sp)
             )

  (* [pick_card p] Prompts the player [p] to pick a card in their hand to play;
   * reprompts player to pick a card if the input is not a card in their hand.
   * Returns a card option associated with the card picked, or no card if the
   * player so chooses. *)
  let rec pick_card p =
    let s = print_string "Play a card in your hand\n> "; read_line () in
    match s with
    | "end" -> None
    | _ -> (try Some (List.hd (List.filter (fun c -> s = c.name) p.hand)) with
           | _ -> (let () = print_endline "This card is not in your hand" in
                  pick_card p))

  (* [choose_card st] returns the state after the current player plays a card in
   * state [st]. *)
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
    | Some card -> (match card.cat with
                   | Minion m -> (let new_mana = p.mana - card.cost in
                                 let new_hand = List.filter (fun c -> c <> card) p.hand in
                                 let new_mins = card::p.minions in
                                 let new_p = {p with mana = new_mana; hand = new_hand; minions = new_mins} in
                                 print_endline ("You have played the minion " ^
                                 card.name);
                                 print_endline ("Your mana is now " ^
                                 (string_of_int new_mana));
                                 match st.first_player with
                                 | true -> {st with players =
                                             (new_p, snd st.players)}
                                 | false -> {st with players =
                                              (fst st.players, new_p)})
                   | Weapon w -> (let new_mana = p.mana - card.cost in
                                 let new_hand = List.filter (fun c -> c <> card) p.hand in
                                 let new_weap = Some card in
                                 let new_p = {p with mana = new_mana; hand = new_hand; weap = new_weap} in
                                 print_endline ("You have equipped the weapon " ^
                                 card.name);
                                 print_endline ("Your mana is now " ^
                                 (string_of_int new_mana));
                                 match st.first_player with
                                 | true -> {st with players =
                                             (new_p, snd st.players)}
                                 | false -> {st with players =
                                              (fst st.players, new_p)})
                   | Spell sp -> (let new_mana = p.mana - card.cost in
                                 let new_hand = List.filter (fun c -> c <> card) p.hand in
                                 let new_p = {p with mana =new_mana; hand = new_hand} in
                                 print_endline ("You have used the spell " ^
                                 card.name);
                                 print_endline ("Your mana is now " ^
                                 (string_of_int new_mana));
                                 match st.first_player with
                                 | true -> play_spell {st with players =
                                             (new_p, snd st.players)} sp
                                 | false -> play_spell {st with players =
                                              (fst st.players, new_p)} sp)
                   )

  (* [play_card state] prompts the current player to play cards until they wish
   * to stop doing so. *)
  let rec play_card state =
    let s = print_string "Do you want to play a card?(y/n)\n> ";
      read_line () in
    match s with
    | "n" -> state
    | "y" -> print_endline "(type \"end\" to stop)";
                (choose_card state) |> play_card
    | _ -> print_endline "Command not understood. Please type y or n";
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
                                  | _ -> failwith "Error" in
    let get_bonus c =
      match c.cat with
      | Minion c -> c.bonus
      | _ -> failwith "Error" in
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
      | _ -> print_string "Please enter y or n\n> "; get_ans () in
    let format_minion c =
      match c.cat with
      | Minion m -> ((string_of_int m.attack) ^ "/" ^ (string_of_int m.hp) ^ " "
                      ^ c.name)
      | _ -> failwith "Error" in
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
                          (print_endline ((format_minion h) ^ " has been slain"); t)
                        else (print_endline (them ^ "'s " ^ (format_minion h) ^
                                " is now a " ^ (format_minion new_min));
                          new_min::t))
                      else h::(attack_minion targ t))
                    else(h::(attack_minion targ t)) in
        let rec attack_hero at =
          let new_armor = (!opp).armor - a in
          if(new_armor <= 0) then
           (let new_hp = (!opp).hp + new_armor in
            opp := {!opp with armor = 0; hp = new_hp};
            print_endline (them ^ "'s Armor: 0");
            print_endline (them ^ "'s HP: " ^(string_of_int (!opp).hp));
            if (new_hp <= 0) then raise GameOver else ();
           )
          else(
            opp := {!opp with armor = new_armor};
            print_endline (them ^ "'s Armor: " ^(string_of_int (!opp).armor));
            print_endline (them ^ "'s HP: " ^(string_of_int (!opp).hp));
            ) in
        match read_line () with
        | "" -> ()
        | x when x = them -> attack_hero a
        | y -> (if (List.exists (fun c -> c.name = y) (!theirs)) then
                  ( let mins = attack_minion y (!theirs) in
                    if mins = !theirs then
                     (print_string "No target selected, try again\n> ";
                      get_target a)
                    else (theirs := mins))
                else (print_string "Invalid target, try again\n> ";
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
                    | _ -> failwith "Error" ) in
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
                |  _ -> failwith "Error" );
      opp := {!opp with minions = (!theirs)} in
    let end_state s =
      target (!player).minions;
      if s.first_player then {s with players = (!player, !opp)}
        else {s with players = (!opp, !player)} in
    let start_attack s =
      let a= string_of_int (!hero_attack) in
      print_state s;
      print_endline ("Your hero has " ^ a ^ " attack"); s in
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
    && new_player.minions = []) then raise GameOverOpp
    else end_turn new_state
end

module AIPlayer : Player =
struct
  let is_human = false

  let draw_card plyr =
    if (not (List.length plyr.deck = 0))
    then
      let card = List.hd plyr.deck in
      let new_deck = List.tl plyr.deck in
      let new_hand = card::plyr.hand in
      let new_plyr = {plyr with hand = new_hand; deck = new_deck} in
      print_endline "AI drew a card";
      new_plyr
    else (print_endline "AI's deck is empty"; plyr)

  (* [replenish_mana st] Returns the state after replenishing the AI's mana
   * in state [st] based on the turn number in that state. *)
  let replenish_mana st =
    if st.first_player then failwith "Error"
    else
    let ai = snd st.players in
    let new_ai = if st.turn > 10 then {ai with mana = 10}
                     else {ai with mana = st.turn} in
    {st with players = (fst st.players, new_ai)}

  let start_turn st =
    if st.first_player then failwith "Error"
    else
    let start_state = replenish_mana st in
    let (p1, ai) = start_state.players in
    let new_plyrs = (p1, ai |> draw_card) in
    let new_st = {start_state with players = new_plyrs} in
    new_st

  let end_turn st =
    match st.first_player with
    | true -> failwith "Error"
    | false -> {st with turn = st.turn+1; first_player = true}

  (* [can_kill st sp] returns true if spell [sp] can kill either Player 1 or any
   * of Player 1's minions (depending on the target of the spell) in state [st].
   * Otherwise returns false. *)
  let can_kill st sp =
    if st.first_player then failwith "Error" else
    let (p,ai) = st.players in
    if sp.effect<>Dmg then failwith "Error" else
    match sp.target with
    | Them -> (if sp.mag>=p.armor+p.hp then true else false)
    | Theirs -> (let mins = List.map
                           (fun c -> match c.cat with
                                     | Minion m -> m.hp
                                     | _ -> failwith "Error")
                           p.minions in
                let boo = List.fold_left
                          (fun b hp -> if sp.mag>=hp then (b||true)
                                       else (b||false)) false mins in
                boo)
    | _ -> (failwith "Error")

  (* [play_spell st sp] returns the state after the AI plays spell [sp] in state
   * [st]. *)
  let rec play_spell st sp =
    if st.first_player then failwith "Error" else
    let (p,ai) = st.players in
    match sp.target with
    | All -> (let inter_st1 = play_spell st {sp with target = Me} in
             let inter_st2 = play_spell inter_st1 {sp with target = Mine} in
             let inter_st3 = play_spell inter_st2 {sp with target = Them} in
             play_spell inter_st3 {sp with target = Theirs})
    | Me -> (match sp.effect with
            | Heal -> (let new_ai = {ai with hp = ai.hp + sp.mag} in
                      print_endline ("AI has healed! AI's health is now " ^
                                    (string_of_int new_ai.hp));
                      {st with players = (p,new_ai)})
            | Dmg -> (let new_ai = if ai.armor=0 then {ai with hp = ai.hp-sp.mag}
                                   else if ai.armor-sp.mag >= 0 then
                                     {ai with armor = ai.armor-sp.mag}
                                   else let new_att = sp.mag-ai.armor in
                                     {ai with armor=0; hp= ai.hp - new_att}
                                   in
                     print_endline ("AI has hurt itself! AI's health is now " ^
                                   (string_of_int new_ai.hp));
                     if new_ai.hp <= 0 then raise GameOverOpp else
                     {st with players = (p,new_ai)})
            | Mana -> (let new_ai = {ai with mana = ai.mana + sp.mag} in
                      print_endline ("AI boosted its mana! Now AI's mana is " ^
                                    (string_of_int new_ai.mana));
                      {st with players = (p,new_ai)}))
    | Them -> (match sp.effect with
              | Heal -> (let new_p = {p with hp = p.hp + sp.mag} in
                        print_endline ("AI healed you! Now your health is " ^
                                      (string_of_int new_p.hp));
                        {st with players = (new_p,ai)})
              | Dmg -> (let new_p = if p.armor=0 then {p with hp = p.hp-sp.mag}
                                    else if p.armor-sp.mag>=0 then
                                      {p with armor = p.armor-sp.mag}
                                    else let new_att = sp.mag - p.armor in
                                      {p with armor=0; hp= p.hp-new_att}
                                    in
                       print_endline ("AI has hurt you! Now your health is " ^
                                      (string_of_int new_p.hp));
                       if new_p.hp <= 0 then raise GameOver else
                       {st with players = (new_p,ai)})
              | Mana -> (let new_p = {p with mana = p.mana + sp.mag} in
                        print_endline ("AI boosted your mana! Now your mana is " ^
                                      (string_of_int new_p.mana));
                        {st with players = (new_p,ai)}))
    | Mine -> (match sp.effect with
              | Heal -> (if ai.minions=[] then
                           (print_endline "AI has no minions in play to heal";
                           st)
                        else (
                        let mins_dmg = List.map
                                       (fun c -> match c.cat with
                                                 | Minion m -> (m.attack,c)
                                                 | _ -> failwith "Error")
                                       ai.minions
                                       in
                        let strongest_min = (List.sort compare mins_dmg) |>
                                            List.rev |> List.hd |> snd in
                        let m = match strongest_min.cat with Minion min -> min
                                | _ -> failwith "Error" in
                        let new_m = {m with hp = m.hp + sp.mag} in
                        let new_c = {strongest_min with cat = Minion new_m} in
                        print_endline ("AI healed its minion " ^ new_c.name);
                        let new_mins = List.filter (fun card -> card <> strongest_min)
                                       ai.minions in
                        let new_ai = {ai with minions = new_c::new_mins} in
                        {st with players = (p,new_ai)}))
              | Dmg -> (if ai.minions=[] then
                          (print_endline "AI has no minions in play to attack";
                          st)
                       else (
                       let c = List.hd ai.minions in
                       let m = match c.cat with Minion min -> min
                               | _ -> failwith "Error" in
                       let new_m = {m with hp = m.hp - sp.mag} in
                       let new_c = {c with cat = Minion new_m} in
                       print_endline ("AI hurt its own minion " ^ new_c.name);
                       let new_mins = List.filter (fun card -> card <> c)
                                      ai.minions in
                       let new_ai =
                         if new_m.hp<=0 then (print_endline
                          ("AI has slain its minion " ^ new_c.name);
                          {ai with minions=new_mins})
                         else {ai with minions = new_c::new_mins} in
                       {st with players = (p,new_ai)}))
              | Mana -> print_endline "No mana effect on minions"; st)
    | Theirs -> (match sp.effect with
                | Heal -> (if p.minions=[] then
                             (print_endline "You have no minions in play for AI to heal";
                             st)
                          else (
                          let c = List.hd p.minions in
                          let m = match c.cat with Minion min -> min
                                  | _ -> failwith "Error" in
                          let new_m = {m with hp = m.hp + sp.mag} in
                          let new_c = {c with cat = Minion new_m} in
                          print_endline ("AI healed your minion " ^ new_c.name);
                          let new_mins = List.filter (fun card -> card <> c)
                                         p.minions in
                          let new_p = {p with minions = new_c::new_mins} in
                          {st with players = (new_p,ai)}))
                | Dmg -> (if p.minions=[] then
                            (print_endline "You have no minions in play for AI to attack";
                            st)
                         else (
                         let mins_dmg = List.map
                                       (fun c -> match c.cat with
                                                 | Minion m -> (m.attack,c)
                                                 | _ -> failwith "Error")
                                       p.minions in
                         let cankill = List.filter
                                       (fun (_,c) -> match c.cat with
                                                     | Minion m -> m.hp<=sp.mag
                                                     | _ -> failwith "Error")
                                       mins_dmg in
                         let c = if cankill=[] then (List.sort compare mins_dmg)
                                   |> List.rev |> List.hd |> snd
                                 else (List.sort compare cankill) |> List.rev |>
                                   List.hd |> snd
                                 in
                         let m = match c.cat with Minion min -> min
                                 | _ -> failwith "Error" in
                         let new_m = {m with hp = m.hp - sp.mag} in
                         let new_c = {c with cat = Minion new_m} in
                         print_endline ("AI hurt your minion " ^ new_c.name);
                         let new_mins = List.filter (fun card -> card <> c)
                                        p.minions in
                         let new_p =
                           if new_m.hp<=0 then (print_endline
                             ("AI has slain your minion " ^ new_c.name);
                             {p with minions = new_mins})
                           else
                           {p with minions = new_c::new_mins} in
                         {st with players = (new_p,ai)}))
                | Mana -> print_endline "No mana effect on minions"; st)
    | Any ->  (match sp.effect with
              | Heal -> (if ai.minions=[] then play_spell st {sp with target = Me}
                        else (
                        if ai.hp <= 5 then play_spell st {sp with target = Me}
                        else if ai.hp <= 15 then
                          (let n = (Random.int 3) in
                          if n<2 then play_spell st {sp with target = Me}
                          else play_spell st {sp with target = Mine})
                        else play_spell st {sp with target = Mine})
                        )
              | Dmg -> (if (can_kill st {sp with target = Them}) then
                           play_spell st {sp with target = Them}
                       else if p.minions = [] then
                               play_spell st {sp with target = Them}
                       else if (can_kill st {sp with target = Theirs}) then
                               play_spell st {sp with target = Theirs}
                       else (let n = (Random.int 3) in
                            if n<2 then play_spell st {sp with target = Theirs}
                            else play_spell st {sp with target = Them}
                            )
                       )
              | Mana -> (play_spell st {sp with target = Me})
             )

  (* [play_card st hand] returns the state after the AI plays cards in its hand
   * [hand] in state [st]. *)
  let rec play_card st hand =
    if st.first_player then failwith "Error"
    else
    let ai = snd st.players in
    match hand with
    | [] -> (Unix.sleep(2); print_endline "AI is done playing cards"; st)
    | card::t -> (match card.cat with
              | Spell sp -> (if ai.mana<card.cost then play_card st t
                            else
                            let new_mana = ai.mana - card.cost in
                            let new_hand = List.filter (fun c -> c <> card) ai.hand in
                            let new_ai = {ai with mana =new_mana; hand = new_hand} in
                            print_endline ("AI used the spell " ^ card.name);
                            let new_st = play_spell {st with
                              players=(fst st.players, new_ai)} sp in
                            Unix.sleep(2);
                            play_card new_st t
                            )
              | Minion m -> (if ai.mana<card.cost then play_card st t
                            else
                            let new_mana = ai.mana - card.cost in
                            let new_hand = List.filter (fun c -> c <> card) ai.hand in
                            let new_mins = card::ai.minions in
                            let new_ai = {ai with mana =new_mana;
                              hand = new_hand; minions = new_mins} in
                            print_endline ("AI played the minion " ^ card.name);
                            let new_st = {st with players=(fst st.players, new_ai)} in
                            Unix.sleep(2);
                            play_card new_st t
                            )
              | Weapon wp -> (if ai.mana<card.cost then play_card st t
                             else
                             let new_mana = ai.mana - card.cost in
                             let new_hand = List.filter (fun c -> c<>card) ai.hand in
                             let new_weap = Some card in
                             let new_ai = {ai with mana=new_mana; hand=new_hand;
                               weap = new_weap} in
                             print_endline ("AI has equipped the weapon "
                               ^ card.name);
                             let new_st = {st with players=(fst st.players, new_ai)} in
                             Unix.sleep(2);
                             play_card new_st t
                             )
              )


  let pre_phase st =
    if st.first_player then failwith "Error"
    else
    let pre_st = start_turn st in
    print_state {pre_st with first_player = true};
    let ai = snd st.players in
    print_endline "AI is thinking...";
    Unix.sleep(5);
    play_card pre_st ai.hand

  let attack_phase st =
    let player = ref (snd st.players) in
    let opp = ref (fst st.players) in
    let hero_attack = match (!player).weap with
                      | None -> ref 0
                      | Some c -> match c.cat with
                                  | Weapon w -> ref w.dmg
                                  | _ -> failwith "Error" in
    let get_bonus c =
      match c.cat with
      | Minion c -> c.bonus
      | _ -> failwith "Error" in
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
      {st with players = (!opp, !player)} in
    let theirs = ref (!opp).minions in
    let format_minion c =
      match c.cat with
      | Minion m -> ((string_of_int m.attack) ^ "/" ^ (string_of_int m.hp) ^ " "
                      ^ c.name)
      | _ -> failwith "Error" in
    let get_min_health c =
      match c.cat with Minion m -> m.hp | _ -> failwith "Error" in
    let get_killable ms a =
      (List.filter
        (fun c -> (get_min_health c) <= a) ms) in
    let get_min_attack c =
      match c.cat with Minion m -> m.hp | _ -> failwith "Error" in
    let max_att ms =
      List.fold_left
        (fun a c -> if (get_min_attack a) < (get_min_attack c) then c
                      else a) (List.hd ms) ms in
    let attack_a_minion () = (Random.int 3) < 2 in
    let rec target ms =
      let get_target a =
        let rec attack_hero () =
          let new_armor = (!opp).armor - a in
          print_endline "P1";
          Unix.sleep(2);
          if(new_armor <= 0) then
           (let new_hp = (!opp).hp + new_armor in
            opp := {!opp with armor = 0; hp = new_hp};
            print_endline ("Your Armor: 0");
            print_endline ("Your HP: " ^(string_of_int (!opp).hp));
            if (new_hp <= 0) then raise GameOver else ();
           )
          else(
            opp := {!opp with armor = new_armor};
            print_endline ("Your Armor: " ^(string_of_int (!opp).armor));
            print_endline ("Your HP: " ^(string_of_int (!opp).hp));
            ) in
        let rec attack_minion m ts =
          match ts with
          | [] -> []
          | h::t ->(if(h = m) then
                     (let new_health = (get_min_health h) - a in
                      let new_cat = match h.cat with
                                    | Minion c -> Minion({c with hp = new_health})
                                    | _ -> h.cat in
                      let new_min = {h with cat = new_cat} in
                      print_endline (format_minion h);
                      Unix.sleep(2);
                      if(new_health <= 0) then
                       (print_endline ("Your "^(format_minion h)^" has been slain"); t)
                      else (print_endline ("Your "^(format_minion h)^" is now a "
                              ^(format_minion new_min)); new_min::t))
                    else(h::(attack_minion m t))) in
        if (((!opp).hp + (!opp).armor) <= a) then
          (attack_hero ())
        else (
          match (get_killable (!theirs) a) with
          | [] -> if (attack_a_minion ()) && (!theirs <> []) then
                    theirs := (attack_minion (max_att !theirs) (!theirs))
                  else
                    attack_hero ()
          | l -> (let new_mins = attack_minion (max_att l) (!theirs) in
                  theirs := new_mins)) in
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
                                    Unix.sleep(2);
                                    player := {!player with weap = Some new_c})
                    | _ -> failwith "Error" ) in
      Unix.sleep(2);
      match ms with
      | [] -> if(!hero_attack <> 0) then
               (print_endline "AI: Picking hero's target..."; Unix.sleep(2);
                print_string "Target: ";
                get_target (!hero_attack); alter_weap (); )
              else ();
      | h::t ->(print_endline ("AI: Picking " ^ (format_minion h) ^ "'s target...");
                Unix.sleep(2);
                print_string "Target: ";
                match h.cat with
                | Minion c -> get_target c.attack; target t;
                |  _ -> failwith "Error"; );
      opp := {!opp with minions = !theirs} in
    let end_state s =
      target (!player).minions;
      {s with players = (!opp, !player)} in
    let start_attack s =
      let a = string_of_int (!hero_attack) in
      ignore (Sys.command "clear");
      print_state ({s with first_player = true});
      print_endline ("AI hero has " ^ a ^ " attack"); s in
    new_state |> start_attack |> end_state

  let post_phase st =
    if st.first_player then failwith "Error" else
    let ai = snd st.players in
    if (ai.weap = None && ai.hand = [] && ai.deck = [] &&
      ai.minions = []) then raise GameOverOpp
    else (print_endline "AI is ending its turn";
         Unix.sleep(3); end_turn st)
end

