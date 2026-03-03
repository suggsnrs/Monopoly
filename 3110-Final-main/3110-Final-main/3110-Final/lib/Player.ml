open Graphics

type t_aux = {
  pl_num : int;
  sq_num : int ref;
  money : int ref;
  property : PropSquare.t list ref;
}

type t = t_aux option

let init_money = 5000
let go_money = 2000

exception NonexistingPl

let e = NonexistingPl
let (empty : t) = None

let create pl_number sq_number =
  Some
    {
      pl_num = pl_number;
      sq_num = sq_number;
      money = ref init_money;
      property = ref [];
    }

let is_empty (pl : t) = match pl with Some _ -> false | None -> true
let number pl = match pl with None -> raise e | Some pl -> pl.pl_num
let current_pos pl = match pl with None -> raise e | Some pl -> !(pl.sq_num)
let current_money pl = match pl with None -> raise e | Some pl -> !(pl.money)

let current_property pl =
  match pl with None -> raise e | Some pl -> !(pl.property)

(** Draw the given informational message [message] in the center of the board for 
    [time] seconds and erase it afterwards*)
let display_info message time =
  let board = 870 in
  let margin = 50 in
  let small_sq = 70 in
  let inner_sq = board - (2 * margin) - (2 * small_sq) in
  let text_width, text_height = text_size message in
  let x = margin + small_sq + ((inner_sq - text_width) / 2) in
  let y = (870 / 2) + 200 in
  set_color black;
  moveto x y;
  draw_string message;
  Unix.sleep time;
  set_color white;
  fill_rect x y (text_width + 1) (text_height + 1)

(** Returns the list of numbers of property squares owned by [player]*)
let prop_num_list player =
  let prop_list = current_property player in
  let rec prop_num_l prop_list =
    match prop_list with
    | [] -> []
    | h :: t -> PropSquare.position h :: prop_num_l t
  in
  prop_num_l prop_list

let bankrupt pl_array pl =
  let all_money = current_money pl in
  let pl_num = number pl in
  let pl_prop = current_property pl in
  let pl_pos = current_pos pl in
  let prop_numbers = prop_num_list pl in
  for i = 0 to List.length pl_prop - 1 do
    let prop = List.nth pl_prop i in
    PropSquare.remove_all_houses prop;
    PropSquare.remove_hotel prop;
    PropSquare.delete_owner prop
    (*Delete owner of property*)
  done;
  pl_array.(pl_num - 1) <- None (*Deleted player from player array*);
  let info_message = "Player " ^ string_of_int pl_num ^ " lost:(" in
  display_info info_message 4;
  Board.erase_pl pl_num pl_pos prop_numbers;
  all_money

let incr_money amount pl =
  match pl with None -> raise e | Some pl -> pl.money := !(pl.money) + amount

let decr_money amount pl pl_array =
  match pl with
  | None -> raise e
  | Some pl ->
      if !(pl.money) - amount < 0 then
        let all_money = bankrupt pl_array (Some pl) in
        all_money
      else (
        pl.money := !(pl.money) - amount;
        amount)

let change_position pl new_pos =
  match pl with
  | None -> raise e
  | Some pl ->
      Board.remove_player pl.pl_num !(pl.sq_num);
      pl.sq_num := new_pos;
      Board.draw_player pl.pl_num !(pl.sq_num)

let get_player (players : t array) pl_id =
  match players.(pl_id - 1) with None -> raise e | Some pl -> Some pl

(*Move player numbered i. Requires that player numbered i exists*)
let move_players pl_array i roll1 roll2 =
  match pl_array.(!i - 1) with
  | None -> raise e
  | Some _ ->
      (* Change position of the player with number [i]*)
      let new_pos =
        if (current_pos pl_array.(!i - 1) + roll1 + roll2) mod 40 = 0 then 40
        else (current_pos pl_array.(!i - 1) + roll1 + roll2) mod 40
      in
      change_position pl_array.(!i - 1) new_pos

let move_players_options pl_array i roll1 roll2 =
  let num_pl = Array.length pl_array in
  let i1 = if (!i + 1) mod num_pl = 0 then num_pl else (!i + 1) mod num_pl in
  i := i1;
  let i2 = if (!i + 1) mod num_pl = 0 then num_pl else (!i + 1) mod num_pl in
  let i3 = if (!i + 2) mod num_pl = 0 then num_pl else (!i + 2) mod num_pl in
  let i4 = if (!i + 3) mod num_pl = 0 then num_pl else (!i + 3) mod num_pl in
  let _ =
    if pl_array.(!i - 1) <> empty then ()
    else if pl_array.(i2 - 1) <> empty then i := i2
    else if pl_array.(i3 - 1) <> empty then i := i3
    else if pl_array.(i4 - 1) <> empty then i := i4
    else raise e
  in
  let prev_pos = current_pos pl_array.(!i - 1) in
  move_players pl_array i roll1 roll2;
  prev_pos

let buy_property player property pl_arr =
  match player with
  | None -> raise e
  | Some pl ->
      let ctr = ref 0 in
      for i = 0 to Array.length pl_arr - 1 do
        let other_pl = pl_arr.(i) in
        match other_pl with
        | None -> ()
        | Some p -> if List.mem property !(p.property) then ctr := 1 else ()
      done;
      (* Check that player doesn't have it yet*)
      if not (PropSquare.owner property = 0) then ()
      else if
        (*Check that the player has enough cash*)
        PropSquare.price property > !(pl.money)
      then ()
      else (*Change owner*)
        PropSquare.change_owner property pl.pl_num;
      (*Update player's property list*)
      pl.property := property :: !(pl.property);
      pl.money := !(pl.money) - PropSquare.price property (*Charge money*)

let add_property player property pl_arr =
  match player with
  | None -> raise e
  | Some pl ->
      let ctr = ref 0 in
      for i = 0 to Array.length pl_arr - 1 do
        let other_pl = pl_arr.(i) in
        match other_pl with
        | None -> ()
        | Some p -> if List.mem property !(p.property) then ctr := 1 else ()
      done;
      (* Check that player doesn't have it yet*)
      if not (PropSquare.owner property = 0) then ()
      else if
        (*Check that the player has enough cash*)
        PropSquare.price property > !(pl.money)
      then ()
      else (*Change owner*)
        PropSquare.change_owner property pl.pl_num;
      (*Update player's property list*)
      pl.property := property :: !(pl.property)

(** Returns a list of properties of the same colour as [prop] (including itself) 
    among all properties in [same_col_list]*)
let rec same_col_list prop_list prop =
  match prop_list with
  | [] -> []
  | h :: t ->
      if PropSquare.color h = PropSquare.color prop then
        h :: same_col_list t prop
      else same_col_list t prop

(** Returns true if the player [player] owns all properties with the same color 
    as the property[prop]*)
let can_build player prop =
  let same_col_lst = same_col_list (current_property player) prop in
  match List.length same_col_lst with
  | 6 -> true
  | 4 -> if PropSquare.color prop = white then false else true
  | 3 ->
      if PropSquare.color prop = blue || PropSquare.color prop = white then
        false
      else true
  | _ -> false

let buy_house player property =
  match player with
  | None -> raise e
  | Some pl ->
      if PropSquare.owner property <> number player then
        let info_message = "Not your property." in
        display_info info_message 2
      else if can_build player property = false then
        let info_message = "Collection not full." in
        display_info info_message 2
      else if PropSquare.building_cost property > !(pl.money) then
        let info_message = "Not enough money." in
        display_info info_message 2
      else if List.length (PropSquare.houses property) = 4 then
        let info_message = "Buy a hotel." in
        display_info info_message 2
      else
        (*Update player's house on property list they are on*)
        let house = House.create () in
        PropSquare.build_house property house;
        (*Charge money*)
        pl.money := !(pl.money) - PropSquare.building_cost property;
        if PropSquare.num_house property < 2 then
          Board.draw_house 1 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 3 then
          Board.draw_house 2 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 4 then
          Board.draw_house 3 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 5 then
          Board.draw_house 4 (PropSquare.position property) Graphics.green
        else ()

let buy_hotel player property =
  match player with
  | None -> raise e
  | Some pl ->
      if PropSquare.owner property <> number player then
        let info_message = "Not your property." in
        display_info info_message 2
      else if List.length (PropSquare.hotels property) > 0 then
        let info_message = "Cannot have more than 1 hotel." in
        display_info info_message 2
      else if List.length (PropSquare.houses property) <> 4 then
        let info_message = "Need 4 houses first." in
        display_info info_message 2
      else if PropSquare.building_cost property > !(pl.money) then
        let info_message = "Not enough money." in
        display_info info_message 2
      else
        (*Update player's house on property list they are on*)
        let hotel = Hotel.create () in
        PropSquare.build_hotel property hotel;
        (*Charge money*)
        pl.money := !(pl.money) - PropSquare.building_cost property;
        PropSquare.remove_all_houses property;
        Board.draw_hotel 1 (PropSquare.position property) Graphics.red

let sell_house player property =
  match player with
  | None -> raise e
  | Some pl ->
      if
        PropSquare.owner property = number player
        && List.length (PropSquare.houses property) > 0
      then (
        PropSquare.remove_house property;
        pl.money :=
          !(pl.money)
          + PropSquare.building_cost property (*Unsure if this is right[]*);
        Board.erase_house (PropSquare.position property);
        if PropSquare.num_house property < 1 then ()
        else if PropSquare.num_house property < 2 then
          Board.draw_house 1 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 3 then
          Board.draw_house 2 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 4 then
          Board.draw_house 3 (PropSquare.position property) Graphics.green
        else if PropSquare.num_house property < 5 then
          Board.draw_house 4 (PropSquare.position property) Graphics.green
        else ())
      else
        let info_message = "Selling unsuccesful." in
        display_info info_message 2

let sell_hotel player property =
  match player with
  | None -> raise e
  | Some pl ->
      if PropSquare.owner property <> number player then
        let info_message = "Not your property." in
        display_info info_message 2
      else if PropSquare.num_hotel property = 0 then
        let info_message = "There are no hotels to sell." in
        display_info info_message 2
      else PropSquare.remove_hotel property;
      pl.money := !(pl.money) + PropSquare.building_cost property;
      Board.erase_hotel (PropSquare.position property)

let rec chg_lst lst prop =
  match lst with
  | [] -> []
  | h :: t -> if h = prop then t else h :: chg_lst t prop

let sell_property player property =
  match player with
  | None -> raise e
  | Some pl ->
      PropSquare.delete_owner property;
      pl.property := chg_lst !(pl.property) property;
      pl.money := !(pl.money) + PropSquare.mortgage_value property

let delete_property player property =
  match player with
  | None -> raise e
  | Some pl ->
      PropSquare.delete_owner property;
      pl.property := chg_lst !(pl.property) property
