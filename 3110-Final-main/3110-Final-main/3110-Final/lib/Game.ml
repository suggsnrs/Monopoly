open Graphics
open Player
open Csv

(** Returns the number of the square such that coordinates [x] and [y] lie inside of it. 
    Requires [x] and [y] to be inside one of the squares (but not on the edge)*)
let coord_to_num x y =
  let sq_size = 70 in
  let margin = 50 in
  let board_side = 11 in
  let w = ((x - margin) / sq_size) + 1 in
  let h = ((y - margin) / sq_size) + 1 in
  if h = 1 then board_side - w + 1
  else if h = board_side then (board_side * 2) - 2 + w
  else if w = 1 then board_side - 1 + h
  else (board_side * 4) - 2 - h

(** Returns true if the coordinates are within some of the board squares*)
let is_within_board_sq x y =
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_size = 70 in
  let margin = 50 in
  if
    x < margin
    || x > board_size_x - margin
    || y < margin
    || y > board_size_y - margin
  then false
  else if
    x > margin + sq_size
    && x < board_size_x - margin - sq_size
    && y > margin + sq_size
    && y < board_size_y - margin - sq_size
  then false
  else true

(** [is_prop prop_array num] returns true if the square numbered [num] is a prop square*)
let is_prop_sq prop_arr num =
  let prop_list = Array.to_list prop_arr in
  let rec is_pr_lst pr_lst num =
    match pr_lst with
    | [] -> false
    | h :: t -> if PropSquare.position h = num then true else is_pr_lst t num
  in
  is_pr_lst prop_list num

(**Check whether coordinates [x] [y] are within some property square*)
let is_click_prop x y prop_arr =
  if is_within_board_sq x y = false then false
  else
    let sq_num = coord_to_num x y in
    if is_prop_sq prop_arr sq_num = false then false else true

(** Return property square with given number from squares array.
    Requires square with given number is in the array*)
let get_prop_sq prop_arr num =
  let prop_lst = Array.to_list prop_arr in
  let rec get prop_list num =
    match prop_list with
    | [] -> PropSquare.empty
    | h :: t -> if PropSquare.position h = num then h else get t num
  in
  get prop_lst num

(** Return special square with given number from squares array.
    Requires square with given number is in the array*)
let get_spec_sq spec_arr num =
  let spec_lst = Array.to_list spec_arr in
  let rec get spec_list num =
    match spec_list with
    | [] -> SpecialSquare.empty
    | h :: t -> if SpecialSquare.position h = num then h else get t num
  in
  get spec_lst num

(** Draw the given informational message [message] in the center of the board*)
let draw_info message =
  let board = 870 in
  let margin = 50 in
  let small_sq = 70 in
  let inner_sq = board - (2 * margin) - (2 * small_sq) in
  let text_width, _ = text_size message in
  let x = margin + small_sq + ((inner_sq - text_width) / 2) in
  let y = (870 / 2) + 200 in
  set_color black;
  moveto x y;
  draw_string message

(** Erase the given informational message [message] from the center of the board*)
let erase_info message =
  let board = 870 in
  let margin = 50 in
  let small_sq = 70 in
  let inner_sq = board - (2 * margin) - (2 * small_sq) in
  let text_width, text_height = text_size message in
  let x = margin + small_sq + ((inner_sq - text_width) / 2) in
  let y = (870 / 2) + 200 in
  set_color white;
  fill_rect x y (text_width + 1) (text_height + 1)

(**Draw the given informational message [message] in the center of the board 
    and erase it after [time] seconds*)
let draw_erase_info message time =
  draw_info message;
  Unix.sleep time;
  erase_info message

(** Return list of remaining players*)
let remaining_players pl_array =
  let ans = ref [] in
  for i = 0 to Array.length pl_array - 1 do
    if pl_array.(i) <> empty then ans := pl_array.(i) :: !ans else ()
  done;
  !ans

(** Check whether there is only one remaining player and if yes display info 
    message and exit the game*)
let check_end_game pl_array =
  let remain_players = remaining_players pl_array in
  if List.length remain_players > 1 then ()
  else
    let winner = List.nth remain_players 0 in
    let winner_num = number winner in
    let info_message = "Player " ^ string_of_int winner_num ^ " won:)" in
    draw_erase_info info_message 3;
    exit 0

(*Check whether a player stepped over GO! square and, if yes, give him a salary*)
let stepped_over_go prev_pos cur_pos player =
  if prev_pos > cur_pos then (
    Player.incr_money Player.go_money player;
    let info_message =
      "Player "
      ^ string_of_int (Player.number player)
      ^ " received "
      ^ string_of_int Player.go_money
      ^ " salary."
    in
    (*Draw info message for 4 seconds*)
    draw_erase_info info_message 4)
  else ()

(** Redraw numbers on the dice*)
let show_dice_roll x y roll1 roll2 =
  (* Assuming a clear area to avoid overlapping of numbers *)
  set_color white;
  fill_rect x y 50 20;
  (* Clear the previous roll; adjust size as needed *)
  set_color black;
  (* Display new roll to the right of the "Dice" button, on the same level *)
  moveto x y;
  draw_string (string_of_int roll1 ^ " " ^ string_of_int roll2)

(**Roll dice once, move correct player, and return its position before the move*)
let roll_dice dice_b pl_array i =
  Random.self_init ();
  let roll1 = Random.int 6 + 1 in
  let roll2 = Random.int 6 + 1 in
  let dice_text_pos = Button.draw_dice dice_b in
  show_dice_roll (fst dice_text_pos) (snd dice_text_pos) roll1 roll2;
  (* Move correct player and update number of player who has to move next*)
  let prev_pos = move_players_options pl_array i roll1 roll2 in
  prev_pos

(** Draw rectangle*)
let draw_rect x y width height =
  moveto x y;
  lineto x (y + height);
  lineto (x + width) (y + height);
  lineto (x + width) y;
  lineto x y

(** Drawing a single player's property*)
let player_draw_property_list p x y =
  if List.length (Player.current_property p) > 0 then
    for i = 1 to List.length (Player.current_property p) do
      set_color black;
      moveto x (y - 160);
      draw_string "Properties: ";
      moveto x (y - 160 - (i * 20));
      set_color
        (PropSquare.color (List.nth (Player.current_property p) (i - 1)));
      fill_rect x (y - 160 - (i * 20)) 20 10;
      set_color black;
      draw_rect x (y - 160 - (i * 20)) 20 10;
      moveto (x + 25) (y - 160 - (i * 20));
      draw_string
        (PropSquare.name (List.nth (Player.current_property p) (i - 1)))
    done
  else (
    moveto x (y - 160);
    draw_string "Properties: ")

(** Draw all players information.*)
let draw_helper_one board_width pl_arr loc_y =
  for i = 1 to Array.length pl_arr do
    let x = board_width + 30 + ((i - 1) * 120) in
    set_color black;
    moveto x (loc_y - 80);
    draw_string ("Player " ^ string_of_int i)
  done

(** Draw all players information.*)
let draw_helper_two board_width pl_arr loc_y =
  for i = 1 to Array.length pl_arr do
    let x = board_width + 30 + ((i - 1) * 120) in
    if Player.is_empty pl_arr.(i - 1) then (
      set_color white;
      moveto x (loc_y - 30);
      fill_rect x (loc_y - 800) 110 800)
    else (
      set_color black;
      moveto x (loc_y - 120);
      draw_string
        ("Money: " ^ string_of_int (Player.current_money pl_arr.(i - 1)));
      player_draw_property_list pl_arr.(i - 1) x loc_y)
  done

let draw_players_info pl_arr pl_num =
  let board_width = 870 in
  let board_height = 870 in
  let loc_x = board_width + 30 + ((pl_num - 1) * 120) in
  let loc_y = board_height in
  set_color white;
  moveto (board_width + 30) (loc_y - 30);
  fill_rect (board_width + 30) (loc_y - 800) 500 800;
  set_color yellow;
  moveto loc_x (loc_y - 80);
  fill_rect loc_x (loc_y - 80) 80 20;
  draw_helper_one board_width pl_arr loc_y;
  draw_helper_two board_width pl_arr loc_y

(*Storing center money*)
let center_money = ref 0

(** Increment the center money*)
let incr_center_money inc =
  let num = !center_money in
  center_money := num + inc

(** Decrement the center money*)
let decr_center_money dec =
  let num = !center_money in
  center_money := num - dec

(** Redraw center money*)
let draw_center_money value =
  let x = 870 / 2 in
  let y = (870 / 2) - 20 in
  set_color green;
  fill_rect (x - 100) (y + 120) 200 10;
  set_color black;
  moveto (x - 100) (y + 120);
  draw_string ("Center Money: $" ^ string_of_int value)

let generate_random_message_helper result win_list lose_list index =
  match result with
  | "win" -> List.nth win_list index
  | "lose" -> List.nth lose_list index
  | _ -> ""

(** Given a type of card [result] choose a card of the corresponding type at random*)
let generate_random_message result =
  let win_list =
    [
      "You won the lottery: $";
      "Income Tax refund: $";
      "From sale of stock you get $";
      "Life insurance matures. Collect $";
    ]
  in
  let lose_list =
    [
      "Speeding ticket, please pay $";
      "Pay hospital fees of $";
      "Pay school fees of $";
      "Pay doctor's fee of $";
    ]
  in
  let index = Random.int (List.length win_list) (* Generate a random index *) in
  generate_random_message_helper result win_list lose_list index

(** Convert data from a csv file [file] into an array of tuples, each of which 
    stores the category of the card and the action item.*)
let get_card_data file =
  (* Load the CSV file *)
  let csv_data = load file in
  (* Skip the header and process rows *)
  match csv_data with
  | [] -> [] (* Return an empty list if the file is empty or just headers *)
  | _header :: rows ->
      List.map
        (function
          | [ category; action ] -> (category, action)
          | _ ->
              failwith
                "Unexpected CSV format: Each row must have exactly two columns.")
        rows

(*Change the second element of tuple to position*)
let chance_modified_chosen card_chosen =
  match snd card_chosen with
  | "GO" -> (fst card_chosen, "1")
  | "Taipei" -> (fst card_chosen, "4")
  | "Beijing" -> (fst card_chosen, "27")
  | "MonopAir" -> (fst card_chosen, "16")
  | "MonopRail" -> (fst card_chosen, "6")
  | "Parking" -> (fst card_chosen, "21")
  | _ -> card_chosen

let message_helper card_chosen =
  match fst card_chosen with
  | "move" -> "Chance: you are moving to " ^ snd card_chosen ^ "!"
  | "lose" ->
      "Chance: " ^ generate_random_message "lose" ^ snd card_chosen ^ "!"
  | "win" -> "Chance: " ^ generate_random_message "win" ^ snd card_chosen ^ "!"
  | _ -> ""

(** Function to get a random chance card from "chance.csv" *)
let chance () : string * string =
  Random.self_init ();
  let card_list = get_card_data "chance.csv" in
  (* Get the total number of cards loaded *)
  let num_cards = List.length card_list in
  if num_cards = 0 then failwith "No cards available in the CSV file."
  else
    (* Generate a random index *)
    let index = Random.int num_cards in
    (* Select the card at the random index *)
    let card_chosen = List.nth card_list index in
    let message = message_helper card_chosen in
    draw_erase_info message 5;
    chance_modified_chosen card_chosen (* Return the chosen card *)

(*Change the second element of tuple to position*)
let chest_modified_chosen card_chosen =
  match snd card_chosen with
  | "GO" -> (fst card_chosen, "1")
  | "Jail" -> (fst card_chosen, "11")
  | _ -> card_chosen

(* Function to get a random community chest card from "chest.csv" *)
let chest () =
  Random.self_init ();
  let chest_list = get_card_data "chest.csv" in
  (* Get the total number of cards loaded *)
  let num_chest = List.length chest_list in
  (* Generate a random index *)
  let index = Random.int num_chest in
  (* Select the card at the random index *)
  let card_chosen = List.nth chest_list index in
  let message =
    match fst card_chosen with
    | "move" -> "Chest: you are moving to " ^ snd card_chosen ^ "!"
    | "lose" ->
        "Chest: " ^ generate_random_message "lose" ^ snd card_chosen ^ "!"
    | "win" -> "Chest: " ^ generate_random_message "win" ^ snd card_chosen ^ "!"
    | _ -> ""
  in
  draw_erase_info message 5;
  (* Return the chosen card *)
  chest_modified_chosen card_chosen

(** Wait until some property square is clicked on and return the number of clicked square*)
let rec wait_prop_click prop_arr =
  let status = wait_next_event [ Button_down ] in
  let x = status.mouse_x in
  let y = status.mouse_y in
  if is_click_prop x y prop_arr = false then wait_prop_click prop_arr
  else
    let sq_num = coord_to_num x y in
    sq_num

(** Wait until a key with number of existing player is pressed and return that number*)
let rec wait_keybord_pl_num pl_array =
  let status = wait_next_event [ Key_pressed ] in
  let key_pressed = status.key in
  match key_pressed with
  | '1' -> if pl_array.(0) <> empty then 1 else wait_keybord_pl_num pl_array
  | '2' -> if pl_array.(1) <> empty then 2 else wait_keybord_pl_num pl_array
  | '3' -> if pl_array.(2) <> empty then 3 else wait_keybord_pl_num pl_array
  | '4' -> if pl_array.(3) <> empty then 4 else wait_keybord_pl_num pl_array
  | _ -> wait_keybord_pl_num pl_array

(** Game logic for [player] to pay taxes in the amount [amount]*)
let pay_tax pl_arr player amount =
  let info_message =
    "Player "
    ^ (string_of_int (Player.number player) ^ " has to pay ")
    ^ string_of_int amount ^ " tax."
  in
  draw_erase_info info_message 4;
  let actually_paid = Player.decr_money amount player pl_arr in
  incr_center_money actually_paid;
  draw_center_money !center_money

(** Game logic when [player] steps on parking*)
let step_on_parking player =
  let amount_paid = !center_money in
  Player.incr_money amount_paid player;
  decr_center_money amount_paid;
  draw_center_money !center_money;
  let info_message =
    "Player "
    ^ (string_of_int (Player.number player) ^ " received ")
    ^ string_of_int amount_paid ^ " for parking."
  in
  draw_erase_info info_message 4
(*Anytime someone pays a fee (Jail, Income, Luxury)
  that money gets put in the center of the board, and the person landing
  on parking gets that money.*)

(** Game logic when player steps on property owned by another player*)
let step_on_owned_prop pl_arr player property =
  if
    Player.is_empty (Array.get pl_arr (PropSquare.owner property - 1))
    || Player.is_empty player
  then ()
  else
    let rent = PropSquare.rent property in
    let info_message =
      "Player "
      ^ string_of_int (Player.number player)
      ^ " has to pay " ^ string_of_int rent ^ " rent."
    in
    (*Draw info message for 4 seconds*)
    draw_erase_info info_message 4;
    let actually_paid = Player.decr_money rent player pl_arr in
    Player.incr_money actually_paid
      (Array.get pl_arr (PropSquare.owner property - 1))

let info_helper player =
  "Player "
  ^ string_of_int (Player.number player)
  ^ " received "
  ^ string_of_int Player.go_money
  ^ " salary."

(** Game logic when [player] gets a card from category "move"*)
let move_card player new_pos prop_array pl_arr =
  Player.change_position player new_pos;
  if is_prop_sq prop_array new_pos = true (*moved to property*) then
    let prop = get_prop_sq prop_array new_pos in
    if
      PropSquare.owner prop <> 0
      && PropSquare.owner prop
         <> Player.number player (*check whether player has to pay rent*)
    then step_on_owned_prop pl_arr player prop
    else ()
  else if new_pos = 1 (*give salary if moved on GO*) then (
    Player.incr_money Player.go_money player;
    let info_message = info_helper player in
    draw_erase_info info_message 4)
  else if new_pos = 21 then
    (*give center money if moved on parking*)
    step_on_parking player
  else ()

(** Game logic when [player] steps on chest square*)
let step_on_chest pl_arr player prop_array =
  let chest = chest () in
  match fst chest with
  | "move" ->
      let new_pos = int_of_string (snd chest) in
      move_card player new_pos prop_array pl_arr
  | "lose" ->
      let _ = Player.decr_money (int_of_string (snd chest)) player pl_arr in
      ()
  | "win" -> Player.incr_money (int_of_string (snd chest)) player
  | _ -> Player.incr_money 0 player

(** Game logic when [player] steps on chance square*)
let step_on_chance pl_arr player prop_array =
  let card = chance () in
  match fst card with
  | "move" ->
      let new_pos = int_of_string (snd card) in
      move_card player new_pos prop_array pl_arr
  | "lose" ->
      let _ = Player.decr_money (int_of_string (snd card)) player pl_arr in
      ()
  | "win" -> Player.incr_money (int_of_string (snd card)) player
  | _ -> Player.incr_money 0 player

(**Game logic when player steps on a special square*)
let step_on_spec_sq pl_arr player spec_sq prop_array =
  match SpecialSquare.name spec_sq with
  | "Chest" -> step_on_chest pl_arr player prop_array
  | "IncomeTax" -> pay_tax pl_arr player 2000
  | "Chance" -> step_on_chance pl_arr player prop_array
  | "Jail" -> () (*Note: if you land on Jail, nothing happens.*)
  | "Parking" -> step_on_parking player
  | "Go to Jail" ->
      let info_message =
        "Player " ^ string_of_int (Player.number player) ^ " goes to jail!"
      in
      (*Draw info message for 4 seconds*)
      draw_erase_info info_message 4;
      Player.change_position player 11
  | "LuxuryTax" -> pay_tax pl_arr player 2000
  | _ -> ()

(* True when dice can be pressed*)
let dice_key = ref true

let rec game_logic button_list pl_arr prop_arr spec_arr i dice_key =
  let status = wait_next_event [ Button_down ] in
  let x = status.mouse_x in
  let y = status.mouse_y in
  match !dice_key with
  (*Allowed to press only dice*)
  | true ->
      if Button.is_within_bounds x y (List.nth button_list 0) = false then
        (*clicked outside dice -> wait for click again with dice still open*)
        game_logic button_list pl_arr prop_arr spec_arr i dice_key
      else
        (*dice was clicked -> lock dice*)
        pressed_dice button_list pl_arr prop_arr spec_arr i dice_key
  | false (*Allowed to press everything but dice*) ->
      pressed_button x y button_list pl_arr prop_arr spec_arr i dice_key

and pressed_dice button_list pl_arr prop_arr spec_arr i dice_key =
  let dice_b = List.nth button_list 0 in
  dice_key := false;
  (*roll dice once and record position before move*)
  let prev_pos = roll_dice dice_b pl_arr i in
  let new_pos = Player.current_pos pl_arr.(!i - 1) in
  let current_player = pl_arr.(!i - 1) in
  (*Give player salary if he stepped over GO!*)
  stepped_over_go prev_pos new_pos current_player;
  let current_player = pl_arr.(!i - 1) in
  draw_players_info pl_arr (Player.number current_player);
  if is_prop_sq prop_arr new_pos = false (*stepped on special square*) then (
    step_on_spec_sq pl_arr current_player
      (get_spec_sq spec_arr new_pos)
      prop_arr;
    draw_players_info pl_arr (Player.number current_player);
    (*Check for winner*)
    check_end_game pl_arr;
    game_logic button_list pl_arr prop_arr spec_arr i dice_key
    (*wait for next click with dice still locked*))
  else
    (*stepped on property square*)
    let prop_sq = get_prop_sq prop_arr new_pos in
    let pl_num = Player.number current_player in
    if
      PropSquare.owner prop_sq <> 0 && PropSquare.owner prop_sq <> pl_num
      (*property has another owner*)
    then (
      step_on_owned_prop pl_arr current_player prop_sq;
      draw_players_info pl_arr (Player.number current_player);
      (*Check for winner*)
      check_end_game pl_arr;
      game_logic button_list pl_arr prop_arr spec_arr i dice_key)
    else
      (*stepped on unowned/your property -> wait for next click with dice locked*)
      game_logic button_list pl_arr prop_arr spec_arr i dice_key

and pressed_button x y button_list pl_arr prop_arr spec_arr i dice_key =
  if Button.is_within_bounds x y (List.nth button_list 1) (*clicked next*) then (
    dice_key := true;
    (*open dice lock*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key
    (*wait for next click with dice open*))
  else if
    Button.is_within_bounds x y (List.nth button_list 2) (*clicked buy prop*)
  then clicked_buy_prop button_list pl_arr prop_arr spec_arr i dice_key
  else if
    Button.is_within_bounds x y (List.nth button_list 3)
    (*clicked sell property*)
  then clicked_sell_prop button_list pl_arr prop_arr spec_arr i dice_key
  else if Button.is_within_bounds x y (List.nth button_list 4) (*clicked trade*)
  then clicked_trade button_list pl_arr prop_arr spec_arr i dice_key
  else if
    Button.is_within_bounds x y (List.nth button_list 5) (*clicked buy house*)
  then clicked_buy_house button_list pl_arr prop_arr spec_arr i dice_key
  else if
    Button.is_within_bounds x y (List.nth button_list 6) (*clicked buy hotel*)
  then clicked_buy_hotel button_list pl_arr prop_arr spec_arr i dice_key
  else if
    Button.is_within_bounds x y (List.nth button_list 7) (*clicked sell house*)
  then clicked_sell_house button_list pl_arr prop_arr spec_arr i dice_key
  else if
    Button.is_within_bounds x y (List.nth button_list 8) (*clicked sell hotel*)
  then clicked_sell_hotel button_list pl_arr prop_arr spec_arr i dice_key
  else
    (*Didn't click on any of allowed buttons -> wait for next click with dice locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key

and clicked_buy_prop button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let curr_pos = Player.current_pos current_player in
  if is_prop_sq prop_arr curr_pos = false (*square is not property*) then (
    let info_message = "Not a property." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i
      dice_key (*wait for next click with dice still locked*))
  else
    let prop = get_prop_sq prop_arr curr_pos in
    let current_player = pl_arr.(!i - 1) in
    if
      PropSquare.owner prop = 0
      && Player.current_money current_player >= PropSquare.price prop
    then (
      Player.buy_property current_player prop pl_arr;
      draw_players_info pl_arr (Player.number current_player);
      game_logic button_list pl_arr prop_arr spec_arr i dice_key)
    else (
      draw_players_info pl_arr (Player.number current_player);
      let info_message = "Purchase unsuccessful." in
      draw_erase_info info_message 3;
      (*wait for next click with dice still locked*)
      game_logic button_list pl_arr prop_arr spec_arr i dice_key)

and clicked_sell_prop button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message =
    "Choose a property that you would like to sell by clicking the property \
     square on the board."
  in
  draw_info info_message;
  let prop_num =
    wait_prop_click prop_arr (*num of property on which player clicked*)
  in
  erase_info info_message;
  let prop = get_prop_sq prop_arr prop_num in
  if PropSquare.owner prop = !i then (
    Player.sell_property current_player prop;
    draw_players_info pl_arr (Player.number current_player);
    game_logic button_list pl_arr prop_arr spec_arr i dice_key)
  else (
    draw_players_info pl_arr (Player.number current_player);
    let info_message = "Selling unsuccessful." in
    draw_erase_info info_message 3;
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key)

and clicked_trade button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message = "Type the number of the player you want to trade with." in
  draw_info info_message;
  let resp_id = wait_keybord_pl_num pl_arr in
  (*number of player with whom to trade*)
  erase_info info_message;
  let info_message = "Select the CURRENT player's property from the board." in
  draw_info info_message;
  let traded_prop_num_curr = wait_prop_click prop_arr in
  (*number of property square that is offered to trade (owned by the current player)*)
  erase_info info_message;
  let info_message = "Select the OTHER player's property from the board." in
  draw_info info_message;
  let traded_prop_num_other = wait_prop_click prop_arr in
  erase_info info_message;
  draw_players_info pl_arr (Player.number current_player);
  let traded_prop_curr = get_prop_sq prop_arr traded_prop_num_curr in
  let traded_prop_other = get_prop_sq prop_arr traded_prop_num_other in
  let price = 0 in
  let offer =
    Transaction.create_offer ~trade_player_id:resp_id
      ~curr_pl_offered_properties:[ traded_prop_curr ]
      ~trade_pl_offered_properties:[ traded_prop_other ] ~offered_money:price
      ~current_player_id:(Player.number current_player)
  in
  let is_succesful =
    Transaction.execute_transaction offer ~trade_pl_id:resp_id
      ~curr_pl_id:(Player.number current_player)
      pl_arr
  in
  if is_succesful then (
    draw_players_info pl_arr (Player.number current_player);
    let info_message = "Trade was successful." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i dice_key)
  else (
    draw_players_info pl_arr (Player.number current_player);
    let info_message = "Trade was unsuccessful." in
    draw_erase_info info_message 3;
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key)

and clicked_buy_house button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message =
    "Choose a property on which to build the house by clicking the property \
     square on the board."
  in
  draw_info info_message;
  let prop_for_house_num =
    wait_prop_click prop_arr (*num of property on which player clicked*)
  in
  erase_info info_message;
  if is_prop_sq prop_arr prop_for_house_num = false (*square is not property*)
  then (
    let info_message = "Not a property." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i
      dice_key (*wait for next click with dice still locked*))
  else
    let prop_for_house = get_prop_sq prop_arr prop_for_house_num in
    Player.buy_house current_player prop_for_house;
    draw_players_info pl_arr (Player.number current_player);
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key

and clicked_buy_hotel button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message =
    "Choose a property on which to build the hotel by clicking the property \
     square on the board."
  in
  draw_info info_message;
  let prop_for_hotel_num =
    wait_prop_click prop_arr (*num of property on which player clicked*)
  in
  erase_info info_message;
  if is_prop_sq prop_arr prop_for_hotel_num = false (*square is not property*)
  then (
    let info_message = "Not a property." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i
      dice_key (*wait for next click with dice still locked*))
  else
    let prop_for_hotel = get_prop_sq prop_arr prop_for_hotel_num in
    Player.buy_hotel current_player prop_for_hotel;
    draw_players_info pl_arr (Player.number current_player);
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key

and clicked_sell_house button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message =
    "Choose a property from which to sell the house by clicking the property \
     square on the board."
  in
  draw_info info_message;
  let prop_with_house_num =
    wait_prop_click prop_arr (*num of property on which player clicked*)
  in
  erase_info info_message;
  if is_prop_sq prop_arr prop_with_house_num = false (*square is not property*)
  then (
    let info_message = "Not a property." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i
      dice_key (*wait for next click with dice still locked*))
  else
    let prop_with_house = get_prop_sq prop_arr prop_with_house_num in
    Player.sell_house current_player prop_with_house;
    draw_players_info pl_arr (Player.number current_player);
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key

and clicked_sell_hotel button_list pl_arr prop_arr spec_arr i dice_key =
  let current_player = pl_arr.(!i - 1) in
  let info_message =
    "Choose a property from which to sell the hotel by clicking the property \
     square on the board."
  in
  draw_info info_message;
  let prop_with_hotel_num =
    wait_prop_click prop_arr (*num of property on which player clicked*)
  in
  erase_info info_message;
  if is_prop_sq prop_arr prop_with_hotel_num = false (*square is not property*)
  then (
    let info_message = "Not a property." in
    draw_erase_info info_message 3;
    game_logic button_list pl_arr prop_arr spec_arr i
      dice_key (*wait for next click with dice still locked*))
  else
    let prop_with_hotel = get_prop_sq prop_arr prop_with_hotel_num in
    Player.sell_hotel current_player prop_with_hotel;
    draw_players_info pl_arr (Player.number current_player);
    (*wait for next click with dice still locked*)
    game_logic button_list pl_arr prop_arr spec_arr i dice_key
