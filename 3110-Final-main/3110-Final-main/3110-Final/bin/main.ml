open Graphics
open Final_3110

let input_err_message =
  "Please enter valid arguments. \n\
   To run the program enter [dune exec bin/main.exe <num of players>, where \n\
   <num of players> is an integer from 2 to 4"

(** Determining number of players*)
let num_players =
  if Array.length Sys.argv <> 2 then (
    print_endline input_err_message;
    exit 0)
  else
    match Sys.argv.(1) with
    | "2" -> 2
    | "3" -> 3
    | "4" -> 4
    | _ ->
        print_endline input_err_message;
        exit 0

(** Initialize the array [pl_array] of [num_players] players in square 1*)
let init_players =
  let pl_array = Array.make num_players Player.empty in
  let pl1 = Player.create 1 (ref 1) in
  Array.set pl_array 0 pl1;
  let pl2 = Player.create 2 (ref 1) in
  Array.set pl_array 1 pl2;
  if num_players > 3 then (
    let pl3 = Player.create 3 (ref 1) in
    let pl4 = Player.create 4 (ref 1) in
    Array.set pl_array 2 pl3;
    Array.set pl_array 3 pl4)
  else if num_players > 2 then
    let pl3 = Player.create 3 (ref 1) in
    Array.set pl_array 2 pl3
  else ();
  pl_array

(** Draw players initially in square 1*)
let draw_players pl_array =
  for i = 1 to Array.length pl_array do
    Board.draw_player i 1
  done

(** Convert data in a csv file [file] into an array of tuples, each of which 
    stores property square position, name, purchase price, and colour (as Graphics 
    type [color])*)
let get_prop_data file =
  let table = Csv.load file in
  let size = List.length table - 1 in
  let tuples = Array.make size (0, "", 0, 0) in
  for row = 1 to size do
    let r = List.nth table row in
    let position = int_of_string (List.nth r 0) in
    let name = List.nth r 1 in
    let price = int_of_string (List.nth r 2) in
    let colour =
      rgb
        (int_of_string (List.nth r 3))
        (int_of_string (List.nth r 4))
        (int_of_string (List.nth r 5))
    in
    tuples.(row - 1) <- (position, name, price, colour)
  done;
  tuples

(** Convert data in a csv file [file] into an array of tuples, each of which 
    stores special square position, name, and text message*)
let get_special_data file =
  let table = Csv.load file in
  let size = List.length table - 1 in
  let tuples = Array.make size (0, "", "") in
  for row = 1 to size do
    let r = List.nth table row in
    let pos = int_of_string (List.nth r 0) in
    let name = List.nth r 1 in
    let message = List.nth r 2 in
    tuples.(row - 1) <- (pos, name, message)
  done;
  tuples

(** Creating property squares with the data stored in  [data] and calling the 
    drawing function for each of them. [data] is the list of tuples containing 
    square number, square name, purchase price, and color (as an int)*)
let init_prop_sq data =
  let prop_sq_array = Array.make (Array.length data) PropSquare.empty in
  for i = 0 to Array.length data - 1 do
    let new_sq = PropSquare.create data.(i) in
    PropSquare.draw new_sq;
    prop_sq_array.(i) <- new_sq
  done;
  prop_sq_array

(** Creating special squares with the data stored in  [data] and calling the 
    drawing function for each of them. [data] is the list of tuples containing 
    square number and square name*)
let init_spec_sq data =
  let spec_sq_array = Array.make (Array.length data) SpecialSquare.empty in
  for i = 0 to Array.length data - 1 do
    let new_sq = SpecialSquare.create data.(i) in
    SpecialSquare.draw new_sq;
    spec_sq_array.(i) <- new_sq
  done;
  spec_sq_array

(**Initialize all buttons*)
let all_buttons () =
  let dice_b = Button.create "Dice" 350 300 100 50 in
  let next_b = Button.create "Next" 780 825 40 40 in
  let buy_prop_b = Button.create "Buy property" 50 825 90 40 in
  let sell_property_b = Button.create "Sell property" 150 825 90 40 in
  let trade_b = Button.create "Trade" 250 825 90 40 in
  let buy_house_b = Button.create "Buy house" 350 825 90 40 in
  let buy_hotel_b = Button.create "Buy hotel" 450 825 90 40 in
  let sell_house_b = Button.create "Sell house" 550 825 90 40 in
  let sell_hotel_b = Button.create "Sell hotel" 650 825 90 40 in
  [
    dice_b;
    next_b;
    buy_prop_b;
    sell_property_b;
    trade_b;
    buy_house_b;
    buy_hotel_b;
    sell_house_b;
    sell_hotel_b;
  ]

(** Draw the list of buttons*)
let draw_buttons (b_list : Button.t list) =
  let _ = Button.draw_dice (List.nth b_list 0) in
  for i = 1 to List.length b_list - 1 do
    Button.draw_button (List.nth b_list i)
  done

(** Draw property list of one player*)
let player_draw_property_list p x y =
  if List.length (Player.current_property p) > 0 then
    for i = 1 to List.length (Player.current_property p) do
      moveto x (y - 160 - ((i - 1) * 10));
      draw_string
        ("Properties: "
        ^ PropSquare.name (List.nth (Player.current_property p) (i - 1)))
    done
  else moveto x (y - 160);
  draw_string "Properties: "

(**Draw each player's information.*)
let draw_player_info pl_num p =
  let board_width = 870 in
  let board_height = 870 in
  let loc_x = board_width + 30 + ((pl_num - 1) * 120) in
  if Player.is_empty p = false (*player still exists*) then (
    let loc_y = board_height in
    set_color black;
    moveto loc_x (loc_y - 80);
    draw_string ("Player " ^ string_of_int pl_num);
    moveto loc_x (loc_y - 120);
    draw_string ("Money: " ^ string_of_int (Player.current_money p));
    player_draw_property_list p loc_x loc_y)
  else
    (*player does not exist -> erase the whole column corresponding to him*)
    let loc_y = 0 in
    set_color white;
    fill_rect loc_x loc_y 120 870

(** Draw every player*)
let draw_players_info pl_array =
  for i = 1 to Array.length pl_array do
    draw_player_info i (Array.get pl_array (i - 1))
  done

(* Main function *)
let () =
  let prop_data = get_prop_data "propSquares.csv" in
  let spec_data = get_special_data "specSquares.csv" in
  Board.init_graphics ();
  Board.draw_board ();
  let prop_sq_arr = init_prop_sq prop_data in
  (*stored the array of property squares*)
  let spec_sq_arr = init_spec_sq spec_data in
  (*stored the array of special square*)
  let pl_arr = init_players in
  (*stored the array of players*)
  let buttons_list = all_buttons () in
  (*Stored array of buttons*)
  let _ = draw_buttons buttons_list in
  (*Draw buttons*)
  draw_players pl_arr;
  (*Draw player info*)
  draw_players_info pl_arr;
  (*draw players*)
  Game.game_logic buttons_list pl_arr prop_sq_arr spec_sq_arr (ref 0) (ref true);
  (* launch game loop *)
  ignore (read_key ())
(* Wait for a key press before closing *)
