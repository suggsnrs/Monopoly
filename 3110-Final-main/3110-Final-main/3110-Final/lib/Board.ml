open Graphics

(* Initialize the graphics window *)
let init_graphics () =
  open_graph " 1500x870";
  set_window_title "Monopoly Board"

(** Draw a single square of the board with lower left corner coord-s [x] and [y] 
    and square side [size] *)
let draw_square x y size =
  moveto x y;
  lineto x (y + size);
  lineto (x + size) (y + size);
  lineto (x + size) y;
  lineto x y

(** Convert number of a square [num] with side [sq_size] to coordinates of its 
    center on a board with margin [margin], length [board_size_x], and 
    width [board_size_y]. Squares are enumerated clockwise with square 1 at the 
    right lower corner square*)
let num_to_coord num margin sq_size board_size_x board_size_y =
  if num <= 11 then
    ( board_size_x - margin - ((num - 1) * sq_size) - (sq_size / 2),
      margin + (sq_size / 2) )
  else if num >= 21 && num <= 31 then
    ( margin + ((num - 21) * sq_size) + (sq_size / 2),
      board_size_y - margin - (sq_size / 2) )
  else if num >= 12 && num <= 20 then
    (margin + (sq_size / 2), margin + ((num - 11) * sq_size) + (sq_size / 2))
  else
    ( board_size_x - margin - (sq_size / 2),
      board_size_y - margin - ((num - 31) * sq_size) - (sq_size / 2) )

(**[center_text] will center the given string [text] when give a tuple with it's 
current position [pos] and a margin [margin] of how far horizontally you want to 
text to go. If the text is longer than this, it will split to the next line down.*)
let center_text text pos =
  let x = fst pos in
  let y = snd pos in
  let text_width, text_height = text_size text in
  let text_x = x - (text_width / 2) + text_height - text_height in
  let text_y = y in

  (text_x, text_y)

(** A helper method for draw_prop_sq, if [col] isn't white the colour will be
    set to col, and draws the rectangle whose location depends on [x],[y],[num], 
and [sq_size], and length and width depends on [strip_length],[strip_width]*)
let draw_helper col num x y sq_size strip_length strip_width =
  if col <> white then (
    set_color col;
    if num > 1 && num < 11 then
      let strip_x = x - (sq_size / 2) in
      let strip_y = y + (sq_size / 2) - strip_width in
      fill_rect strip_x strip_y strip_length strip_width
    else if num > 11 && num < 21 then
      let strip_x = x + (sq_size / 2) - strip_width in
      let strip_y = y - (sq_size / 2) in
      fill_rect strip_x strip_y strip_width strip_length
    else if num > 21 && num < 31 then
      let strip_x = x - (sq_size / 2) in
      let strip_y = y - (sq_size / 2) in
      fill_rect strip_x strip_y strip_length strip_width
    else
      let strip_x = x - (sq_size / 2) in
      let strip_y = y - (sq_size / 2) in
      fill_rect strip_x strip_y strip_width strip_length)
  else ()

let draw_prop_sq num name price col =
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_size = 70 in
  let margin = 50 in
  let strip_width = (sq_size / 10) - 2 in
  let strip_length = sq_size in
  let x = fst (num_to_coord num margin sq_size board_size_x board_size_y) in
  let y = snd (num_to_coord num margin sq_size board_size_x board_size_y) in
  moveto (fst (center_text name (x, y))) (snd (center_text name (x, y)));
  set_color black;
  draw_string name;
  let str = "$" ^ string_of_int price in
  moveto (fst (center_text str (x, y))) (snd (center_text str (x, y)) - 15);
  draw_string str;
  draw_helper col num x y sq_size strip_length strip_width

let draw_spec_sq num name message =
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_zize = 70 in
  let margin = 50 in
  let x = fst (num_to_coord num margin sq_zize board_size_x board_size_y) in
  let y = snd (num_to_coord num margin sq_zize board_size_x board_size_y) in
  moveto (fst (center_text name (x, y))) (snd (center_text name (x, y)));
  set_color black;
  draw_string name;
  moveto
    (fst (center_text message (x, y)))
    (snd (center_text message (x, y)) - 15);
  draw_string message

(**Helper function of draw_player, determines colour based on [pl_num] and draws
    players whose locations are based on [sq_pos], [sq_size], and 
    dimension based on [pl_size]*)
let draw_player_helper pl_num sq_pos sq_size pl_size =
  let () =
    if pl_num = 1 then set_color magenta
    else if pl_num = 2 then set_color blue
    else if pl_num = 3 then set_color red
    else if pl_num = 4 then set_color green
    else ()
  in
  fill_rect
    (fst sq_pos - (sq_size / 2)
    + (sq_size / 10 * pl_num)
    + (pl_size * (pl_num - 1)))
    (snd sq_pos - (sq_size / 2) + (sq_size / 10))
    pl_size pl_size

let draw_player pl_num sq_num =
  let margin = 50 in
  let sq_size = 70 in
  let pl_size = sq_size / 8 in
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_pos = num_to_coord sq_num margin sq_size board_size_x board_size_y in
  draw_player_helper pl_num sq_pos sq_size pl_size

let remove_player_helper pl_num sq_pos_x sq_pos_y sq_size pl_size =
  let x =
    sq_pos_x - (sq_size / 2) + (sq_size / 10 * pl_num) + (pl_size * (pl_num - 1))
  in
  let y = sq_pos_y - (sq_size / 2) + (sq_size / 10) in
  fill_rect x y pl_size pl_size

let remove_player pl_num sq_num =
  let margin = 50 in
  let sq_size = 70 in
  let board_sz_x = 870 in
  let board_sz_y = 870 in
  let sq_pos_x, sq_pos_y =
    num_to_coord sq_num margin sq_size board_sz_x board_sz_y
  in
  let pl_size = sq_size / 8 in
  set_color Graphics.white;
  match pl_num with
  | 1 -> remove_player_helper 1 sq_pos_x sq_pos_y sq_size pl_size
  | 2 -> remove_player_helper 2 sq_pos_x sq_pos_y sq_size pl_size
  | 3 -> remove_player_helper 3 sq_pos_x sq_pos_y sq_size pl_size
  | 4 -> remove_player_helper 4 sq_pos_x sq_pos_y sq_size pl_size
  | _ -> ()

let draw_house house_num sq_num color =
  let margin = 50 in
  let sq_size = 70 in
  let pl_size = sq_size / 8 in
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_pos = num_to_coord sq_num margin sq_size board_size_x board_size_y in
  set_color color;
  fill_rect
    (fst sq_pos - (sq_size / 2) + (sq_size / 10))
    (snd sq_pos + (sq_size / 2) - 5 - (sq_size / 10)
    - (((sq_size / 10) - 4) * 3))
    (pl_size * 2) (pl_size * 2);
  set_color black;
  moveto
    (fst sq_pos - (sq_size / 2) + (sq_size / 10) + 2)
    (snd sq_pos + (sq_size / 2) - (sq_size / 10)
    - (((sq_size / 10) - 4) * 2)
    - 5);
  draw_string (string_of_int house_num)

let erase_house sq_num =
  let margin = 50 in
  let sq_size = 70 in
  let pl_size = sq_size / 8 in
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_pos = num_to_coord sq_num margin sq_size board_size_x board_size_y in
  set_color white;
  fill_rect
    (fst sq_pos - (sq_size / 2) + (sq_size / 10))
    (snd sq_pos + (sq_size / 2) - (sq_size / 10)
    - (((sq_size / 10) - 4) * 3)
    - 5)
    (pl_size * 2) (pl_size * 2)

let draw_hotel hotel_num sq_num color =
  erase_house sq_num;
  let margin = 50 in
  let sq_size = 70 in
  let pl_size = sq_size / 8 in
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_pos = num_to_coord sq_num margin sq_size board_size_x board_size_y in
  set_color color;
  fill_rect
    (fst sq_pos - (sq_size / 2) + (sq_size / 10) + 20)
    (snd sq_pos + (sq_size / 2) - 5 - (sq_size / 10)
    - (((sq_size / 10) - 4) * 3))
    (pl_size * 4) (pl_size * 2);
  set_color black;
  moveto
    (fst sq_pos - (sq_size / 2) + (sq_size / 10) + 30)
    (snd sq_pos + (sq_size / 2) - 5 - (sq_size / 10)
    - (((sq_size / 10) - 4) * 2));
  draw_string (string_of_int hotel_num)

let erase_hotel sq_num =
  let margin = 50 in
  let sq_size = 70 in
  let pl_size = sq_size / 8 in
  let board_size_x = 870 in
  let board_size_y = 870 in
  let sq_pos = num_to_coord sq_num margin sq_size board_size_x board_size_y in
  set_color white;
  fill_rect
    (fst sq_pos - (sq_size / 2) + (sq_size / 10) + 20)
    (snd sq_pos + (sq_size / 2) - (sq_size / 10)
    - (((sq_size / 10) - 4) * 3)
    - 5)
    (pl_size * 4) (pl_size * 2)

let erase_pl pl_num pl_pos (prop_numbers : int list) =
  let board_width = 870 in
  let board_height = 870 in
  let pl_column_width = 118 in
  let pl_num = pl_num in
  let pl_pos = pl_pos in
  let x = board_width + 30 + ((pl_num - 1) * 120) in
  let y = 0 in
  set_color white;
  moveto x y;
  fill_rect x y pl_column_width board_height;
  remove_player pl_num pl_pos;
  for i = 0 to List.length prop_numbers - 1 do
    erase_house (List.nth prop_numbers i);
    erase_hotel (List.nth prop_numbers i)
  done

(** Function to draw a simplified representation of the Monopoly logo *)
let draw_simplified_logo x y =
  set_color red;
  fill_rect x y 200 100;
  (* This would be the red background *)
  set_color black;
  moveto (x + 75) (y + 40);
  (* Position the text roughly in the middle of the red background *)
  draw_string "MONOPOLY"

(** Function to capture the drawn logo as an image and draw it in the center of the board *)
let place_logo_in_center () =
  let board_width = 870 in
  let board_height = 870 in
  let logo_width = 200 in
  let logo_height = 100 in
  (* Coordinates to center the logo on the board *)
  let logo_x = (board_width - logo_width) / 2 in
  let logo_y = (board_height - logo_height) / 2 in
  (* Draw the simplified logo at the center *)
  draw_simplified_logo logo_x logo_y;
  (* Capture the drawn logo as an image *)
  let logo_image = get_image logo_x logo_y logo_width logo_height in
  (* Now you can use logo_image to draw it elsewhere, or simply keep it here *)
  draw_image logo_image logo_x logo_y

(** Draw initial center money*)
let draw_init_center_money value =
  let x = 870 / 2 in
  let y = (870 / 2) - 20 in
  set_color green;
  fill_rect (x - 100) (y + 120) 200 10;
  set_color black;
  moveto (x - 100) (y + 120);
  draw_string ("Center Money: $" ^ string_of_int value)

(** Draw the entire board *)
let draw_board () =
  let outer_size = 770 in
  let square_size = outer_size / 11 in
  let margin = (870 - outer_size) / 2 in

  (* Draw squares - top and bottom *)
  for i = 0 to 10 do
    let x = margin + (i * square_size) in
    draw_square x margin square_size;
    draw_square x (margin + outer_size - square_size) square_size
  done;
  (* Draw squares - left and right*)
  for i = 1 to 9 do
    let y = margin + (i * square_size) in
    draw_square margin y square_size;
    draw_square (margin + outer_size - square_size) y square_size
  done;
  (*Draw initial center money*)
  draw_init_center_money 0;
  place_logo_in_center ()
