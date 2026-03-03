open Graphics

type t = { name : string; x : int; y : int; width : int; height : int }

let create text x_coor y_coor w h =
  { name = text; x = x_coor; y = y_coor; width = w; height = h }

let draw_button button =
  let text = button.name in
  let x = button.x in
  let y = button.y in
  let width = button.width in
  let height = button.height in
  (* Draw the button rectangle *)
  set_color black;
  draw_rect x y width height;
  set_color red;
  fill_rect x y width height;
  (* Calculate the position to draw the button text centered vertically but left-aligned *)
  let _, text_height = text_size text in
  let text_x = x + 10 in
  (* Adding a margin to the left *)
  let text_y = y + (height / 2) - (text_height / 2) in
  (* Move to the calculated position and draw the text *)
  moveto text_x text_y;
  set_color black;
  draw_string text

let draw_dice button =
  let text = button.name in
  let x = button.x in
  let y = button.y in
  let width = button.width in
  let height = button.height in
  (* Draw the button rectangle *)
  set_color black;
  draw_rect x y width height;
  (* Calculate the position to draw the button text centered vertically but left-aligned *)
  let text_width, text_height = text_size text in
  let text_x = x + 10 in
  (* Adding a margin to the left *)
  let text_y = y + (height / 2) - (text_height / 2) in
  (* Move to the calculated position and draw the text *)
  moveto text_x text_y;
  draw_string text;
  (text_x + text_width + 10, text_y)
(* Return position for dice roll *)

let is_within_bounds x y button =
  let rect_x = button.x in
  let rect_y = button.y in
  let rect_width = button.width in
  let rect_height = button.height in
  x >= rect_x
  && x <= rect_x + rect_width
  && y >= rect_y
  && y <= rect_y + rect_height

let text button = button.name
let x_coor button = button.x
let y_coor button = button.y
let width button = button.width
let height button = button.height
