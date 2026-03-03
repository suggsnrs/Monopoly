val init_graphics : unit -> unit
(** Initialize the graphics window *)

val draw_prop_sq : int -> string -> int -> Graphics.color -> unit
(** Draw a single square of the board*)

val draw_spec_sq : int -> string -> string -> unit
(** Convert number of a square [num] with side [sq_size] to coordinates of its 
    center on a board with margin [margin], length [board_size_x], and 
    width [board_size_y]. Squares are enumerated clockwise with square 1 at the 
    right lower corner square*)

val draw_player : int -> int -> unit
(** 1st, 2nd, 3rd, and 4th player's tokens are always drawn along the lower side of 
a board square. [pl_num], [sq_pos], and [sq_size] are inputs that denote the 
     number of the player, the center position of the player's square, and the size of the 
     player's square.*)

val remove_player : int -> int -> unit
(** Visually remove player [pl_num] from square with center coordinates [sq_pos] 
and side [sq_size]. Achived by drawing a white rectangle in place of the
     player's token.*)

val erase_pl : int -> int -> int list -> unit
(** Visually removes player's info column and token of player with number [pl_num]
 and current position [pl_pos]; erase buildings on squares with numbers [prop_numbers]*)

val draw_house : int -> int -> Graphics.color -> unit
(** Draws a house in the top right corner of the square*)

val erase_house : int -> unit
(*** Erases houses on the given square*)

val draw_hotel : int -> int -> Graphics.color -> unit
(** Draws a hotel in the top middle of the square*)

val erase_hotel : int -> unit
(** Erases hotels on the given square*)

val draw_board : unit -> unit
(** Draws the entire board*)
