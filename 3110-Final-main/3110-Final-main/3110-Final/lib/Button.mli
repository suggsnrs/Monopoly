type t

val create : string -> int -> int -> int -> int -> t
(** Create a button with specified dimensions and content.*)

val draw_button : t -> unit
(** Draw a button on the board.*)

val draw_dice : t -> int * int
(** Draw the dice button and return the position for drawing string with dice rolls.*)

val is_within_bounds : int -> int -> t -> bool
(** Check if click is within bounds.*)

val text : t -> string
(** Represent text as a string.*)

val x_coor : t -> int
(** The x-coordinate on the board.*)

val y_coor : t -> int
(** The y-coordinate on the board.*)

val width : t -> int
(** The width of the button.*)

val height : t -> int
(** The height of the button.*)
