val coord_to_num : int -> int -> int
(** Returns the number of the square such that coordinates [x] and [y] lie inside it. 
    Requires [x] and [y] to be inside one of the squares (but not on the edge)*)

val is_within_board_sq : int -> int -> bool
(** Returns true if the coordinates are within some of the board squares*)

val is_prop_sq : PropSquare.t array -> int -> bool
(** [is_prop prop_array num] returns true if the square numbered [num] is a prop square*)

val is_click_prop : int -> int -> PropSquare.t array -> bool
(** Check whether coordinates [x] [y] are within some property square*)

val get_prop_sq : PropSquare.t array -> int -> PropSquare.t
(** Return property square with given number from squares array.
    Requires square with given number is in the array*)

val get_spec_sq : SpecialSquare.t array -> int -> SpecialSquare.t
(** Return special square with given number from squares array.
    Requires square with given number is in the array*)

val draw_info : string -> unit
(** Draw the given informational message [message] in the center of the board*)

val erase_info : string -> unit
(** Erase the given informational message [message] from the center of the board*)

val draw_erase_info : string -> int -> unit
(** Draw the given informational message [message] in the center of the board 
    and erase it after [time] seconds*)

val stepped_over_go : int -> int -> Player.t -> unit
(** Check whether a player stepped over GO! square and, if yes, give him a salary*)

val show_dice_roll : int -> int -> int -> int -> unit
(** Redraw numbers on the dice*)

val roll_dice : Button.t -> Player.t array -> int ref -> int
(** Roll dice once, move correct player, and return its position before the move*)

val draw_rect : int -> int -> int -> int -> unit
(** Draw rectangle*)

val player_draw_property_list : Player.t -> int -> int -> unit
(** Drawing a single player's property*)

val draw_players_info : Player.t array -> int -> unit
(** Draw all players information.*)

val center_money : int ref
(** Storing center money*)

val incr_center_money : int -> unit
(** Increment the center money*)

val decr_center_money : int -> unit
(** Decrement the center money*)

val draw_center_money : int -> unit
(** Redraw center money*)

val generate_random_message : string -> string
(** Given a type of card [result] choose a card of the corresponding type at random*)

val get_card_data : string -> (string * string) list
(** Convert data from a csv file [file] into an array of tuples, each of which 
    stores the category of the card and the action item.*)

val chance : unit -> string * string
(** Function to get a random chance card from "chance.csv" *)

val chest : unit -> string * string
(** Function to get a random community chest card from "chest.csv" *)

val wait_prop_click : PropSquare.t array -> int
(** Wait until some property square is clicked on and return the number of clicked square*)

val wait_keybord_pl_num : Player.t array -> int
(** Wait until a key with number of existing player is pressed and return that number*)

val step_on_spec_sq :
  Player.t array -> Player.t -> SpecialSquare.t -> PropSquare.t array -> unit
(** Game logic when player steps on a special square*)

val dice_key : bool ref
(** True when dice can be pressed*)

val game_logic :
  Button.t list ->
  Player.t array ->
  PropSquare.t array ->
  SpecialSquare.t array ->
  int ref ->
  bool ref ->
  unit
(** Recursive loop for the game which determines what happens when a button is pressed.*)
