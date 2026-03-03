type t

val init_money : int
(** Initial amount of money given to every player*)

val go_money : int
(** Amount of money given to any player when crossing "GO" square*)

val empty : t
(**Nonexisting player*)

val create : int -> int ref -> t
(** Create a player with a given number placed on square 1 with [init_money] 
    amount of money, and no property squares*)

val is_empty : t -> bool
(**Returns true is the player is a nonexisting player*)

val number : t -> int
(** Return player's number*)

val current_pos : t -> int
(** Return current position of a player*)

val current_money : t -> int
(** Return current amount of money of a player*)

val current_property : t -> PropSquare.t list
(** Return list of current property squares of a player*)

val bankrupt : t array -> t -> int
(** Making the given player bancrupt. All buildings are removed from 
   corresponding properties; owner is deleted from properties;
   player is deleted from player array; player info, token, and buildings are visually removed; 
   display message about bankrupcy.*)

val incr_money : int -> t -> unit
(** Increase player's amount of money by a given number*)

val decr_money : int -> t -> t array -> int
(** Decrease player's amount of money by a given number if it doesn't make the 
    overall player's amount strictly negative and return that number. 
Otherwise call [bankrupt] function on this player and return the amount of money 
the player actually paid (which is what)*)

val change_position : t -> int -> unit
(** Change position field in the player's record to a given number and redraw the 
    player's token accordingly *)

val get_player : t array -> int -> t
(**[get_player players pl_id] returns a player with [pl_id] when [players] is 
    current players' array. Raises exception if there is no such player*)

val move_players_options : t array -> int ref -> int -> int -> int
(** Determines the number of a player who has to move when dice is rolled and 
    calls [change_position] on that player; returns previous position of this player*)

val buy_property : t -> PropSquare.t -> t array -> unit
(** Buy property for a player if the player has enough enough cash and does not have the property yet*)

val add_property : t -> PropSquare.t -> t array -> unit
(** Adds a property to a player's property list*)

val delete_property : t -> PropSquare.t -> unit
(** Removes property from a player's property list*)

val sell_property : t -> PropSquare.t -> unit
(** Sells property for its morgage value and removes that property from the player's property list*)

val buy_house : t -> PropSquare.t -> unit
(** Buys a house for the given property. If the player doesn't own the property, it prints a message
    saying "Not your property". If the player doesn't have all of the properties of a specific color,
        if prints a message saying "Collection not full". If the player doesn't have enough money to
            build, it prints a message saying "Not enough money". And, if the player already has 4 
                houses, it prints a message saying "Buy a Hotel"*)

val buy_hotel : t -> PropSquare.t -> unit
(** Buys a hotel for the given property. If the player doesn't own the property, it prints a message
    saying "Not your property". If it tries to buy more than one hotel, it prints out a message saying
     "Cannot have more than 1 hotel". If the player doesn't have enough money to
            build, it prints a message saying "Not enough money". And, if the player doesn't have 4 
                houses, it prints a message saying "Need 4 houses first"*)

val sell_house : t -> PropSquare.t -> unit
(** Sells the house form the given property. If the player doesn't own that property or the number of 
    houses on that property is 0, then it prints a message saying "Selling unsuccessful"*)

val sell_hotel : t -> PropSquare.t -> unit
(** Sells the hotel form the given property. If the player doesn't own that property then it 
    prints a message saying "Not your property". If there are no houses on the property then
        it prints a message saying "There are no houses to sell"*)
