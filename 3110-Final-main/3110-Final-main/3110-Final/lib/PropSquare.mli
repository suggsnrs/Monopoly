type t

val empty : t
(** Nonexisting square (not used anywhere except while creating an empty array 
    that later stores the property squares)*)

val create : int * string * int * Graphics.color -> t
(** [create pos name price col] creates a property square with a given position 
[pos], [name], [price], [col]. Owner is set to [None]; houses and hotels are empty 
lists.*)

val position : t -> int
(**Returns the number of the square on a board. Squares are numerated clockwise 
    with square 1 in the lower right corner (GO!)*)

val name : t -> string
(**Returns name of the property*)

val price : t -> int
(**Returns cost of buying property*)

val color : t -> Graphics.color
(** Returns color of the property*)

val rent : t -> int
(** Returns cost of rent of this property depending on its buildings*)

val building_cost : t -> int
(** Returns cost of building a single house or a single hotel (but hotel can be 
    built only together/after 4 houses are built on the same property)*)

val mortgage_value : t -> int
(** Returns the amount bank gives a player when the property is mortgaged*)

val owner : t -> int
(** Returns the number of the current owner. Returns [0] is the property does
     not have an owner or if it is nonexisting property*)

val houses : t -> House.t list
(** Returns the list of current houses*)

val hotels : t -> Hotel.t list
(** Returns the list of current hotels*)

val change_owner : t -> int -> unit
(** Changes the owner to the given number [n]. Requires: [n] is a number of a player*)

val delete_owner : t -> unit
(** Changes the owner to [None]*)

val draw : t -> unit
(** Draws the square*)

val build_house : t -> House.t -> unit
(** Build a house on the property*)

val build_hotel : t -> Hotel.t -> unit
(** build a hotel on the property*)

val num_house : t -> int
(** Returns number of houses*)

val num_hotel : t -> int
(** Returns number of hotels*)

val remove_house : t -> unit
(** Removes one house*)

val remove_hotel : t -> unit
(** Removes the one and only hotel*)

val remove_all_houses : t -> unit
(** Removes all houses at once*)
