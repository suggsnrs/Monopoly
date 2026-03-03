type t

val empty : t
(** Nonexisting square*)

val create : int * string * string -> t
(** [create pos n m] creates a special square with number [pos], name [n], and message [m]*)

val position : t -> int
(** Returns the square's position*)

val name : t -> string
(** Returns the square's name*)

val message : t -> string
(** Returns the square's message*)

val draw : t -> unit
(** Draws the square on the board*)
