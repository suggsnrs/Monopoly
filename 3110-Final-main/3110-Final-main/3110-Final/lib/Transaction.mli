(*Interface for a transaction between two players*)

type offer = {
  trade_player_id : int;
  curr_pl_offered_properties : PropSquare.t list;
  trade_pl_offered_properties : PropSquare.t list;
  offered_money : int;
  current_player_id : int;
}
(** Type representing a transaction offer. *)

val create_offer :
  trade_player_id:int ->
  curr_pl_offered_properties:PropSquare.t list ->
  trade_pl_offered_properties:PropSquare.t list ->
  offered_money:int ->
  current_player_id:int ->
  offer
(** Create a new offer *)

val execute_transaction :
  offer -> trade_pl_id:int -> curr_pl_id:int -> Player.t array -> bool
(** Execute a transaction between two players. Returns a boolean indicating if the transaction was successful. *)

val validate_offer : Player.t array -> int -> PropSquare.t list -> bool
(** Validate an offer. Ensures the player can legally make the offer (has the properties, has the money, etc.) *)
