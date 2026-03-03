(*Implementation file for a transaction*)
type offer = {
  trade_player_id : int;
  curr_pl_offered_properties : PropSquare.t list;
  trade_pl_offered_properties : PropSquare.t list;
  offered_money : int;
  current_player_id : int;
}

let create_offer ~trade_player_id ~curr_pl_offered_properties
    ~trade_pl_offered_properties ~offered_money ~current_player_id =
  {
    trade_player_id;
    curr_pl_offered_properties;
    trade_pl_offered_properties;
    offered_money;
    current_player_id;
  }

let validate_offer players pl_id pl_prop =
  let player = Array.get players (pl_id - 1) in
  (* Validate that the player owns the offered properties and property doesn't have buildings *)
  let owned_properties = Player.current_property player in
  List.for_all
    (fun prop ->
      List.mem prop owned_properties
      && PropSquare.houses prop = []
      && PropSquare.hotels prop = [])
    pl_prop

let execute_transaction offer ~trade_pl_id ~curr_pl_id players =
  if
    not
      (validate_offer players offer.current_player_id
         offer.curr_pl_offered_properties
      && validate_offer players offer.trade_player_id
           offer.trade_pl_offered_properties)
  then false
  else
    let responder = players.(trade_pl_id - 1) in
    let responder_has_money =
      Player.current_money responder >= offer.offered_money
    in
    if responder_has_money then (
      (* Exchange properties *)
      PropSquare.delete_owner (List.nth offer.curr_pl_offered_properties 0);
      PropSquare.delete_owner (List.nth offer.trade_pl_offered_properties 0);
      Player.delete_property
        players.(curr_pl_id - 1)
        (List.nth offer.curr_pl_offered_properties 0);
      Player.add_property
        players.(trade_pl_id - 1)
        (List.nth offer.curr_pl_offered_properties 0)
        players;
      Player.delete_property
        players.(trade_pl_id - 1)
        (List.nth offer.trade_pl_offered_properties 0);
      Player.add_property
        players.(curr_pl_id - 1)
        (List.nth offer.trade_pl_offered_properties 0)
        players;
      (* Exchange money *)
      Player.incr_money offer.offered_money
        (Array.get players (offer.current_player_id - 1));
      let _ = Player.decr_money offer.offered_money responder players in
      true)
    else false
