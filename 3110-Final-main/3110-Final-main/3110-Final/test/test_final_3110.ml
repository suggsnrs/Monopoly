open OUnit2
open Final_3110
open Graphics

(** Convert data in a csv file [file] into an array of tuples, each of which 
    stores property square position, name, purchase price, and colour (as Graphics 
    type [color].)*)
let get_prop_data file =
  let table = Csv.load file in
  let size = List.length table - 1 in
  let tuples = Array.make size (0, "", 0, 0) in
  for row = 1 to size do
    let r = List.nth table row in
    let position = int_of_string (List.nth r 0) in
    let name = List.nth r 1 in
    let price = int_of_string (List.nth r 2) in
    let colour =
      rgb
        (int_of_string (List.nth r 3))
        (int_of_string (List.nth r 4))
        (int_of_string (List.nth r 5))
    in
    tuples.(row - 1) <- (position, name, price, colour)
  done;
  tuples

(** Convert data in a csv file [file] into an array of tuples, each of which 
        stores special square position, name, and text message*)
let get_special_data file =
  let table = Csv.load file in
  let size = List.length table - 1 in
  let tuples = Array.make size (0, "", "") in
  for row = 1 to size do
    let r = List.nth table row in
    let pos = int_of_string (List.nth r 0) in
    let name = List.nth r 1 in
    let message = List.nth r 2 in
    tuples.(row - 1) <- (pos, name, message)
  done;
  tuples

(** Creating property squares with the data stored in  [data] and calling the 
        drawing function for each of them. [data] is the list of tuples containing 
        square number, square name, purchase price, and color (as an int)*)
let init_prop_sq data =
  let prop_sq_array = Array.make (Array.length data) PropSquare.empty in
  for i = 0 to Array.length data - 1 do
    let new_sq = PropSquare.create data.(i) in
    (*removed drawing*)
    prop_sq_array.(i) <- new_sq
  done;
  prop_sq_array

(** Creating special squares with the data stored in  [data] and calling the 
        drawing function for each of them. [data] is the list of tuples containing 
        square number and square name*)
let init_spec_sq data =
  let spec_sq_array = Array.make (Array.length data) SpecialSquare.empty in
  for i = 0 to Array.length data - 1 do
    let new_sq = SpecialSquare.create data.(i) in
    (*removed drawing*)
    spec_sq_array.(i) <- new_sq
  done;
  spec_sq_array

let prop_data = get_prop_data "propSquaresTest.csv"
let spec_data = get_special_data "specSquaresTest.csv"

(*Code from main file that makes it so that you can create a property array and
   a special square array.*)

let button_tests =
  "test suite"
  >::: [
         (*Tests for Button.ml*)
         ( "Button.create and Button.text" >:: fun _ ->
           let button = Button.create "hi" 1 2 3 4 in
           assert_equal (Button.text button) "hi" );
         ( "Button.x_coor" >:: fun _ ->
           let button = Button.create "hi" 1 2 3 4 in
           assert_equal (Button.x_coor button) 1 );
         ( "Button.y_coor" >:: fun _ ->
           let button = Button.create "hi" 1 2 3 4 in
           assert_equal (Button.y_coor button) 2 );
         ( "Button.width" >:: fun _ ->
           let button = Button.create "hi" 1 2 3 4 in
           assert_equal (Button.width button) 3 );
         ( "Button.height" >:: fun _ ->
           let button = Button.create "hi" 1 2 3 4 in
           assert_equal (Button.height button) 4 );
       ]

let game_tests_one =
  "test suite"
  >::: [
         (*Tests for Game.ml*)
         ( "Game.coords_to_num" >:: fun _ ->
           assert_equal 12 (Game.coord_to_num 90 125) );
         ( "Game.is_within_board_sq true" >:: fun _ ->
           assert_equal true (Game.is_within_board_sq 90 125) );
         ( "Game.is_within_board_sq false" >:: fun _ ->
           assert_equal false (Game.is_within_board_sq 0 0) );
         ( "Game.is_prop_sq false" >:: fun _ ->
           assert_equal false (Game.is_prop_sq (init_prop_sq prop_data) 1) );
         ( "Game.is_prop_sq true" >:: fun _ ->
           assert_equal true (Game.is_prop_sq (init_prop_sq prop_data) 2) );
         ( "Game.is_click_prop false 1" >:: fun _ ->
           assert_equal false (Game.is_click_prop 1 1 (init_prop_sq prop_data))
         );
         ( "Game.is_click_prop false 2" >:: fun _ ->
           assert_equal false
             (Game.is_click_prop 60 60 (init_prop_sq prop_data)) );
       ]

let game_tests_two =
  "test suite"
  >::: [
         ( "Game.is_click_prop true 1" >:: fun _ ->
           assert_equal true
             (Game.is_click_prop 90 125 (init_prop_sq prop_data)) );
         ( "Game.get_prop_sq" >:: fun _ ->
           assert_equal
             (init_prop_sq prop_data).(0)
             (Game.get_prop_sq (init_prop_sq prop_data) 2) );
         ( "Game.get_spec_sq" >:: fun _ ->
           assert_equal
             (init_spec_sq spec_data).(0)
             (Game.get_spec_sq (init_spec_sq spec_data) 1) );
         ( "Game.incr_center_money" >:: fun _ ->
           assert_equal
             (let num = !Game.center_money in
              Game.center_money := num + 1)
             (Game.incr_center_money 1) );
       ]

let game_tests_three =
  "test suite"
  >::: [
         ( "Game.decr_center_money" >:: fun _ ->
           assert_equal
             (let num = !Game.center_money in
              Game.center_money := num - 1)
             (Game.decr_center_money 1) );
         ( "Game.get_card_data chance" >:: fun _ ->
           assert_equal ("move", "Taipei")
             (List.nth (Game.get_card_data "chanceTest.csv") 0) );
         ( "Game.get_card_data chest" >:: fun _ ->
           assert_equal ("move", "GO")
             (List.nth (Game.get_card_data "chestTest.csv") 0) );
       ]

(*Tests for Hotel.ml*)
let hotel_and_house_tests =
  "test suite"
  >::: [
         ( "Hotel.get_name and Hotel.create" >:: fun _ ->
           let hotel = Hotel.create () in
           assert_equal (Hotel.get_name hotel) "hotel" );
         ( "House.get_name and House.create" >:: fun _ ->
           let house = House.create () in
           assert_equal (House.get_name house) "name" );
       ]

(*Tests for House.ml*)
let player_tests_one =
  "test suite"
  >::: [
         (*Tests for Player.ml*)
         ("Player.init_money" >:: fun _ -> assert_equal 5000 Player.init_money);
         ("Player.go_money" >:: fun _ -> assert_equal 2000 Player.go_money);
         ( "Player.is_empty false" >:: fun _ ->
           assert_equal false (Player.is_empty (Player.create 1 (ref 1))) );
         ( "Player.is_empty true" >:: fun _ ->
           assert_equal true (Player.is_empty Player.empty) );
         ( "Player.number" >:: fun _ ->
           assert_equal 2 (Player.number (Player.create 2 (ref 1))) );
         ( "Player.current_pos" >:: fun _ ->
           assert_equal 2 (Player.current_pos (Player.create 1 (ref 2))) );
         ( "Player.current_money" >:: fun _ ->
           assert_equal 5000 (Player.current_money (Player.create 1 (ref 2))) );
         ( "Player.current_property" >:: fun _ ->
           assert_equal [] (Player.current_property (Player.create 1 (ref 2)))
         );
       ]

let player_tests_two =
  "test suite"
  >::: [
         ( "Player.incr_money" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           Player.incr_money 1 pl;
           assert_equal 5001 (Player.current_money pl) );
         ( "Player.decr_money" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let _ = Player.decr_money 1 pl (Array.make 1 pl) in
           assert_equal 4999 (Player.current_money pl) );
         ( "Player.buy_property" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = Player.buy_property pl ps (Array.make 1 pl) in
           assert_equal (Player.current_money pl) 4998 );
       ]

let player_tests_three =
  "test suite"
  >::: [
         ( "Player.add_property" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = Player.buy_property pl ps (Array.make 1 pl) in
           assert_equal (Player.current_property pl) [ ps ] );
         ( "Player.sell_property" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = Player.buy_property pl ps (Array.make 1 pl) in
           let _ = Player.sell_property pl ps in
           assert_equal (Player.current_property pl) [] );
         ( "Player.delete_property" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = Player.buy_property pl ps (Array.make 1 pl) in
           let _ = Player.delete_property pl ps in
           assert_equal (Player.current_property pl) [] );
       ]

let propsquare_tests_one =
  "test suite"
  >::: [
         (*Tests for PropSquare.ml*)
         ( "PropSquare.position" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal 1 (PropSquare.position ps) );
         ( "PropSquare.name" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal "hi" (PropSquare.name ps) );
         ( "PropSquare.price" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal 2 (PropSquare.price ps) );
         ( "PropSquare.color" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal Graphics.blue (PropSquare.color ps) );
         ( "PropSquare.owner" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal 0 (PropSquare.owner ps) );
         ( "PropSquare.houses" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal [] (PropSquare.houses ps) );
       ]

let propsquare_tests_two =
  "test suite"
  >::: [
         ( "PropSquare.hotels" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           assert_equal [] (PropSquare.hotels ps) );
         ( "PropSquare.rent" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 16, Graphics.blue) in
           assert_equal 2 (PropSquare.rent ps) );
         ( "PropSquare.building_cost" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 16, Graphics.blue) in
           assert_equal 8 (PropSquare.building_cost ps) );
         ( "PropSquare.mortgage_value" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 16, Graphics.blue) in
           assert_equal 8 (PropSquare.mortgage_value ps) );
         ( "PropSquare.change_owner" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = PropSquare.change_owner ps (Player.number pl) in
           assert_equal (PropSquare.owner ps) (Player.number pl) );
       ]

let propsquare_tests_three =
  "test suite"
  >::: [
         ( "PropSquare.delete_owner" >:: fun _ ->
           let pl = Player.create 1 (ref 2) in
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let _ = PropSquare.change_owner ps (Player.number pl) in
           let _ = PropSquare.delete_owner ps in
           assert_equal (PropSquare.owner ps) 0 );
         ( "PropSquare.build_house" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let house = House.create () in
           let _ = PropSquare.build_house ps house in
           assert_equal (PropSquare.houses ps) [ house ] );
         ( "PropSquare.build_hotel" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let hotel = Hotel.create () in
           let _ = PropSquare.build_hotel ps hotel in
           assert_equal (PropSquare.hotels ps) [ hotel ] );
       ]

let propsquare_tests_four =
  "test suite"
  >::: [
         ( "PropSquare.num_house" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let house = House.create () in
           let _ = PropSquare.build_house ps house in
           assert_equal (PropSquare.num_house ps) 1 );
         ( "PropSquare.num_hotel" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let hotel = Hotel.create () in
           let _ = PropSquare.build_hotel ps hotel in
           assert_equal (PropSquare.num_hotel ps) 1 );
         ( "PropSquare.remove_house" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let house = House.create () in
           let _ = PropSquare.build_house ps house in
           let _ = PropSquare.remove_house ps in
           assert_equal (PropSquare.num_house ps) 0 );
       ]

let propsquare_tests_five =
  "test suite"
  >::: [
         ( "PropSquare.remove_all_houses" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let house = House.create () in
           let _ = PropSquare.build_house ps house in
           let _ = PropSquare.build_house ps house in
           let _ = PropSquare.build_house ps house in
           let _ = PropSquare.build_house ps house in
           let _ = PropSquare.remove_all_houses ps in
           assert_equal (PropSquare.num_house ps) 0 );
         ( "PropSquare.remove_hotel" >:: fun _ ->
           let ps = PropSquare.create (1, "hi", 2, Graphics.blue) in
           let hotel = Hotel.create () in
           let _ = PropSquare.build_hotel ps hotel in
           let _ = PropSquare.remove_hotel ps in
           assert_equal (PropSquare.num_hotel ps) 0 );
       ]

let specsquare_tests =
  "test suite"
  >::: [
         (*Tests for SpecialSquare.ml*)
         ( "SpecialSquare.position" >:: fun _ ->
           let ss = SpecialSquare.create (1, "Jay", "Hi") in
           assert_equal 1 (SpecialSquare.position ss) );
         ( "SpecialSquare.name" >:: fun _ ->
           let ss = SpecialSquare.create (1, "Jay", "Hi") in
           assert_equal "Jay" (SpecialSquare.name ss) );
         ( "SpecialSquare.message" >:: fun _ ->
           let ss = SpecialSquare.create (1, "Jay", "Hi") in
           assert_equal "Hi" (SpecialSquare.message ss) );
       ]

(*Tests for Transaction.ml*)
let test_suite =
  "interval test suite"
  >::: [
         button_tests;
         game_tests_one;
         game_tests_two;
         game_tests_three;
         hotel_and_house_tests;
         player_tests_one;
         player_tests_two;
         player_tests_three;
         propsquare_tests_one;
         propsquare_tests_two;
         propsquare_tests_three;
         propsquare_tests_four;
         propsquare_tests_five;
         specsquare_tests;
       ]

let _ = run_test_tt_main test_suite
