type t_aux = {
  num_sq : int;
  name : string;
  price : int;
  color : Graphics.color;
  owner : int option ref;
  houses : House.t list ref;
  hotels : Hotel.t list ref;
}

type t = t_aux option

let empty = None

let create (pos, n, pr, col) =
  Some
    {
      num_sq = pos;
      name = n;
      price = pr;
      color = col;
      owner = ref None;
      houses = ref [];
      hotels = ref [];
    }

let position square = match square with Some sq -> sq.num_sq | None -> -1
let name square = match square with Some sq -> sq.name | None -> ""
let price square = match square with Some sq -> sq.price | None -> -1

let color square =
  match square with Some sq -> sq.color | None -> Graphics.white

let owner square =
  match square with
  | None -> 0
  | Some sq -> ( match !(sq.owner) with Some i -> i | None -> 0)

let houses square = match square with None -> [] | Some sq -> !(sq.houses)
let hotels square = match square with None -> [] | Some sq -> !(sq.hotels)

let rent square =
  match square with
  | Some sq -> (
      if sq.houses = ref [] && sq.hotels = ref [] then sq.price / 8
      else if sq.hotels <> ref [] then sq.price * 3
      else
        match List.length !(sq.houses) with
        | 1 -> sq.price / 4
        | 2 -> sq.price / 2
        | 3 -> sq.price
        | 4 -> sq.price * 2
        | _ -> 0)
  | None -> -1

let building_cost square =
  match square with Some sq -> sq.price / 2 | None -> -1

let mortgage_value square =
  match square with Some sq -> sq.price / 2 | None -> -1

let change_owner square pl_num =
  match square with None -> () | Some sq -> sq.owner := Some pl_num

let delete_owner square =
  match square with None -> () | Some sq -> sq.owner := None

let draw square =
  match square with
  | Some sq -> Board.draw_prop_sq sq.num_sq sq.name sq.price sq.color
  | None -> ()

let build_house square house =
  match square with None -> () | Some sq -> sq.houses := house :: !(sq.houses)

let build_hotel square hotel =
  match square with None -> () | Some sq -> sq.hotels := hotel :: !(sq.hotels)

let num_house square =
  match square with None -> 0 | Some sq -> List.length !(sq.houses)

let num_hotel square =
  match square with None -> 0 | Some sq -> List.length !(sq.hotels)

let remove_house square =
  match square with
  | None -> ()
  | Some sq -> ( match !(sq.houses) with _ :: t -> sq.houses := t | [] -> ())

let remove_all_houses square =
  match square with None -> () | Some sq -> sq.houses := []

let remove_hotel square =
  match square with None -> () | Some sq -> sq.hotels := []
