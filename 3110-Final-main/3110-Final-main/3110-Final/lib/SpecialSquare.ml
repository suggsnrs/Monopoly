type t_aux = { num_sq : int; name : string; message : string }
type t = t_aux option

let empty = None
let create (pos, n, m) = Some { num_sq = pos; name = n; message = m }
let position square = match square with None -> -1 | Some sq -> sq.num_sq
let name square = match square with None -> "" | Some sq -> sq.name
let message square = match square with None -> "" | Some sq -> sq.message

let draw square =
  match square with
  | Some sq -> Board.draw_spec_sq sq.num_sq sq.name sq.message
  | None -> ()
