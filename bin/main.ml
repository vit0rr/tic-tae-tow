type turn = O | X | E
type tree = Move of turn list * tree list

let won = function
  | [ a; b; c; d; e; f; g; h; i ] ->
      (a && b && c)
      || (d && e && f)
      || (g && h && i)
      || (a && d && g)
      || (b && e && h)
      || (c && f && i)
      || (a && e && i)
      || (c && e && g)
  | _ -> false

let empty b =
  List.mapi (fun i t -> if t = E then i + 1 else 0) b
  |> List.filter (fun x -> x <> 0)

let replace turn board p =
  List.mapi (fun i t -> if i = p - 1 then turn else t) board

let flip_turn t = match t with O -> X | X -> O | E -> E

let rec next_moves turn board =
  let next =
    if won (List.map (( = ) O) board) || won (List.map (( = ) X) board) then []
    else
      empty board
      |> List.map (fun pos -> replace turn board pos)
      |> List.map (next_moves (flip_turn turn))
  in
  Move (board, next)

let game_tree = next_moves O [ E; E; E; E; E; E; E; E; E ]

let rec num_wins turn (Move (b, bs)) =
  (if won (List.map (( = ) turn) b) then 1 else 0)
  + List.fold_left ( + ) 0 (List.map (num_wins turn) bs)

let () =
  Printf.printf "%d\n" (num_wins O game_tree);
  Printf.printf "%d\n" (num_wins X game_tree)
