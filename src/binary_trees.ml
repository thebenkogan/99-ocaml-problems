type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

let generate_tree_combinations left right =
  left
  |> List.map (fun l_tree ->
         List.map (fun r_tree -> Node ('x', l_tree, r_tree)) right)
  |> List.flatten

let rec cbal_tree n =
  if n = 0 then [ Empty ]
  else if n mod 2 = 0 then
    let t1 = cbal_tree (n / 2) in
    let t2 = cbal_tree ((n / 2) - 1) in
    generate_tree_combinations t1 t2 @ generate_tree_combinations t2 t1
  else
    let t = cbal_tree (n / 2) in
    generate_tree_combinations t t

let is_symmetric t =
  let rec is_mirror l r =
    match (l, r) with
    | Empty, Empty -> true
    | Node (_, l1, r1), Node (_, l2, r2) ->
        is_mirror l1 r2 && is_mirror r1 l2
    | _ -> false
  in
  match t with
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r

let construct vals =
  let rec insert v = function
    | Empty -> Node (v, Empty, Empty)
    | Node (nv, l, r) ->
        if v > nv then Node (nv, l, insert v r)
        else if v = nv then Node (v, l, r)
        else Node (nv, insert v l, r)
  in
  let rec construct_aux acc = function
    | [] -> acc
    | v :: t -> construct_aux (insert v acc) t
  in
  construct_aux Empty vals
