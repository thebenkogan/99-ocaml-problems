type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval_expr env = function
  | Var a -> List.assoc a env
  | Not b -> not (eval_expr env b)
  | And (b1, b2) -> eval_expr env b1 && eval_expr env b2
  | Or (b1, b2) -> eval_expr env b1 || eval_expr env b2

let table2 v1 v2 expr =
  [ (false, false); (false, true); (true, false); (true, true) ]
  |> List.map (fun (x1, x2) ->
         (x1, x2, eval_expr [ (v1, x1); (v2, x2) ] expr))

let table vars expr =
  let rec get_inputs acc = function
    | [] -> List.map List.rev acc
    | v :: t ->
        get_inputs
          (List.map (fun inputs -> (v, true) :: inputs) acc
          @ List.map (fun inputs -> (v, false) :: inputs) acc)
          t
  in
  vars |> get_inputs [ [] ]
  |> List.map (fun env -> (env, eval_expr env expr))

let gray n =
  let rec gray_aux acc n =
    if n = 0 then acc
    else
      gray_aux
        (List.map (fun code -> code ^ "1") acc
        @ List.map (fun code -> code ^ "0") acc)
        (n - 1)
  in
  gray_aux [ "" ] n
