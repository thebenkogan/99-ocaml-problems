let rec last = function
  | [] -> None
  | [ h ] -> Some h
  | _ :: t -> last t

let rec last_two = function
  | [ _ ]
  | [] ->
      None
  | [ a; b ] -> Some (a, b)
  | h :: t -> last_two t

let rec nth lst n =
  match lst with
  | [] -> failwith "nth"
  | h :: t -> if n = 0 then h else nth t (n - 1)
