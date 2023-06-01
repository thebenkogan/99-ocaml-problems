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

let rec length = function
  | [] -> 0
  | _ :: t -> 1 + length t

let rev lst =
  let rec rev_aux acc = function
    | [] -> acc
    | h :: t -> rev_aux (h :: acc) t
  in
  rev_aux [] lst

let is_palindrome lst = lst = rev lst
