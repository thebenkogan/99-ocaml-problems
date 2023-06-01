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

type 'a node =
  | One of 'a
  | Many of 'a node list

let rec flatten = function
  | [] -> []
  | h :: t -> begin
      match h with
      | One v -> v :: flatten t
      | Many vs -> flatten vs @ flatten t
    end

let compress lst =
  let rec compress_aux last = function
    | [] -> []
    | h :: t -> begin
        match last with
        | Some v when v = h -> compress_aux last t
        | _ -> h :: compress_aux (Some h) t
      end
  in
  compress_aux None lst

let pack lst =
  let rec pack_aux last = function
    | [] -> [ last ]
    | h :: t -> begin
        match last with
        | v :: _ when v = h -> pack_aux (h :: last) t
        | [] -> pack_aux [ h ] t
        | l -> l :: pack_aux [ h ] t
      end
  in
  pack_aux [] lst
