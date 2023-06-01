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

let encode lst =
  List.map
    (function
      | h :: _ as l -> (length l, h)
      | [] -> failwith "pack returned empty sublist")
    (pack lst)

type 'a rle =
  | One of 'a
  | Many of int * 'a

let modified_encode lst =
  List.map
    (fun (count, v) -> if count = 1 then One v else Many (count, v))
    (encode lst)

let rec create_many v n =
  if n = 0 then [] else v :: create_many v (n - 1)

let rec decode = function
  | [] -> []
  | h :: t -> begin
      match h with
      | Many (count, v) -> create_many v count @ decode t
      | One v -> v :: decode t
    end

let rec duplicate = function
  | [] -> []
  | h :: t -> h :: h :: duplicate t

let rec replicate lst n =
  match lst with
  | [] -> []
  | h :: t -> create_many h n @ replicate t n

let drop lst n =
  let rec drop_aux pos = function
    | [] -> []
    | h :: t ->
        if pos = 1 then drop_aux n t else h :: drop_aux (pos - 1) t
  in
  drop_aux n lst
