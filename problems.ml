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

let split lst n =
  let rec split_aux acc n = function
    | [] -> (List.rev acc, [])
    | h :: t as l ->
        if n = 0 then (List.rev acc, l)
        else split_aux (h :: acc) (n - 1) t
  in
  split_aux [] n lst

let rec slice lst low high =
  match lst with
  | [] -> []
  | h :: t ->
      if low <> 0 then slice t (low - 1) (high - 1)
      else if high >= 0 then h :: slice t low (high - 1)
      else []

let rec rotate lst n =
  match lst with
  | [] -> []
  | h :: t -> if n = 0 then lst else rotate (t @ [ h ]) (n - 1)

let rec remove_at n = function
  | [] -> failwith "index out of bounds"
  | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t

let rec insert_at v n = function
  | [] -> failwith "index out of bounds"
  | h :: t as l -> if n = 0 then v :: l else h :: insert_at v (n - 1) t

let rec range a b =
  if a = b then [ a ]
  else if a < b then a :: range (a + 1) b
  else a :: range (a - 1) b

module IntSet = Set.Make (Int)

(* Assumes n <= length of lst *)
let rand_select lst n =
  let len = length lst in
  let rec get_random_indices max n set =
    if n = 0 then IntSet.elements set
    else
      let rand_val = Random.int max in
      if IntSet.mem rand_val set then get_random_indices max n set
      else get_random_indices max (n - 1) (IntSet.add rand_val set)
  in
  let random_indices =
    get_random_indices len n IntSet.empty |> List.sort compare
  in
  let rec extract_values indices idx = function
    | [] -> []
    | h :: t -> begin
        match indices with
        | [] -> []
        | i :: tl ->
            if i = idx then h :: extract_values tl (idx + 1) t
            else extract_values indices (idx + 1) t
      end
  in
  extract_values random_indices 0 lst
