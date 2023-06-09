let rec last = function
  | [] -> None
  | [ h ] -> Some h
  | _ :: t -> last t

let rec last_two = function
  | [ _ ]
  | [] ->
      None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t

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

let rand_select lst n =
  let len = length lst in
  let rec rand_idxs_with_replacement max n acc =
    if n = 0 then acc
    else rand_idxs_with_replacement max (n - 1) (Random.int max :: acc)
  in
  let random_indices = rand_idxs_with_replacement len n [] in
  List.map (List.nth lst) random_indices

let lotto_select n m = rand_select (range 1 m) n

module IntSet = Set.Make (Int)

let permutation lst =
  let len = length lst in
  let rec permuatation_aux n idx_set acc =
    if n = 0 then acc
    else
      let rand_idx = Random.int len in
      if IntSet.mem rand_idx idx_set then permuatation_aux n idx_set acc
      else
        permuatation_aux (n - 1)
          (IntSet.add rand_idx idx_set)
          (List.nth lst rand_idx :: acc)
  in
  permuatation_aux len IntSet.empty []

let rec extract n lst =
  if n = 1 then List.map (fun x -> [ x ]) lst
  else
    let rec extract_aux acc = function
      | [] -> acc
      | h :: t ->
          let sets =
            List.map (fun set -> h :: set) (extract (n - 1) t)
          in
          extract_aux (sets @ acc) t
    in
    extract_aux [] lst

let group lst sizes =
  let subset_groups = List.map (fun size -> extract size lst) sizes in
  let is_disjoint (lst1 : 'a list) (lst2 : 'a list) =
    let contains_elt lst v = List.exists (fun x -> x = v) lst in
    List.for_all (fun x -> not (contains_elt lst2 x)) lst1
  in
  let is_lst_disjoint_from_sets (lst : 'a list) (sets : 'a list list) =
    List.for_all (is_disjoint lst) sets
  in
  let rec group_aux acc = function
    | [] -> acc
    | subsets :: t ->
        let acc' =
          subsets
          |> List.map (fun subset ->
                 acc
                 |> List.filter (fun djs ->
                        is_lst_disjoint_from_sets subset djs)
                 |> List.map (fun djs -> subset :: djs))
          |> List.flatten
        in
        group_aux acc' t
  in
  group_aux [ [] ] subset_groups

let length_sort lst =
  List.sort (fun lst1 lst2 -> compare (length lst1) (length lst2)) lst

module IntMap = Map.Make (Int)

let frequency_sort lst =
  let len = lst |> length |> float_of_int in
  let count_map =
    List.fold_left
      (fun acc sublist ->
        let sublist_len = length sublist in
        match IntMap.find_opt sublist_len acc with
        | None -> IntMap.add sublist_len 1. acc
        | Some count -> IntMap.add sublist_len (count +. 1.) acc)
      IntMap.empty lst
  in
  let freq_map = IntMap.map (fun count -> count /. len) count_map in
  List.sort
    (fun lst1 lst2 ->
      compare
        (IntMap.find (length lst1) freq_map)
        (IntMap.find (length lst2) freq_map))
    lst
