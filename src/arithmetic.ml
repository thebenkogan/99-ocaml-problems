open Lists

let is_prime n =
  if n = 2 || n = 3 then true
  else
    n |> float_of_int |> sqrt |> int_of_float |> range 2
    |> List.for_all (fun x -> n mod x <> 0)

let rec gcd x y =
  if x = y then x else if x > y then gcd (x - y) y else gcd (y - x) x

let coprime x y = gcd x y = 1

let phi x =
  if x = 1 then 1
  else range 1 (x - 1) |> List.filter (coprime x) |> List.length

let factors n =
  let rec factors_aux d n =
    if n = 1 then []
    else if n mod d = 0 then d :: factors_aux d (n / d)
    else factors_aux (d + 1) n
  in
  factors_aux 2 n

let factors_multiplicity n =
  List.map
    (function
      | One x -> (x, 1)
      | Many (count, x) -> (x, count))
    (modified_encode (factors n))

let phi_improved n =
  n |> factors_multiplicity
  |> List.fold_left
       (fun acc (p, m) ->
         acc * (p - 1)
         * (float_of_int p ** float_of_int (m - 1) |> int_of_float))
       1

let timeit f x =
  let t = Sys.time () in
  let _ = f x in
  Format.sprintf "%fs" (Sys.time () -. t)

let all_primes low high = range low high |> List.filter is_prime

let goldbach n =
  range 3 n |> List.find (fun x -> is_prime x && is_prime (n - x))
  |> fun x -> (x, n - x)

let goldbach_list low high =
  range low high
  |> List.filter_map (fun n ->
         if n mod 2 = 0 then Some (n, goldbach n) else None)
