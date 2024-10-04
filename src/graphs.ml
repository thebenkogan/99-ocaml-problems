type 'a graph = ('a, 'a list) Hashtbl.t

let paths g a b =
  let seen = Hashtbl.create 100 in
  Hashtbl.add seen b ();
  let q = Queue.create () in
  Queue.add (b, [ b ], seen) q;
  let rec dfs paths =
    match Queue.take_opt q with
    | Some (v, path, seen) ->
        if v = a then dfs (path :: paths)
        else begin
          List.iter
            (fun adj ->
              if not (Hashtbl.mem seen adj) then begin
                let next_seen = Hashtbl.copy seen in
                Hashtbl.add next_seen adj ();
                Queue.add (adj, adj :: path, next_seen) q
              end)
            (Hashtbl.find g v);
          dfs paths
        end
    | None -> paths
  in
  dfs []
