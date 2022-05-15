let rec remove_all (list1 : 'a list) (list2 : 'a list) : 'a list = 
    match list2 with 
    | head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
    | [] -> list1

let rec relate_option f list = match list with
    | cur :: next :: more -> if f cur next then relate_option f (cur :: more) 
                            else relate_option f (next :: more)
    | cur :: [] -> Some cur
    | [] -> None



let breadth_first (graph : Graph.vgt) start_id end_id distance_f = 
    let debugging = false in
    (**Helper functions.*)
        let string_of_triplet = function (x,y,z) -> 
            string_of_int(x)^":"^string_of_int(y)^"="^string_of_float(z) in 
        let rec string_of_intl = function [] -> 
            "" | some :: [] -> (string_of_int some) |
            some :: more-> (string_of_int some)^","^string_of_intl more in 
        let rec string_of_heap = function [] -> 
            "" | triplet :: more -> "("^string_of_triplet triplet^")" ^ string_of_heap more in
        let src_of = function (src, _, _) -> src in
        let dest_of = function (_, dest, _) -> dest in
        let distance_of = function (_, _, dist) -> dist in
        let source_minimum id memory =  (*Remove all destinations in memory then find the minimum. *)
            let edges = remove_all (Graph.neighbors graph id) memory in 
                let min = relate_option (fun x y -> distance_f id x < distance_f id y) edges in
                match min with None -> None | Some min -> Some (id, min, distance_f id min) 
            in
        let rec compile_minimums ids memory = match ids with 
            | id :: more -> 
            (match source_minimum id memory with 
                Some s -> s :: compile_minimums more memory
                | None -> compile_minimums more memory)
            | [] -> [] in
        let global_minimum id_minimums = relate_option 
            (fun c1 c2 -> distance_of c1 < distance_of c2) id_minimums in
        let minimum ids memory = 
            global_minimum (compile_minimums ids memory) in
        let rec pathtrace heap memory look_for = match heap with
        | (src,dest,dist) :: more -> 
        if dest = look_for then dest :: pathtrace more (dest :: memory) src
        else pathtrace more (memory) look_for
        | [] -> [start_id]
        in
        let reduce_heap heap = 
            List.rev @@ pathtrace heap [] end_id
        in 
    (**End helper.*)
    let rec dijkstras frontier memory heap = (
        match frontier with [] -> raise (Failure "DIJKSTRAS..id not found")
        | _ -> (match minimum frontier memory with
            Some min ->
            if dest_of min = end_id then (min :: heap) else
            (**Debugging *)
                let _ = if debugging then print_endline @@ "====cycle_of{"^string_of_triplet min^"}====" in
                let _ = if debugging then print_endline @@ "FTR  ["^string_of_intl frontier^"]" in
                let _ = if debugging then print_endline @@ "MEM  ["^string_of_intl memory^"]" in
                let _ = if debugging then print_endline @@ "HEP  ["^string_of_heap heap^"]" in
            dijkstras
                ((dest_of min) :: frontier)
                ((dest_of min) :: memory) 
                (min :: heap)
            | None -> raise (Failure ("DIJKSTRA..no minimum..death occured @ node"^(string_of_int @@ List.hd frontier)))
        )
    ) 
    in
    let djk = dijkstras [start_id] [start_id] [] in
    let _ = print_endline @@ string_of_heap djk in
    reduce_heap djk

(** Test graph.
let myg12 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5 |> add 6 |> add 7 |> add 8 |> add 9 |> add 10 |> add 11 |> add 12 |> connect 1 2 0.5 |> connect 1 3 0.5 |> connect 2 4 0.5 |> connect 2 5 0.5 |> connect 3 5 0.5 |> connect 3 6 0.5 |> connect 4 7 0.5 |> connect 4 8 0.5 |> connect 5 8 0.5 |> connect 5 9 0.5 |> connect 6 7 0.5 |> connect 6 8 0.5 |> connect 7 11 0.5 |> connect 9 10 0.5 |> connect 10 12 0.5 |> connect 11 12 0.5 |> verify;; *)

