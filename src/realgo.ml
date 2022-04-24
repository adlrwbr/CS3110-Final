(** [remove_all list1 list2] is [list1] without the elements of [list2]*)
let rec remove_all (list1 : 'a list) (list2 : 'a list) : 'a list = 
    match list2 with 
    | head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
    | [] -> list1


let breadth_first (graph : Graph.vgt) start_id end_id distance_f = 
    (**Minimum of every node to every other node in neighbors*)
    (**Minimum of these minimums across nodes. *)
    let string_of_triplet = function (x,y,z) -> 
    string_of_int(x)^":"^string_of_int(y)^"="^string_of_float(z) in 
    let rec string_of_intl = function [] -> 
    "" | some :: more-> (string_of_int some)^","^string_of_intl more in 
    let src_of = function (src, _, _) -> src in
    let dest_of = function (_, dest, _) -> dest in
    let distance_of = function (_, _, dist) -> dist in
    let source_minimum id memory = 
        let edges = remove_all (Graph.neighbors graph id) memory in 
            let min = Algo.relate (fun x y -> distance_f id x < distance_f id y) edges in
            (id, min, distance_f id min) 
        in
    let rec compile_minimums ids memory = match ids with 
    | id :: more -> source_minimum id memory :: compile_minimums more memory
    | [] -> [] 
    in
    let global_minimum id_minimums = Algo.relate 
    (fun c1 c2 -> distance_of c1 < distance_of c2) id_minimums 
    in
    let minimum ids memory = 
        global_minimum (compile_minimums ids memory) in
    let rec dijkstras frontier memory heap = (
        match frontier with
        | [] -> raise (Failure "Terminated without finding end_id.")
        | _ -> 
        let min = minimum frontier memory in
            let _ = print_endline 
            (string_of_triplet min^
            ":["^string_of_intl frontier^"]:["
            ^string_of_intl memory^"]") in
        dijkstras 
        (**New frontier *)
        (Graph.neighbors graph (src_of min) @ frontier)
        (**New memory *)
        (src_of min :: memory)
        (**New heap *)
        heap
    ) 
    in
    dijkstras [start_id] [start_id] []

(** Test graph. 
let myg12 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5 |> add 6 |> add 7 |> add 8 |> add 9 |> add 10 |> add 11 |> add 12 |> connect 1 2 |> connect 1 3 |> connect 2 4 |> connect 2 5 |> connect 3 5 |> connect 3 6 |> connect 4 7 |> connect 4 8 |> connect 5 8 |> connect 5 9 |> connect 6 7 |> connect 6 8 |> connect 7 11 |> connect 9 10 |> connect 10 12 |> connect 11 12 |> verify;; *)
        





