exception UndefinedSlope

let rec relate f list = match list with
    | cur :: next :: more -> if f cur next then relate f (cur :: more) 
                            else relate f (next :: more)
    | cur :: [] -> cur
    | [] -> failwith "No elements"

let rec relate_option f list = match list with
    | cur :: next :: more -> if f cur next then relate_option f (cur :: more) 
                            else relate_option f (next :: more)
    | cur :: [] -> Some cur
    | [] -> None

let slope x1 y1 x2 y2 =
    let m = (y2 -. y1) /. (x2 -. x1) in
    if m |> abs_float = infinity then raise UndefinedSlope else m

let rec string_of_intl = function 
    | [] -> "" 
    | some :: [] -> (string_of_int some) 
    | some :: more-> (string_of_int some)^","^string_of_intl more

let distance p1 p2 =
    let x1, y1 = p1 in
    let x2, y2 = p2 in
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    sqrt (dx *. dx +. dy *. dy)

let in_range p p1 p2 = (p >= p1 && p <= p2) || (p >= p2 && p <= p1)

let rec remove_all list1 = function
    head :: tail -> remove_all (List.filter (fun x -> x <> head) list1) tail
    | [] -> list1

type edge = int * int * float
(**[edge] represents (id1,id2,weight), where [weight] is the 'distance'
 of [id2] from [id1]. Not necessarily bidirectional.*)

let src : edge -> int = function (src, _, _) -> src

let dest : edge -> int  = function (_, dest, _) -> dest

let dist  : edge -> float = function (_, _, dist) -> dist

let string_of_edge = function (e : edge) -> 
    "("^string_of_int(src(e))^"->"
    ^string_of_int(dest(e))^"="
    ^string_of_float(dist(e))^")"

let rec string_of_edge_list : edge list -> string = function 
    | [] -> "" 
    | edge :: more -> "("^string_of_edge edge^")" ^ string_of_edge_list more

let rec distance_before id visited_edges = 
    match visited_edges with
    | (_, before, dist_before) :: more ->
        if id = before 
            then dist_before 
            else distance_before id more
    | [] -> 
        if visited_edges = [] 
            then 0. 
            else failwith @@ "DIST_BEFORE..unknown id"^(string_of_int id)

let rec pathtrace start_id edge_list dead_ids look_for = match edge_list with
| (src,dest,dist) :: more -> if dest = look_for 
    then dest :: pathtrace start_id more (dest :: dead_ids) src
    else pathtrace start_id more (dead_ids) look_for
| [] -> [start_id]

let reduce_edge_list_to_path edge_list start_id end_id = 
    List.rev @@ pathtrace start_id edge_list [] end_id

let breadth_first f start_id end_id (graph : Graph.vgt) = 
    (**Helper functions.*)
    let min_of_id id dead_ids visited_edges =
        let edges = remove_all (Graph.neighbors graph id) dead_ids in 
            let prior = distance_before id visited_edges in
            let min = relate_option 
            (fun x y -> f id x +. prior < f id y +. prior)
            edges in
            match min with 
                | None -> None 
                | Some min -> Some (id, min, f id min +. prior) in
    let minimum_edge ids dead_ids edge_list : edge option = 
        let rec compile_minimums = function 
            | id :: more -> 
                (match min_of_id id dead_ids edge_list with 
                    | Some s -> s :: compile_minimums more
                    | None -> compile_minimums more)
            | [] -> [] in relate_option
            (fun e1 e2 -> dist(e1) < dist(e2)) (compile_minimums ids) in
    let rec dijkstras frontier dead_ids edge_list = 
    (
        match frontier with [] -> raise (Failure "DIJKSTRAS..id not found")
        | _ -> (match minimum_edge frontier dead_ids edge_list with
            Some e -> 
            if dest(e) = end_id then (e :: edge_list) else
            dijkstras ((dest(e)) :: frontier) ((dest(e)) :: dead_ids) 
                (e :: edge_list)
            | None -> raise 
            (Failure ("DIJKSTRA..no minimum..death occured @ node"^
            (string_of_int @@ List.hd frontier)))
        )
    ) in let djk = dijkstras [start_id] [start_id] [] in
    reduce_edge_list_to_path djk start_id end_id

let shortest_path start_id end_id (graph : Graph.vgt) = 
    breadth_first (Graph.weight graph) start_id end_id graph 

let distance_between start_id end_id (graph : Graph.vgt) = 
    let rec pair_accumulation = function
        e1 :: e2 :: [] -> Graph.weight graph e1 e2 
        | e1 :: e2 :: more -> Graph.weight graph e1 e2  +. pair_accumulation (e2 :: more)
        | _ :: [] -> raise (Failure "DIST_BTWN..single element") 
        | [] -> raise (Failure "DIST_BTWN..no elements")
        in
    let sequence = breadth_first (Graph.weight graph) start_id end_id graph in 
    pair_accumulation sequence
