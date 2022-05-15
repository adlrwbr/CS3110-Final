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
    if x1 = x2 then raise UndefinedSlope
    else (y2 -. y1) /. (x2 -. x1)

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

let src_of = function (src, _, _) -> src

let dest_of = function (_, dest, _) -> dest

let dist_of = function (_, _, dist) -> dist

let string_of_edge = function (e : edge) -> 
    string_of_int(src_of e)^"->"
    ^string_of_int(dest_of e)^"="
    ^string_of_float(dist_of e)

let breadth_first (graph : Graph.vgt) start_id end_id distance_f = 
    let debugging = true in
    (**Helper functions.*)
        let rec string_of_heap = function 
            | [] -> "" 
            | edge :: more -> "("^string_of_edge edge^")" ^ string_of_heap more in
        let rec distance_before id visited_edges = 
            match visited_edges with
            | (_, before, dist_before) :: more ->
                if id = before 
                    then dist_before 
                    else distance_before id more
            | [] -> 
                if visited_edges = [] 
                    then 0. 
                    else failwith @@ "DIST_BEFORE..unknown id"^(string_of_int id) in
        let source_minimum id memory visited_edges =  (*Remove all destinations in memory then find the minimum. *)
            let edges = remove_all (Graph.neighbors graph id) memory in 
                let prior = distance_before id visited_edges in
                let min = relate_option (fun x y -> distance_f id x +. prior < distance_f id y +. prior) edges in
                match min with None -> None | Some min -> Some (id, min, distance_f id min +. prior) 
            in
        let rec compile_minimums ids memory heap = match ids with 
            | id :: more -> 
            (match source_minimum id memory heap with 
                Some s -> s :: compile_minimums more memory heap
                | None -> compile_minimums more memory heap)
            | [] -> [] in
        let minimum_edge ids memory heap : edge option = 
            relate_option 
            (fun e1 e2 -> dist_of e1 < dist_of e2)
            (compile_minimums ids memory heap) in
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
        | _ -> (match minimum_edge frontier memory heap with
            Some min ->
            if dest_of min = end_id then (min :: heap) else
            (**Debugging *)
                let _ = if debugging then (print_endline @@ "====cycle_of{"^string_of_edge min^"}====";
                print_endline @@ "Frontier  ["^string_of_intl frontier^"]";
                print_endline @@ "Memory  ["^string_of_intl memory^"]";
                print_endline @@ "Heap  ["^string_of_heap heap^"]") in
            dijkstras
                ((dest_of min) :: frontier)
                ((dest_of min) :: memory) 
                (min :: heap)
            | None -> raise (Failure ("DIJKSTRA..no minimum..death occured @ node"^(string_of_int @@ List.hd frontier)))
        )
    ) 
    in
    let djk = dijkstras [start_id] [start_id] [] in
    (*let _ = print_endline @@ string_of_heap djk in *)
    reduce_heap djk

(**Test graph:
let myg = empty();;
let _ = add_many [1;2;3;4;5;6;7] myg;;
let _ = 
    connect 1 2 0.3 myg;
    connect 2 4 0.3 myg;
    connect 4 5 0.3 myg;
    connect 5 6 0.3 myg;
    connect 6 7 0.3 myg;
    connect 2 5 0.5 myg;
    connect 5 7 0.5 myg;
    connect 1 3 0.6 myg;
    connect 3 7 0.6 myg;;
let myg = verify myg;;
 *)

let shortest_path start finish graph = breadth_first graph start finish Graph.weight

let distance_between graph id1 id2 distance_f = 
    let rec pair_accumulation = function
        e1 :: e2 :: [] -> distance_f e1 e2
        | e1 :: e2 :: more -> distance_f e1 e2 +. pair_accumulation (e2 :: more)
        | _ :: [] -> raise (Failure "DIST_BTWN..single element") 
        | [] -> raise (Failure "DIST_BTWN..no elements")
        in
    let sequence = breadth_first graph id1 id2 distance_f in 
    pair_accumulation sequence
