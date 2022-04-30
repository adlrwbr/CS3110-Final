exception IllegalWorld of string
open Printf

type lt = {
  id : int;
  name : string;
  category : string;
  road : Road.t;
  pos_on_road : float;
}

type wt = {
  name : string;
  roads : Road.t list;
  intersections : Road.it list;
  locations : lt list;
}

type lit = Loc of lt | Inter of Road.it
(** a type that represents a location or an intersection *)

type path = lit list

let size_x = 1000.
let size_y = 1000.
let empty name = { name; roads = []; intersections = []; locations = [] }

let distance pt1 pt2 =
  match (pt1, pt2) with
  | (a, b), (c, d) ->
      sqrt (((a -. c) *. (a -. c)) +. ((b -. d) *. (b -. d)))

(** [next] is the next integer in a 0-based counter *)
let next =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

let add_loc name category road pos world =
  (* create location *)
  let new_loc = { id = next (); name; category; road; pos_on_road = pos } in
  (* add location to world *)
  let new_world =
    {
      world with
      locations = new_loc :: world.locations;
    }
  in
  (new_loc, new_world)

let delete_loc world loc  = {world with locations = List.filter (fun l -> l != loc) world.locations}

let add_road road world =
  (* check for intersections with new road and all existing roads *)
  let rec new_intersns acc rd_lst =
    match rd_lst with
    | [] -> acc
    | road2 :: t ->
        match Road.intersection road road2 with
        | None -> new_intersns acc t
        | Some intersn -> new_intersns (intersn :: acc) t
  in
  {
    world with
    roads = road :: world.roads;
    intersections = List.rev_append (new_intersns [] world.roads) world.intersections;
  }

let delete_road world road =
  {
    world with
    roads = List.filter (fun r -> r != road) world.roads;
    locations = List.filter (fun l -> l.road != road) world.locations;
    intersections = List.filter (fun (i : Road.it) -> i.road1 != road && i.road2 != road) world.intersections;
  }

let locations world = world.locations
let name (loc : lt) = loc.name
let category loc = loc.category

let loc_coord loc =
  (* get the start and end coordinates of the location's road *)
  let road_start, road_end = Road.road_coords loc.road in
  (* calculate location's coordinates w/ pos_on_road *)
  match (road_start, road_end) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads

let inty road1 road2 =
    match (road1, road2) with (**Copies [intersection] from [world.ml] code w/o typechecking.*)
    | ( ((p_a1_x, p_a1_y), (p_a2_x, p_a2_y)),
        ((p_b1_x, p_b1_y), (p_b2_x, p_b2_y)) ) ->
        let m_a = Algo.slope p_a1_x p_a1_y p_a2_x p_a2_y in
        let m_b = Algo.slope p_b1_x p_b1_y p_b2_x p_b2_y in
        (* x and y below is the intersection point of the two lines that
          the segments lie on. *)
        let x =
          (p_b1_y -. p_a1_y +. (m_a *. p_a1_x) -. (m_b *. p_b1_x))
          /. (m_a -. m_b)
        in
        let y = (m_b *. (x -. p_b1_x)) +. p_b1_y in
        (* Check if the intersection point is actually on the segments. *)
        if
          Algo.in_range x p_a1_x p_a2_x
          && Algo.in_range x p_b1_x p_b2_x
          && Algo.in_range y p_a1_y p_a2_y
          && Algo.in_range y p_b1_y p_b2_y
        then Some (x, y)
        else None

(**[nearest_pt_on_line fix ref_line] is the nearest point on [refline] to [fix]
or none if the perpendicular does not intersect. (fix beyond endpoint) *)
let nearest_pt_on_line fix ref_line =
(match fix, Road.road_coords ref_line with
    ((s1,s2),((p_a1_x, p_a1_y), (p_a2_x, p_a2_y))) ->
    let m_a = -.(Algo.slope p_a1_y p_a1_x p_a2_y p_a2_x) in
    let source1 = (s1 -. 10000.,s2-.(10000.*.m_a))in (**Should be good enough.*)
    let source2 = (s1 +. 10000.,s2+.(10000.*.m_a))in
    inty (source1,source2) (Road.road_coords ref_line)
    )

(**[seg_dist] is the minimum distance of [source] to the segment [road] *)
let seg_dist source road =
    let maybe = nearest_pt_on_line source road in
    (match maybe with
    | None -> min
    (distance source (road |> Road.road_coords |> fst))
    (distance source (road |> Road.road_coords |> snd))
    | Some (x,y) ->
    min
      (min
        (distance source (road |> Road.road_coords |> fst))
        (distance source (road |> Road.road_coords |> snd)))
      (distance source (x,y))
     )

let nearroad source world =
  let rel d1 d2 = ((seg_dist source d1)) < ((seg_dist source d2)) in
    let minrd = Algo.relate rel (roads world) in
    let insct = nearest_pt_on_line source minrd in
    let ordp = (match insct with
    | Some (x,y) ->
    if (distance source (x,y)) <
        distance source (fst (Road.road_coords minrd))
    then
      if (distance source (x,y) < distance source (snd (Road.road_coords minrd)))
      then (x,y)
      else (snd (Road.road_coords minrd))
    else
      if (distance source (fst (Road.road_coords minrd))
              < distance source (snd (Road.road_coords minrd)))
      then (fst (Road.road_coords minrd))
      else (snd (Road.road_coords minrd))
    | None ->
    if (distance source (fst (Road.road_coords minrd)))
    < (distance source (snd (Road.road_coords minrd)))
    then fst (Road.road_coords minrd)
    else snd (Road.road_coords minrd))
    in
    let interp_dist =
    distance ordp (fst (Road.road_coords minrd))/.
    distance (fst (Road.road_coords minrd)) (snd (Road.road_coords minrd))
    in
    (interp_dist, minrd)

let roads_at_coord coord world = match nearroad coord world with 
| (_, r) -> [r]

let locs_at_coord coord world = raise (Failure "unimplemented")

(** [lit_coord inter_loc] is the (x, y) coordinate of [inter_loc] *)
let lit_coord = function
  | Loc loc -> loc_coord loc
  | Inter inter -> Road.inter_coord inter

(** [closest loc intersections_locs] is the closest intersection/location (whichever is closer) in [intersections_locs]
    to [loc] *)
let closest (coord : float * float) (intersections_locs : lit list) : lit =
  (List.fold_left (fun (closest : lit) (inter_loc : lit) -> 
    if (Algo.distance coord (lit_coord inter_loc) < Algo.distance coord (lit_coord closest))
      then (inter_loc) else (closest)
  ) (List.hd intersections_locs) intersections_locs)

(** [nextdoor_neighbors world node] is a list of all intersections and
    locations that immediately connect to [node] in the [world] *)
let nextdoor_neighbors (world : wt) (node : lit) : lit list =
  let helper coord pos_on_road road =
    let r = road in
      (* intersections on the same road as loc *)
      let inters_on_road = List.filter (fun (inter : Road.it) -> r = inter.road1 || r = inter.road2) world.intersections in
      (* intersections on the same road as loc, closer to the end *)
      let inters_above = List.filter
      (fun (inter : Road.it) ->
        let pos = if inter.road1 = r then inter.pos_on_road1 else inter.pos_on_road2 in
        pos > pos_on_road) inters_on_road in
      let inters_below = List.filter
      (fun (inter : Road.it) ->
        let pos = if inter.road1 = r then inter.pos_on_road1 else inter.pos_on_road2 in
        pos < pos_on_road) inters_on_road in
      (* locations on the same road as loc *)
      let locs_on_road = List.filter (fun (l : lt) -> r = l.road) world.locations in
      (* locations on the same road as loc, closer to the end *)
      let locs_above = List.filter
      (fun (l : lt) ->
        l.pos_on_road > pos_on_road) locs_on_road in
      let locs_below = List.filter
      (fun (l : lt) ->
        l.pos_on_road < pos_on_road) locs_on_road in
      (* Convert all intersections and locations into lit type and merge them into one list *)
      let above = (List.map (fun i -> Inter i) inters_above) @ (List.map (fun l -> Loc l) locs_above) in
      let below = (List.map (fun i -> Inter i) inters_below) @ (List.map (fun l -> Loc l) locs_below) in
      (* Check to see if there are nodes above and below. For the cases where there are no nodes above/below, do not
         attempt to find the closest location/intersection. *)
      (if List.length above = 0 then [] else [closest coord above]) @ (if List.length below = 0 then [] else [closest coord below])
  in
  let neighbors =
  match node with
  | Loc loc -> helper (loc_coord loc) loc.pos_on_road loc.road
  | Inter inter ->
      helper (Road.inter_coord inter) inter.pos_on_road1 inter.road1
      @ helper (Road.inter_coord inter) inter.pos_on_road2 inter.road2
  in neighbors |> List.sort_uniq compare

(** [reduce_aux world] is a tuple containing an unverified graph form of
    [world] and a map from that graph's ids to the [world]'s [lit]s *)
let reduce_aux (world : wt) : (Graph.ugt * (int, lit) Hashtbl.t ) =
  let id_to_lit = Hashtbl.create 10 in
  let seen = Hashtbl.create 10 in
  (** [spread acc queue] is a fully formed graph. Every node in [queue] has been
      "seen" or added to the graph [acc]. *)
  let rec spread (acc : Graph.ugt) (queue : lit list) : Graph.ugt =
      if not (List.for_all (fun n -> Hashtbl.mem seen n) queue) then (* TODO: comment out for optimization *)
        failwith "Loop invariant: all nodes in queue must have been added to the graph"
      else
      match queue with
      | [] -> acc
      | cur :: t ->
        let cur_id = Hashtbl.find seen cur in
        let neighbors = nextdoor_neighbors world cur in
        let t = ref t in
        (* for all neighbors *)
        let _ = List.map (fun n ->
          (* if neighbor hasn't been seen, add to seen, graph, and append to queue *)
          (if not (Hashtbl.mem seen n) then 
            let id = next () in
            Hashtbl.add seen n id;
            Hashtbl.add id_to_lit id n;
            (* add neighbor to graph and queue *)
            Graph.add id acc;
            t := n :: !t);
          (* connect neighbor to cur *)
          let n_id = Hashtbl.find seen n in
          let distance = Algo.distance (lit_coord cur) (lit_coord n) in
          Graph.connect cur_id n_id distance acc
        ) neighbors in
        (* recursive call *)
        spread acc !t
  (* spread from the first intersection *)
  in
  let inter = Inter (world.intersections |> List.hd) in
  let id = next () in
  Hashtbl.add seen inter id;
  Hashtbl.add id_to_lit id inter;
  let g = Graph.empty () in Graph.add id g;
  let result = spread g [inter] in
  (* "Total nodes seen: " ^ (Hashtbl.length seen |> Int.to_string) |> print_endline; *)
  (result, id_to_lit)

(** [reduce world] is a tuple containing a graph representing the simplified
    [world] where intersections and locations are nodes connected by edges
    (road segments) and a map from that graph's ids to the [world]'s [lit]s *)
let reduce (world : wt) : (Graph.vgt * (int, lit) Hashtbl.t ) =
    (* all roads must have an intersection on them, unless there is only 1 *)
    (* guard against floating roads *)
    let has_inter road = List.exists (fun (inter : Road.it) -> inter.road1 = road || inter.road2 = road) world.intersections in
    let isolated = not @@ List.for_all has_inter world.roads in
    if List.length world.roads > 1 && isolated then
      failwith "Invalid world! At least one road is isolated."
    else if List.length world.intersections > 1 then
      (* construct a graph *)
      let unverified, id_to_lit = reduce_aux world in
      (* guard against graph components (floating networks of roads) *)
      if (Graph.size unverified != List.length world.intersections + List.length world.locations) then
        failwith ("Invalid world! There are " ^ (List.length world.intersections |> Int.to_string) ^ " intersections and " ^ (List.length world.locations |> Int.to_string) ^ " locations, but the reduced graph has " ^ (Graph.size unverified |> Int.to_string) ^ " nodes.")
      else if (Graph.size unverified != Hashtbl.length id_to_lit) then
        failwith ("Invalid world! There are " ^ (List.length world.intersections |> Int.to_string) ^ " intersections and " ^ (List.length world.locations |> Int.to_string) ^ " locations, but after reducing the map id_to_lit has " ^ (Hashtbl.length id_to_lit |> Int.to_string) ^ " bindings.")
      (* verify the graph *)
      else Graph.verify unverified, id_to_lit
    else Graph.verify @@ Graph.empty (), Hashtbl.create 0

let rep_ok world =
  match reduce world with
  | exception e ->
      (* e |> Printexc.to_string |> print_endline; *)
      false
  | _ -> true

let directions world start finish =
  (* convert to graph and find shortest path *)
  match reduce world with
  | exception _ -> failwith "Cannot calculate directions for invalid world!"
  | graph, id_to_lit ->
  let key_val_pairs = id_to_lit |> Hashtbl.to_seq |> List.of_seq in
  let start_id = List.find (fun (_, node) -> node = Loc start) key_val_pairs
            |> fst in
  let finish_id = List.find (fun (_, node) -> node = Loc finish) key_val_pairs
            |> fst in
  let id_path = Algo.shortest_path start_id finish_id graph in
  (* print id_path, for debugging purposes only*)
  let _ = List.iter (printf "%d ") id_path in
  (* convert ids back to [lit]s and return a [path] type *)
  List.map (fun id -> Hashtbl.find id_to_lit id) id_path

let path_coords (p : path) = List.map lit_coord p
