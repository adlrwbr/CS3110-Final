exception IllegalWorld of string
exception RoadNameConflict of string
exception ParseError of string

open Printf
open Yojson.Basic.Util

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

(** [next] is the next integer in a 0-based counter *)
let next =
  let ctr = ref 0 in
  fun () ->
    incr ctr;
    !ctr

let road_of_json j =
  let name = j |> member "name" |> to_string in
  let parse_list (coords : float list) : (float * float) =
    if List.length coords <> 2 then failwith "Coordinate list must consist of two (x, y) float values"
    else (List.nth coords 0, List.nth coords 1)
  in
  let startPt = j |> member "start" |> to_list |> List.map (fun m -> to_number m) |> parse_list in
  let endPt = j |> member "end" |> to_list |> List.map (fun m -> to_number m) |> parse_list in
  Road.create name startPt endPt

let intersect_of_json (roads : Road.t list) j =
  let r1_name = j |> member "road1" |> to_string in
  let r2_name = j |> member "road2" |> to_string in
  try
    let r1 = List.find (fun r -> Road.name r = r1_name) roads in
    let r2 = List.find (fun r -> Road.name r = r2_name) roads in
    match Road.intersection r1 r2 with
    | None -> failwith (r1_name ^ " and " ^ r2_name ^ " do not intersect")
    | Some inter -> inter
  with
  | Not_found -> failwith "Road specified in intersections must also be in roads"

let loc_of_json (roads : Road.t list) j =
  let name = j |> member "name" |> to_string in
  let category = j |> member "category" |> to_string in
  let road_name = j |> member "road" |> to_string in
  let road =
    try List.find (fun r -> Road.name r = road_name) roads with
    | Not_found -> failwith "Road specified in location must also be in roads"
  in
  {
    id = next ();
    name = name;
    category = category;
    road = road;
    pos_on_road = j |> member "pos_on_road" |> to_number
  }

let world_of_json j =
  let name = j |> member "name" |> to_string in
  let roads = j |> member "roads" |> to_list |> List.map road_of_json in
  {
    name = name;
    roads = roads;
    intersections = j |> member "intersections" |> to_list |> List.map (intersect_of_json roads);
    locations = j |> member "locations" |> to_list |> List.map (loc_of_json roads)
  }

let from_json json =
  try world_of_json json with
  | Type_error (s, _) -> raise (ParseError s)
  | Failure s -> raise (ParseError s)

let road_to_json (road : Road.t) =
  let (x1, y1), (x2, y2) = Road.road_coords road in
  `Assoc [
    ("name", `String (Road.name road));
    ("start", `List [`Float x1; `Float y1]);
    ("end", `List [`Float x2; `Float y2]);
  ]

let intersect_to_json (inter : Road.it) =
  `Assoc [
    ("road1", `String (Road.name inter.road1));
    ("road2", `String (Road.name inter.road2));
  ]

let loc_to_json (loc : lt) =
  `Assoc [
    ("name", `String loc.name);
    ("category", `String loc.category);
    ("road", `String (Road.name loc.road));
    ("pos_on_road", `Float loc.pos_on_road)
  ]

let to_json (world : wt) =
  `Assoc [
    ("name", `String world.name);
    ("roads", `List (List.map road_to_json world.roads));
    ("intersections", `List (List.map intersect_to_json world.intersections));
    ("locations", `List (List.map loc_to_json world.locations));
  ]

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

let add_road (road : Road.t) world =
  let road_name = Road.name road in
  if world.roads |> List.exists (fun r -> Road.name r = road_name)
  then raise (RoadNameConflict road_name)
  else
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
let intersections world = world.intersections
let name (world : wt) = world.name
let rename w n = { w with name = n }

let loc_category loc = loc.category
let loc_name (loc : lt) = loc.name
let loc_coord loc =
  (* get the start and end coordinates of the location's road *)
  let road_start, road_end = Road.road_coords loc.road in
  (* calculate location's coordinates w/ pos_on_road *)
  match (road_start, road_end) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads

(**[nearest_pt_on_line fix ref_line] is the nearest point on [refline] to [fix]
or none if the perpendicular does not intersect. (fix beyond endpoint) *)
let nearest_pt_on_line fix ref_line =
  let ((s1,s2), ((p_a1_x, p_a1_y), (p_a2_x, p_a2_y))) = fix, Road.road_coords ref_line in
  let m_a = -.(try Algo.slope p_a1_y p_a1_x p_a2_y p_a2_x with
  | Algo.UndefinedSlope -> 1000.) in
  let source1 = (s1 -. 10000.,s2-.(10000.*.m_a)) in (**10000 should be good enough.*)
  let source2 = (s1 +. 10000.,s2+.(10000.*.m_a)) in
  let r = Road.create "" source1 source2 in
  (* [r] is perpendicular to [ref_line] *)
  match Road.intersection r ref_line with
  | None -> None
  | Some inter -> Some (Road.inter_coord inter)

(**[seg_dist] is the minimum distance of [source] to the segment [road] *)
let seg_dist source road =
    let maybe = nearest_pt_on_line source road in
    (match maybe with
    | None -> min
    (Algo.distance source (road |> Road.road_coords |> fst))
    (Algo.distance source (road |> Road.road_coords |> snd))
    | Some (x,y) ->
    min
      (min
        (Algo.distance source (road |> Road.road_coords |> fst))
        (Algo.distance source (road |> Road.road_coords |> snd)))
      (Algo.distance source (x,y))
     )

let nearroad source world =
  let rel d1 d2 = ((seg_dist source d1)) < ((seg_dist source d2)) in
    let minrd = Algo.relate rel (roads world) in
    let c1, c2 = Road.road_coords minrd in
    let insct = nearest_pt_on_line source minrd in
    let ordp = (match insct with
    | Some (x,y) ->
    if (Algo.distance source (x,y)) <
        Algo.distance source c1
    then
      if (Algo.distance source (x,y) < Algo.distance source c2)
      then (x,y)
      else c2
    else
      if (Algo.distance source c1
              < Algo.distance source c2)
      then c1
      else c2
    | None ->
    if (Algo.distance source c1)
    < (Algo.distance source c2)
    then c1
    else c2)
    in
    let interp_dist =
    Algo.distance ordp c1 /.
    Algo.distance c1 c2
    in (interp_dist, minrd)

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
  (* let _ = List.iter (printf "%d ") id_path in *)
  (* convert ids back to [lit]s and return a [path] type *)
  List.map (fun id -> Hashtbl.find id_to_lit id) id_path

let path_coords (p : path) = List.map lit_coord p
