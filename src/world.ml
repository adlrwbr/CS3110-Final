exception IllegalWorld of string

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
      name = world.name;
      roads = world.roads;
      intersections = world.intersections;
      locations = new_loc :: world.locations;
    }
  in
  (new_loc, new_world)

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
    name = world.name;
    roads = road :: world.roads;
    intersections = List.rev_append (new_intersns [] world.roads) world.intersections;
    locations = world.locations;
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

(** [closest loc intersxns] is the closest intersection in [intersxns]
    to [loc] *)
let closest (loc : lt) (intersxns : Road.it list) : Road.it =
  raise (Failure "TODO: Unimplemented")
  (*
      List.fold_left (fun (closest : Road.it) (inter : Road.it) ->
        let inter_coords (i : Road.it) =
          match Road.inter_coords i.road1 i.road2 with
          | None -> raise (Failure "Invalid intersection.")
          | Some pair -> pair *)


(** [nextdoor_neighbors world node] is a list of all intersections and
    locations that immediately connect to [node] in the [world] *)
let nextdoor_neighbors (world : wt) (node : lit) : lit list =
  raise (Failure "TODO: for andrew :) ty")
      (*
  match node with
  | Loc loc ->
      let r = loc.road in
      (* intersections on the same road as loc *)
      let inters_on_road = List.filter (fun (inter : Road.it) -> r = inter.road1 || r = inter.road2) world.intersections in
      (* intersections on the same road as loc, closer to the end *)
      let inters_above = List.filter
      (fun (inter : Road.it) ->
        let pos = if inter.road1 = r then inter.pos_on_road1 else inter.pos_on_road2 in
        pos > loc.pos_on_road) inters_on_road in
      let inters_below = Algo.remove_all inters_on_road inters_above in
        if Algo.distance (loc_coord loc) (inter_coords inter) < Algo.distance (loc_coord loc) (inter_coords closest)
        then inter else closest
        
      ) (List.hd inters_above) inters_above


  | Inter inter -> raise (Failure "Unimplemented")
  *)

(** [reduce tbl world] is a graph representing the simplified state of the
    world where intersections and locations are nodes connected by edges (road
    segments). [tbl] maps the resulting graph's nodes' ids to the corresponding
    [lit]s in [world]. *)
let reduce (hashtbl : (int, lit) Hashtbl.t) (world : wt) : Graph.vgt =
  (** [spread acc queue] is a fully formed graph. [queue] is composed of pairs
      [(cur, prev)], where [prev] connects to [cur]'s value in [hashtbl],
      and [acc] is the accumulating graph *)
  let rec spread (acc : Graph.ugt) (queue : (lit * int option) list)
  : Graph.ugt =
    match queue with
    | [] -> acc
    | (cur, prev_id) :: t ->
      (* try to add node to graph *)
      let cur_id = next () in
      (* map Graph ids to [lit]s *)
      Hashtbl.add hashtbl cur_id cur;
      match Graph.add cur_id acc with
      (* [cur] is already in [acc] *)
      | exception Failure id ->
          spread acc t
      (* this node has not yet been seen *)
      | new_graph ->
          (* connect nodes if [prev_id] exists *)
          let new_graph =
            match prev_id with
            | None -> new_graph
            | Some prev_id ->
                let _ = assert (prev_id <> cur_id) in
                Graph.connect prev_id cur_id new_graph
          (* search around [next] for neighbor [lit]s *)
          in let neighbors = List.map (fun n -> (n, Some cur_id))
            (nextdoor_neighbors world cur) in
          spread new_graph (List.append neighbors queue)
  (* check if world has more than one location *)
  in if List.length world.locations = 0
  then Graph.verify Graph.empty
  else
    let loc = Loc (world.locations |> List.hd) in
    (* verify the graph *)
    let unverified = spread Graph.empty [loc, None]
      (* create a list of tuples for (loc, neighbor) for all locations *)
    in Graph.verify unverified

let rep_ok world =
  match reduce (Hashtbl.create 10) world with
  | exception _ -> false
  | _ -> true

let directions world start finish =
  assert (rep_ok world);
  (* convert to graph and find shortest path *)
  let hashtbl = Hashtbl.create 10 in
  let graph = reduce hashtbl world in
  let key_val_pairs = hashtbl |> Hashtbl.to_seq |> List.of_seq in
  let start_id = List.find (fun (_, node) -> node = Loc start) key_val_pairs
            |> fst in
  let finish_id = List.find (fun (_, node) -> node = Loc finish) key_val_pairs
            |> fst in
  let id_path = Algo.shortest_path start_id finish_id graph in
  (* convert ids back to [lit]s and return a [path] type *)
  List.map (fun id -> Hashtbl.find hashtbl id) id_path

let path_coords (p : path) =
  let lit_to_coords = fun (node : lit) : (float * float) ->
    match node with
    | Loc loc -> loc_coord loc
    | Inter inter ->
        begin
        match Road.inter_coords inter.road1 inter.road2 with
        | None -> raise (Failure "Invalid path: intersection does not exist.")
        | Some coord -> coord
        end
  in List.map lit_to_coords p
