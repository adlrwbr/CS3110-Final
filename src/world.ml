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
  g : Graph.ugt; (* represents the world in simplified graph form *)
  roads : Road.t list;
  locations : lt list;
}

type lit = Loc of lt | Inter of Road.intersection
(** a type that represents a location or an intersection *)

type path = lit list

let size_x = 1000.
let size_y = 1000.
let empty name = { name; g = Graph.empty; roads = []; locations = [] }

let midpt road =
  let fp = road |> Road.coords |> fst in
  let lp = road |> Road.coords |> snd in
  match (fp, lp) with
  | (a, b), (c, d) -> ((a +. c) /. 2., (b +. d) /. 2.)

let distance pt1 pt2 =
  match (pt1, pt2) with
  | (a, b), (c, d) ->
      sqrt (((a -. c) *. (a -. c)) +. ((b -. d) *. (b -. d)))

let add_loc name category road pos world =
  raise (Failure "Fix add_loc")
  (* TODO: deal with counter *)
  (*
  match Graph.add world.g with
  | nid, ng ->
      (* create location *)
      let new_loc =
        { id = nid; name; category; road; pos_on_road = pos }
      in
      (* add location to world *)
      let new_world =
        {
          name = world.name;
          g = ng;
          roads = world.roads;
          locations = new_loc :: world.locations;
        }
      in
      (new_loc, new_world)
    *)

let add_road road world =
  {
    name = world.name;
    g = world.g;
    roads = road :: world.roads;
    locations = world.locations;
  }

let locations world = world.locations
let name (loc : lt) = loc.name
let category loc = loc.category

let loc_coord loc =
  (* get the start and end coordinates of the location's road *)
  let road_start, road_end = Road.coords loc.road in
  (* calculate location's coordinates w/ pos_on_road *)
  match (road_start, road_end) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads
let slope x1 y1 x2 y2 = (y2 -. y1) /. (x2 -. x1)
let in_range p p1 p2 = (p >= p1 && p <= p2) || (p >= p2 && p <= p1)

(** [reduce world] is a graph representing the simplified state of the world
    where intersections and locations are nodes connected by roads *)
let reduce world = Graph.verify Graph.empty (* TODO: implement *)

let intersection road1 road2 =
  match (Road.coords road1, Road.coords road2) with
  | ( ((p_a1_x, p_a1_y), (p_a2_x, p_a2_y)),
      ((p_b1_x, p_b1_y), (p_b2_x, p_b2_y)) ) ->
      let m_a = slope p_a1_x p_a1_y p_a2_x p_a2_y in
      let m_b = slope p_b1_x p_b1_y p_b2_x p_b2_y in
      (* x and y below is the intersection point of the two lines that
         the segments lie on. *)
      let x =
        (p_b1_y -. p_a1_y +. (m_a *. p_a1_x) -. (m_b *. p_b1_x))
        /. (m_a -. m_b)
      in
      let y = (m_b *. (x -. p_b1_x)) +. p_b1_y in
      (* Check if the intersection point is actually on the segments. *)
      if
        in_range x p_a1_x p_a2_x
        && in_range x p_b1_x p_b2_x
        && in_range y p_a1_y p_a2_y
        && in_range y p_b1_y p_b2_y
      then Some (x, y)
      else None

let inty road1 road2 =
    match (road1, road2) with (**Copies [intersection] from [world.ml] code w/o typechecking.*)
    | ( ((p_a1_x, p_a1_y), (p_a2_x, p_a2_y)),
        ((p_b1_x, p_b1_y), (p_b2_x, p_b2_y)) ) ->
        let m_a = slope p_a1_x p_a1_y p_a2_x p_a2_y in
        let m_b = slope p_b1_x p_b1_y p_b2_x p_b2_y in
        (* x and y below is the intersection point of the two lines that
          the segments lie on. *)
        let x =
          (p_b1_y -. p_a1_y +. (m_a *. p_a1_x) -. (m_b *. p_b1_x))
          /. (m_a -. m_b)
        in
        let y = (m_b *. (x -. p_b1_x)) +. p_b1_y in
        (* Check if the intersection point is actually on the segments. *)
        if
          in_range x p_a1_x p_a2_x
          && in_range x p_b1_x p_b2_x
          && in_range y p_a1_y p_a2_y
          && in_range y p_b1_y p_b2_y
        then Some (x, y)
        else None

(**[nearest_pt_on_line fix ref_line] is the nearest point on [refline] to [fix]
or none if the perpendicular does not intersect. (fix beyond endpoint) *)
let nearest_pt_on_line fix ref_line = 
(match fix, Road.coords ref_line with
    ((s1,s2),((p_a1_x, p_a1_y), (p_a2_x, p_a2_y))) ->
    let m_a = -.(slope p_a1_y p_a1_x p_a2_y p_a2_x) in
    let source1 = (s1 -. 10000.,s2-.(10000.*.m_a))in (**Should be good enough.*)
    let source2 = (s1 +. 10000.,s2+.(10000.*.m_a))in
    inty (source1,source2) (Road.coords ref_line)
    )

(**[seg_dist] is the minimum distance of [source] to the segment [road] *)
let seg_dist source road =   
    let maybe = nearest_pt_on_line source road in
    (match maybe with
    | None -> min 
    (distance source (road |> Road.coords |> fst)) 
    (distance source (road |> Road.coords |> snd))
    | Some (x,y) -> 
    min 
      (min 
        (distance source (road |> Road.coords |> fst))
        (distance source (road |> Road.coords |> snd)))
      (distance source (x,y))  
     )

let nearroad source world = 
  let rel d1 d2 = ((seg_dist source d1)) < ((seg_dist source d2)) in 
    let minrd = Algo.relate rel (roads world) in 
    let insct = nearest_pt_on_line source minrd in
    let ordp = (match insct with
    | Some (x,y) -> 
    if (distance source (x,y)) < 
        distance source (fst (Road.coords minrd)) 
    then 
      if (distance source (x,y) < distance source (snd (Road.coords minrd)))
      then (x,y)
      else (snd (Road.coords minrd))
    else 
      if (distance source (fst (Road.coords minrd)) 
              < distance source (snd (Road.coords minrd)))
      then (fst (Road.coords minrd))
      else (snd (Road.coords minrd))
    | None -> 
    if (distance source (fst (Road.coords minrd))) 
    < (distance source (fst (Road.coords minrd))) 
    then fst (Road.coords minrd) 
    else snd (Road.coords minrd))
    in
    let interp_dist = 
    distance ordp (fst (Road.coords minrd))/.
    distance (fst (Road.coords minrd)) (snd (Road.coords minrd))
    in
    (interp_dist, minrd)

let rep_ok world =
  raise (Failure "Unimplemented") (* TODO *)

let directions world start finish =
  raise (Failure "Unimplemented") (* TODO *)

let path_coords (p : path) =
  raise (Failure "Unimplemented") (* TODO *)
