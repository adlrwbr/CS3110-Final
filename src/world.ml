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

let size_x = 1000.
let size_y = 1000.
let empty name = { name; g = Graph.empty; roads = []; locations = [] }

(** [midpt road] finds the midpoint of the [road] passed in SOLELY by
    looking at the start and end nodes. *)
let midchord road =
  let fp = road |> Road.coords |> List.hd in
  let lp = road |> Road.coords |> Algo.tl in
  match (fp, lp) with
  | (a, b), (c, d) -> ((a +. c) /. 2., (b +. d) /. 2.)

let distance pt1 pt2 =
  match (pt1, pt2) with
  | (a, b), (c, d) ->
      sqrt (((a -. c) *. (a -. c)) +. ((b -. d) *. (b -. d)))

let add_loc name category road pos world =
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
  let road_coords = Road.coords loc.road in
  let road_start = List.nth road_coords 0 in
  let road_end = List.nth road_coords 1 in
  (* calculate location's coordinates w/ pos_on_road *)
  match (road_start, road_end) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads
let slope x1 y1 x2 y2 = (y2 -. y1) /. (x2 -. x1)
let in_range p p1 p2 = (p >= p1 && p <= p2) || (p >= p2 && p <= p1)

let intersection road1 road2 =
  match (Road.coords road1, Road.coords road2) with
  | ( [ (p_a1_x, p_a1_y); (p_a2_x, p_a2_y) ],
      [ (p_b1_x, p_b1_y); (p_b2_x, p_b2_y) ] ) ->
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
  | _, _ -> raise (Invalid_argument "")
