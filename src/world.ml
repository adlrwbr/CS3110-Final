type lt = {
  id : int;
  name : string;
  category : string;
  road : Road.t;
  pos_on_road : float;
}

type wt = {
  name : string;
  g : Graph.vgt; (* represents the world in simplified graph form *)
  roads : Road.t list;
  locations : lt list;
}

let empty name = {
  name = name;
  g = Graph.empty;
  roads = [];
  locations = [];
}

let add_loc name category road pos world =
  raise (Failure "Not fully implemented")
  (* match Graph.add world.g with *)
  (* | id, ng -> *)
  (*     { *)
  (*       name = world.name; *)
  (*       g = ng; *)
  (*       roads = world.roads; *)
  (*       locations = world.locations *)
  (*     } *)

let add_road road world =
  {
    name = world.name;
    g = world.g;
    roads = road :: world.roads;
    locations = world.locations
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
