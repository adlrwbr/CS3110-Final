type rt = {
  startPt : float * float;
  endPt : float * float;
  name : string;
}

type lt = {
  id : int;
  name : string;
  category : string;
  road : rt;
  pos_on_road : float;
}

type wt = {
  name : string;
  g : Graph.vgt;
  roads : rt list;
  locations : lt list;
}

let empty name = {
  name = name;
  g = Graph.empty;
  roads = [];
  locations = [];
}

let add name category road pos world =
  match Graph.add world.g with 
  | nid, ng ->
      {
        name = world.name;
        g = ng;
        roads = world.roads;
        locations = {id = nid; name = name; category = category; road = road; pos_on_road = pos} 
        :: world.locations 
  }

let locations world = world.locations
let name (loc : lt) = loc.name
let category loc = loc.category
let loc_coord loc =
  match (loc.road.startPt, loc.road.endPt) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads
let road_coords road = [road.startPt; road.endPt]
