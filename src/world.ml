type rt = {
  coords : (float * float) list;
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
  g : Graph.vgt;
  roads : rt list;
  locations : lt list;
}

let add world name category road pos =
  match Graph.add world.g with
  | id, ng ->
      { g = ng; roads = world.roads; locations = world.locations }

let name world loc = loc.name
let category world loc = loc.category
let locations world = world.locations

let loc_coord loc =
  match (loc.road.startPt, loc.road.endPt) with
  | (x1, y1), (x2, y2) ->
      ( x1 +. (loc.pos_on_road *. (x2 -. x1)),
        y1 +. (loc.pos_on_road *. (y2 -. y1)) )

let roads world = world.roads
let road_coords road = road.coords
