type wt = { g : Graph.vgt }

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

let add world name category road pos = raise (Failure "Unimplemented")
let name world loc = loc.name
let category world loc = loc.category
let locations world = raise (Failure "Unimplemented")

let loc_coord loc =
  ( fst loc.road.startPt
    +. (loc.pos_on_road *. (fst loc.road.endPt -. fst loc.road.startPt)),
    snd loc.road.startPt
    +. (loc.pos_on_road *. (snd loc.road.endPt -. snd loc.road.startPt))
  )

let roads world = raise (Failure "Unimplemented")
let road_coords road = raise (Failure "Unimplemented")
