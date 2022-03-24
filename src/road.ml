type t = {
  name : string;
  startPt : float * float;
  endPt : float * float;
}

type intersection = {
  id : string;
  road1 : t;
  road2 : t;
  pos_on_road1 : float;
  pos_on_road2 : float;
}

let create name startPoint endPoint =
  { name; startPt = startPoint; endPt = endPoint }

let coords road = road.startPt, road.endPt
let name road = road.name
