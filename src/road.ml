type t = {
  name : string;
  startPt : float * float;
  endPt : float * float;
}

let create name startPoint endPoint =
  { name; startPt = startPoint; endPt = endPoint }

let coords road = [ road.startPt; road.endPt ]
let name road = road.name
