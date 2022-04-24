type t = {
  name : string;
  startPt : float * float;
  endPt : float * float;
}

type it = {
  road1 : t;
  road2 : t;
  pos_on_road1 : float;
  pos_on_road2 : float;
}

let create name startPoint endPoint =
  { name; startPt = startPoint; endPt = endPoint }

let name road = road.name
let road_coords road = (road.startPt, road.endPt)

let midpt road =
  let fp = road |> road_coords |> fst in
  let lp = road |> road_coords |> snd in
  match (fp, lp) with
  | (a, b), (c, d) -> ((a +. c) /. 2., (b +. d) /. 2.)

let inter_coords road1 road2 : (float * float) option =
  match (road_coords road1, road_coords road2) with
  | ( ((start1_x, start1_y), (end1_x, end1_y)),
      ((start2_x, start2_y), (end2_x, end2_y)) ) ->
      (* calculate the slopes of lines 1 and 2 *)
      let m1 = Algo.slope start1_x start1_y end1_x end1_y in
      let m2 = Algo.slope start2_x start2_y end2_x end2_y in
      (* x and y is the intersection of the two lines extended from line 
         segments 1 and 2 *)
      (* for now, we are saying that it is impossible for two roads with the
         same slope to intersect *)
      if m1 = m2 then None
      else
      let x =
        (start2_y -. start1_y +. (m1 *. start1_x) -. (m2 *. start2_x))
        /. (m1 -. m2)
      in
      let y = (m2 *. (x -. start2_x)) +. start2_y in
      (* Check if the intersection point is actually on the segments. *)
      if
        Algo.in_range x start1_x end1_x
        && Algo.in_range x start2_x end2_x
        && Algo.in_range y start1_y end1_y
        && Algo.in_range y start2_y end2_y
      then Some (x, y)
      else None

let intersection (r1 : t) (r2 : t) : it option =
  match inter_coords r1 r2 with
  | None -> None
  | Some intersect -> 
      let r1_dist = Algo.distance r1.startPt r1.endPt in
      let pos_on_road1 = Algo.distance intersect r1.startPt /. r1_dist in
      let r2_dist = Algo.distance r2.startPt r2.endPt in
      let pos_on_road2 = Algo.distance intersect r2.startPt /. r2_dist in
        Some
        { road1 = r1; road2 = r2; pos_on_road1 = pos_on_road1; pos_on_road2 = pos_on_road2 }
