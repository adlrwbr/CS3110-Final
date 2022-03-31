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

let intersection (r1 : t) (r2 : t) : it option =
  (* TODO *)
  let _ =
    Some
      { road1 = r1; road2 = r2; pos_on_road1 = 0.; pos_on_road2 = 0. }
  in
  raise (Failure "Unimplemented")

let inter_coords inter =
  match (road_coords inter.road1, road_coords inter.road2) with
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
      then (x, y)
      else
        raise
          (Failure "Intersection provided to [inter_coords] is invalid!")
