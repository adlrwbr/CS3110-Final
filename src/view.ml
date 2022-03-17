open Graphics

let init =
  (* initialize default window *)
  let _ = open_graph "" in
  let _ = set_window_title "Pathfinder TODO: change name" in
  ()

let world_to_pixel (coord : float * float) : int * int =
  let f_x, f_y = coord in
  let i_x =
    f_x /. World.size_x *. (() |> Graphics.size_x |> Int.to_float)
    |> Int.of_float
  in
  let i_y =
    f_y /. World.size_y *. (() |> Graphics.size_y |> Int.to_float)
    |> Int.of_float
  in
  (i_x, i_y)

let pixel_to_world (coord : int * int) : float * float =
  let i_x, i_y = coord in
  let f_x =
    Int.to_float i_x
    /. Int.to_float (Graphics.size_x ())
    *. World.size_x
  in
  let f_y =
    Int.to_float i_y
    /. Int.to_float (Graphics.size_y ())
    *. World.size_y
  in
  (f_x, f_y)

(** [draw_loc loc] draws the location [loc] *)
let draw_loc (loc : World.lt) =
  (* node coords in pixel space *)
  let x, y = World.loc_coord loc |> world_to_pixel in
  (* draw node *)
  let _ = fill_circle x y 15 in
  (* draw name label *)
  let _ = moveto x y in
  let _ = rmoveto (-15) 20 in
  let _ = draw_string (World.name loc) in
  (* draw category label *)
  let _ = moveto x y in
  let _ = rmoveto (-20) (-30) in
  let _ = draw_string (World.category loc) in
  ()

(** [draw_road loc] draws the location [loc] *)
let draw_road (road : Road.t) =
  let rec draw_segment prev lst =
    let x1, y1 = prev in
    match lst with
    | [] -> ()
    | coord2 :: t ->
        let x2, y2 = world_to_pixel coord2 in
        let _ = moveto x1 y1 in
        let _ = lineto x2 y2 in
        draw_segment (x2, y2) t
  in
  let coords = Road.coords road in
  let hx, hy = List.hd coords in
  let x, y = World.midchord road |> world_to_pixel in
  let t = List.tl coords in
  let _ = draw_segment (world_to_pixel (hx, hy)) t in
  (* draw name label *)
  let _ = moveto x y in
  let _ = draw_string (Road.name road) in
  ()

let draw world =
  let _ = List.map draw_loc (World.locations world) in
  let _ = List.map draw_road (World.roads world) in
  ()
