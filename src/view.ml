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
  let _ = set_text_size 100 in
  let _ = draw_string (World.name loc) in
  (* draw category label *)
  let _ = moveto x y in
  let _ = rmoveto (-20) (-30) in
  let _ = draw_string (World.category loc) in
  ()

(** [draw_road loc] draws the location [loc] *)
let draw_road (road : Road.t) =
  (* draw road line *)
  let coord1, coord2 = Road.coords road in
  let x1, y1 = world_to_pixel coord1 in
  let x2, y2 = world_to_pixel coord2 in
  moveto x1 y1;
  lineto x2 y2;
  (* draw name label *)
  let x, y = World.midpt road |> world_to_pixel in
  moveto x y;
  draw_string (Road.name road)

let draw world =
  let _ = List.map draw_loc (World.locations world) in
  let _ = List.map draw_road (World.roads world) in
  ()

let draw_input_popup prompt input =
  let win_width, win_height =
    (Int.to_float (size_x ()), Int.to_float (size_y ())) in
  (* draw background box *)
  let _ = rgb 170 227 255 |> set_color in
  let box_width = win_width *. 0.66 |> Int.of_float in
  let box_height = win_height *. 0.2 |> Int.of_float in
  let box_ll_x = size_x () / 2 - box_width / 2 in
  let box_ll_y = size_y () / 2 - box_height / 2 in
  let _ = fill_rect box_ll_x box_ll_y box_width box_height in
  (* draw prompt text *)
  let _ = rgb 0 0 0 |> set_color in
  let _ = moveto
    (box_ll_x + 10) (box_ll_y + box_height - 15) in
  let _ = draw_string (prompt ^ " (press Enter to submit):") in
  (* draw input text *)
  let _ = moveto (box_ll_x + 5) (size_y () / 2) in
  let _ = rmoveto 30 (-30) in
  let _ = draw_string input in
  ()
