open Graphics
open Button

let init =
  (* initialize default window *)
  let _ = open_graph "" in
  let _ = set_window_title "Pathfinder TODO:\n    change name" in
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

type x_anchor =
  | LEFT
  | CENTER
  | RIGHT

type y_anchor =
  | TOP
  | MIDDLE
  | BOTTOM

type anchor = y_anchor * x_anchor
(** [anchor] is a variant that specifies how an element is anchored
    relative to its coordinates *)

(** [draw_text x y anchor size text] draws the string [text] with its
    [anchor] at pixel-space coordinate ([x], [y]) in the font size
    [size] *)
let draw_text
    (x : int)
    (y : int)
    (anchor : anchor)
    ?size:(font_size : int = 12)
    (text : string) : unit =
  (* set_font "-misc-dejavu sans
     mono-bold-r-normal--12-0-0-0-m-0-iso8859-1"; *)
  (* "-misc-dejavu sans mono-bold-r-normal--" ^ Int.to_string font_size
     ^ "-0-0-0-m-0-iso8859-1" |> set_font; *)
  moveto x y;
  rgb 0 0 0 |> set_color;
  let width, height = text_size text in
  let dy =
    match anchor with
    | TOP, _ -> -1 * height
    | MIDDLE, _ -> -1 * height / 2
    | BOTTOM, _ -> 0
  in
  let dx =
    match anchor with
    | _, LEFT -> 0
    | _, CENTER -> -1 * width / 2
    | _, RIGHT -> -1 * width
  in
  rmoveto dx dy;
  draw_string text

(** [draw_loc loc] draws the location [loc] *)
let draw_loc (loc : World.lt) =
  rgb 0 0 0 |> set_color;
  (* node coords in pixel space *)
  let x, y =
    match World.loc_coord loc |> world_to_pixel with
    | exception _ ->
        (0, 0) (* set default point to (0, 0) if exception raised *)
    | x, y -> (x, y)
  in
  (* draw node *)
  fill_circle x y 15;
  (* draw name label *)
  loc |> World.name |> draw_text x (y + 15) (BOTTOM, CENTER);
  (* draw category label *)
  loc |> World.category |> draw_text x (y - 15) (TOP, CENTER)

(** [draw_road loc] draws the location [loc] *)
let draw_road (road : Road.t) =
  rgb 128 128 128 |> set_color;
  set_line_width 3;
  (* draw road line *)
  let coord1, coord2 = Road.road_coords road in
  let x1, y1 = world_to_pixel coord1 in
  let x2, y2 = world_to_pixel coord2 in
  moveto x1 y1;
  lineto x2 y2;
  (* draw name label *)
  let x, y = Road.midpt road |> world_to_pixel in
  road |> Road.name |> draw_text x y (BOTTOM, CENTER)

let draw_button b =
  let x, y, w, h = b.xywh in
  let (x, y), (w, h), (text_x, text_y) =
    ( world_to_pixel (x, y),
      world_to_pixel (w, h),
      world_to_pixel (x +. (w /. 2.), y +. (h /. 2.)) )
  in
  rgb 170 227 255 |> set_color;
  fill_rect x y w h;
  rgb 0 0 0 |> set_color;
  draw_text text_x text_y (MIDDLE, CENTER) b.text;
  ()

let draw_buttons buttons =
  buttons |> List.filter button_enabled |> List.iter draw_button

let draw_location_instructions () =
  draw_text
    (size_x () / 2)
    (size_y ()) (TOP, CENTER)
    "Click a point close to a road to place a location.";
  ()

let draw_road_instructions () =
  draw_text
    (size_x () / 2)
    (size_y ()) (TOP, CENTER)
    "Click two points on the screen to place a road";
  ()

let delete_road_instructions () =
  draw_text
    (size_x () / 2)
    (size_y ()) (TOP, CENTER) "Click the road you want to delete";
  ()

let delete_loc_instructions () =
  draw_text
    (size_x () / 2)
    (size_y ()) (TOP, CENTER) "Click the location you want to delete";
  ()

let draw_world world =
  let _ = List.map draw_road (World.roads world) in
  let _ = List.map draw_loc (World.locations world) in
  ()

let draw_input_popup prompt input =
  let win_width, win_height =
    (Int.to_float (size_x ()), Int.to_float (size_y ()))
  in
  (* draw background box *)
  let _ = rgb 170 227 255 |> set_color in
  let box_width = win_width *. 0.66 |> Int.of_float in
  let box_height = win_height *. 0.2 |> Int.of_float in
  let box_ll_x = (size_x () / 2) - (box_width / 2) in
  let box_ll_y = (size_y () / 2) - (box_height / 2) in
  fill_rect box_ll_x box_ll_y box_width box_height;
  (* draw prompt text *)
  rgb 0 0 0 |> set_color;
  prompt ^ " (press Enter to submit):"
  |> draw_text (box_ll_x + 10)
       (box_ll_y + box_height - 15)
       (BOTTOM, LEFT);
  (* draw input text *)
  input
  |> draw_text (box_ll_x + 35) ((size_y () / 2) - 30) (BOTTOM, LEFT)

let draw_edit_mode () =
  (* draw welcome text *)
  let title = "Welcome to edit mode." in
  draw_text 5 (size_y ()) (TOP, LEFT) ~size:15 title;
  ()
(* let _, title_height = text_size title in *)
(* draw keybinding guide *)
(* let keybind_height = "P" |> text_size |> snd in draw_text 5 (size_y
   () - title_height - (1 * (keybind_height + 5))) (TOP, LEFT) "Press r
   to place road endpoints"; draw_text 5 (size_y () - title_height - (2
   * (keybind_height + 5))) (TOP, LEFT) "Press l to place a location"
   (draw_text 5 (size_y () - title_height - (3 * (keybind_height + 5)))
   (TOP, LEFT) "Press e to exit" *)

let draw_path (path : World.path) =
  rgb 61 190 255 |> set_color;
  set_line_width 9;
  path |> World.path_coords
  |> List.map (fun c -> world_to_pixel c)
  |> Array.of_list |> draw_poly_line
