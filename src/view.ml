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

type x_anchor = LEFT | CENTER | RIGHT
type y_anchor = TOP | MIDDLE | BOTTOM

(** [anchor] is a variant that specifies how an element is anchored relative
    to its coordinates *)
type anchor = y_anchor * x_anchor

(** [draw_text x y anchor text] draws the string [text] with its [anchor] at
    pixel-space coordinate ([x], [y]) *)
let draw_text (x : int) (y : int) (anchor : anchor) (text : string) : unit =
  set_text_size 100; (* TODO: why isn't this working? *)
  (* set_font "-misc-dejavu sans mono-bold-r-normal--256-0-0-0-m-0-iso8859-1"; *)
  moveto x y;
  let width, height = text_size text in
  let dy =
    match anchor with
    | TOP, _ -> -1 * height
    | MIDDLE, _ -> -1 * height / 2
    | BOTTOM, _ -> 0
  in let dx =
    match anchor with
    | _, LEFT -> 0
    | _, CENTER -> -1 * width / 2
    | _, RIGHT -> -1 * width
  in
  rmoveto dx dy;
  draw_string text

(** [draw_loc loc] draws the location [loc] *)
let draw_loc (loc : World.lt) =
  (* node coords in pixel space *)
  let x, y = World.loc_coord loc |> world_to_pixel in
  (* draw node *)
  fill_circle x y 15;
  (* draw name label *)
  loc |> World.name |> draw_text (x - 15) (y + 20) (BOTTOM, LEFT);
  (* draw category label *)
  loc |> World.category |> draw_text (x - 20) (y - 30) (BOTTOM, LEFT)

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
  road |> Road.name |> draw_text x y (BOTTOM, LEFT)

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
  fill_rect box_ll_x box_ll_y box_width box_height;
  (* draw prompt text *)
  rgb 0 0 0 |> set_color;
  prompt ^ " (press Enter to submit):"
  |> draw_text (box_ll_x + 10) (box_ll_y + box_height - 15) (BOTTOM, LEFT);
  (* draw input text *)
  input |> draw_text (box_ll_x + 35) (size_y () / 2 - 30) (BOTTOM, LEFT)

let draw_edit_mode () =
  draw_text 0 0 (BOTTOM, LEFT) "Welcome to edit mode"
