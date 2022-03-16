open Graphics

let init =
    (* initialize default window *)
    let _ = open_graph "" in
    let _ = set_window_title "Pathfinder TODO: change name" in
    ()

(** [world_to_pixel (x, y)] is an integer coordinate pair in pixel space from
    the float coordinates in World *)
let world_to_pixel ( coord : float * float ) : int * int =
    let f_x, f_y = coord in
    let i_x =
        f_x /. World.size_x *. (() |> Graphics.size_x |> Int.to_float)
        |> Int.of_float
    in let i_y = f_y /. World.size_y *. (() |> Graphics.size_y |> Int.to_float)
        |> Int.of_float
    in (i_x, i_y)

(** [draw_loc loc] draws the location [loc] *)
let rec draw_loc ( loc : World.lt ) =
    let x, y = World.loc_coord loc |> world_to_pixel in
    fill_circle x y 15

let draw world =
    let _ = List.map draw_loc (World.locations world) in
    ()
