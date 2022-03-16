(** [road_placement_mode world] is a world that may or may not have been
    modified during Road Placement Mode *)
let road_placement_mode ( world : World.wt ) : World.wt =
    let coord1 = Graphics.mouse_pos () |> View.pixel_to_world in
    let road_event = Graphics.wait_next_event [Graphics.Key_pressed] in
    if road_event.key == 'r' then
        (* place road *)
        let coord2 = Graphics.mouse_pos () |> View.pixel_to_world in
        (* create road from coord 1 to coord 2 *)
        let new_road = Road.create "New St" coord1 coord2 in
        let world = World.add_road new_road world in
        world
    else world

(** [loop world] is the main event loop of the application that manages user
    input and displays [world] *)
let rec loop ( world : World.wt ) =
    (* clear graph *)
    let _ = Graphics.clear_graph () in
    (* display world *)
    let _ = View.draw world in
    (* wait for next keypress event *)
    let event = Graphics.wait_next_event [Graphics.Key_pressed] in
    (* check for quit key else loop again *)
    if event.key == 'q' then exit 0
    (* enter road placement mode *)
    else if event.key == 'r' then
        loop (road_placement_mode world)
    else loop world

let start () =
    let _ = View.init in
    let road = Road.create "Jane St" (250., 250.) (750., 750.) in
    let _, world =
        World.empty "Hello World" |>
        World.add_road road |>
        (* add a Wendy's 70% down Jane St *)
        World.add_loc "Wendy's" "restaurant" road 0.7
    in let _, world =
        World.add_loc "Denny's" "restaurant" road 0.3 world
    in let _, world =
        World.add_loc "Friendly's" "restaurant" road 1. world
    in loop world
