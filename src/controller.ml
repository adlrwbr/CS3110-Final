(** [loop world] is the main event loop of the application that manages user
    input and displays [world] *)
let rec loop (world : World.wt) =
    (* clear graph *)
    let _ = Graphics.clear_graph () in
    (* display world *)
    let _ = View.draw world in
    (* wait for next keypress event *)
    let event = Graphics.wait_next_event [Graphics.Key_pressed] in
    (* check for quit key else loop again *)
    if event.key == 'q' then exit 0
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
