(** [input prompt acc] is a user-entered string in response to a popup input
    field with the prompt message [prompt] where [acc] is the pending input
    before the user presses enter *)
let rec input ( prompt : string ) ( acc : string ) : string =
  (* clear graph *)
  let _ = Graphics.clear_graph () in
  (* draw popup *)
  let _ = View.draw_input_popup prompt acc in
  (* get next key *)
  let event = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  let key = event.key in
  (* if user presses enter return accumulated input *)
  if key == '\r' then acc
  (* backspace last character and ask for input again *)
  else if key == '\b' then
    (* limit backspace to empty string *)
    if acc = "" then input prompt acc
    else input prompt (String.sub acc 0 (String.length acc - 1))
  (* append to acc and ask for input again *)
  else input prompt (acc ^ (String.make 1 key))


(** [nearest_road] is a tuple ([pos, road]) that specifies a position
    [pos] on a [road] nearest the cursor *)
let nearest_road (world : World.wt) : float * Road.t =
  let point = Graphics.mouse_pos () |> View.pixel_to_world in
  (*let allroads = world |> World.roads in
    ( 0.5,
    Algo.relate
      (fun a b ->
        World.distance (World.midpt a) point
        <= World.distance (World.midpt b) point)
      allroads ) DEPRECATED NAIVE APPROACH <---*)
  World.nearroad point world

(** [place_loc world] is a world that may or may not have been
    modified by a location placed on the road nearest the cursor *)
let place_loc (world : World.wt) : World.wt =
  let pos, r = nearest_road world in
  (* create loc at nearest road r at position pos *)
  let name = input "Enter new location name" "" in
  let category = input "Enter new location category" "" in
  let _, new_world = World.add_loc name category r pos world in
  new_world

(** [road_placement_mode world] is a world that may or may not have been
    modified during Road Placement Mode *)
let road_placement_mode (world : World.wt) : World.wt =
  let coord1 = Graphics.mouse_pos () |> View.pixel_to_world in
  let road_event = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  if road_event.key == 'r' then
    (* get input*)
    let coord2 = Graphics.mouse_pos () |> View.pixel_to_world in
    let name = input "Enter new road name" "" in
    (* create road from coord 1 to coord 2 *)
    let new_road = Road.create name coord1 coord2 in
    let world = World.add_road new_road world in
    world
  else world

(** [edit_mode world] is a world edited by the user that may be reduced into
    a graph by [World.reduce] without raising an exception *)
let rec edit_mode (world : World.wt) : World.wt =
  (* clear graph *)
  Graphics.clear_graph ();
  (* draw edit mode GUI elements *)
  View.draw world;
  View.draw_edit_mode ();
  (* wait for input *)
  let event = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  if event.key == 'e' then
    (* check if user is allowed to quit edit mode *)
    match World.reduce world with
    | exception World.IllegalWorld s -> print_endline s; edit_mode world
    | _ -> world
  else if event.key == 'r' then (road_placement_mode world)
  else if event.key == 'l' then (place_loc world)
  else edit_mode world

(** [loop world] is the main event loop of the application that manages
    user input and displays [world] *)
let rec loop (world : World.wt) =
  (* clear graph *)
  let _ = Graphics.clear_graph () in
  (* display world *)
  let _ = View.draw world in
  (* wait for next keypress event *)
  let event = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  (* check for input *)
  if event.key == 'q' then exit 0
  else if event.key == 'e' then world |> edit_mode |> loop
  else loop world

let start () =
  let _ = View.init in
  let road = Road.create "Jane St" (250., 250.) (750., 750.) in
  let _, world =
    World.empty "Hello World"
    |> World.add_road road
    |> (* add a Wendy's 70% down Jane St *)
    World.add_loc "Wendy's" "restaurant" road 0.7
  in
  let _, world = World.add_loc "Denny's" "restaurant" road 0.3 world in
  let _, world =
    World.add_loc "Friendly's" "restaurant" road 1. world
  in
  loop world
