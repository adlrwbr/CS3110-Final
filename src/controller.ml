open Button

exception NoButtonFound

(** [input prompt acc] is a user-entered string in response to a popup
    input field with the prompt message [prompt] where [acc] is the
    pending input before the user presses enter *)
let rec input (prompt : string) (acc : string) : string =
  (* clear graph *)
  Graphics.clear_graph ();
  (* draw popup *)
  View.draw_input_popup prompt acc;
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
  else input prompt (acc ^ String.make 1 key)

let button_touching_point coord b =
  let x, y = coord in
  let x_r, y_r, w_r, h_r = b.xywh in
  x >= x_r && x <= x_r +. w_r && y >= y_r && y <= y_r +. h_r

let invoke_action w b = b.action w

let hit_buttons w buttons coord =
  List.fold_left invoke_action w
    (match
       buttons
       |> List.filter button_enabled
       |> List.filter (button_touching_point coord)
     with
    | [] -> raise NoButtonFound
    | l -> l)

(** [nearest_road world] is a tuple ([pos, road]) that specifies a
    position [pos] on a [road] in [world] nearest the cursor *)
let nearest_road (world : World.wt) : float * Road.t =
  let point = Graphics.mouse_pos () |> View.pixel_to_world in
  World.nearroad point world

(** [nearest_loc world] is a location in [world] nearest the cursor *)
let nearest_loc (world : World.wt) : World.lt =
  (* cursor pos *)
  let cx, cy = Graphics.mouse_pos () |> View.pixel_to_world in
  let distance loc =
    let x, y = World.loc_coord loc in
    let dx, dy = (cx -. x, cy -. y) in
    sqrt ((dx *. dx) +. (dy *. dy))
  in
  let rec nearest_loc_rec smallest = function
    | [] -> smallest
    | h :: t ->
        if distance h < distance smallest then nearest_loc_rec h t
        else nearest_loc_rec smallest t
  in
  let locs = world |> World.locations in
  nearest_loc_rec (List.hd locs) locs

(** [loc_placement_mode world] is a world that may or may not have been
    modified by a location placed on the road nearest the cursor *)
let loc_placement_mode (world : World.wt) : World.wt =
  let _ = View.draw_location_instructions () in
  let loc_placement_mode_buttons =
    [
      {
        text = "Cancel";
        action = (fun w -> w);
        xywh = (180., 850., 100., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons loc_placement_mode_buttons;
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  if event.button then
    let mouse_coord = Graphics.mouse_pos () |> View.pixel_to_world in
    let new_world =
      match
        mouse_coord |> hit_buttons world loc_placement_mode_buttons
      with
      | exception _ -> (
          match nearest_road world with
          | exception _ ->
              world (* Make no changes if no roads are available *)
          | pos, r ->
              (* create loc at nearest road r at position pos *)
              let name = input "Enter new location name" "" in

              let category = input "Enter new location category" "" in
              let _, new_world =
                World.add_loc name category r pos world
              in
              new_world)
      | w -> w
    in
    new_world
  else world

(** [road_placement_mode world] is a world that may or may not have been
    modified during Road Placement Mode *)
let road_placement_mode (world : World.wt) : World.wt =
  let _ = View.draw_road_instructions () in
  let road_placement_mode_buttons =
    [
      {
        text = "Cancel";
        action = (fun w -> w);
        xywh = (20., 850., 100., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons road_placement_mode_buttons;
  let click1 = Graphics.wait_next_event [ Graphics.Button_down ] in
  if click1.button then
    let coord1 = Graphics.mouse_pos () |> View.pixel_to_world in
    let new_world =
      match coord1 |> hit_buttons world road_placement_mode_buttons with
      | exception _ ->
          let click2 =
            Graphics.wait_next_event [ Graphics.Button_down ]
          in
          if click2.button then
            (* get input*)
            let coord2 = Graphics.mouse_pos () |> View.pixel_to_world in
            let new_world =
              match
                coord2 |> hit_buttons world road_placement_mode_buttons
              with
              | exception _ ->
                  let name = input "Enter new road name" "" in
                  (* create road from coord 1 to coord 2 *)
                  let new_road = Road.create name coord1 coord2 in
                  let w = World.add_road new_road world in
                  w
              | w -> w
            in
            new_world
          else world
      | w -> w
    in
    new_world
  else world

(** [road_deletion_mode world] is a world that may or may not have been
    modified during Road Deletion Mode *)
let road_deletion_mode (world : World.wt) : World.wt =
  let _ = View.delete_road_instructions () in
  let road_deletion_mode_buttons =
    [
      {
        text = "Cancel";
        action = (fun w -> w);
        xywh = (340., 850., 100., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons road_deletion_mode_buttons;
  let click = Graphics.wait_next_event [ Graphics.Button_down ] in
  if click.button then
    let coord = Graphics.mouse_pos () |> View.pixel_to_world in
    let new_world =
      match coord |> hit_buttons world road_deletion_mode_buttons with
      | exception _ ->
          (* find roads located at coord *)
          let selected_roads = World.roads_at_coord coord world in
          let world =
            List.fold_left World.delete_road world selected_roads
          in
          world
      | w -> w
    in
    new_world
  else world

(** [location_deletion_mode world] is a world that may or may not have
    been modified during Location Deletion Mode *)
let loc_deletion_mode (world : World.wt) : World.wt =
  let _ = View.delete_loc_instructions () in
  let loc_deletion_mode_buttons =
    [
      {
        text = "Cancel";
        action = (fun w -> w);
        xywh = (500., 850., 100., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons loc_deletion_mode_buttons;
  let click = Graphics.wait_next_event [ Graphics.Button_down ] in
  if click.button then
    let coord = Graphics.mouse_pos () |> View.pixel_to_world in
    let new_world =
      match coord |> hit_buttons world loc_deletion_mode_buttons with
      | exception _ ->
          (* find location located at coord *)
          let selected_locs = World.locs_at_coord coord world in
          let world =
            List.fold_left World.delete_loc world selected_locs
          in
          world
      | w -> w
    in
    new_world
  else world

(** [edit_mode world] is a world edited by the user that may be reduced
    into a graph by [World.reduce] without raising an exception *)
let rec edit_mode (world : World.wt) : World.wt =
  (* clear graph *)
  Graphics.clear_graph ();
  (* draw edit mode GUI elements *)
  View.draw_world world;
  View.draw_edit_mode ();
  let edit_mode_buttons =
    [
      {
        text = "Add Road";
        action = (fun w -> w |> road_placement_mode |> edit_mode);
        xywh = (20., 900., 150., 40.);
        enabled = true;
      };
      {
        text = "Add Location";
        action = (fun w -> w |> loc_placement_mode |> edit_mode);
        xywh = (180., 900., 150., 40.);
        enabled = true;
      };
      {
        text = "Delete Road";
        action = (fun w -> w |> road_deletion_mode |> edit_mode);
        xywh = (340., 900., 150., 40.);
        enabled = true;
      };
      {
        text = "Delete Location";
        action = (fun w -> w |> loc_deletion_mode |> edit_mode);
        xywh = (500., 900., 150., 40.);
        enabled = true;
      };
      {
        text = "Done";
        action =
          (fun w ->
            if World.rep_ok w then (
              print_endline "Valid world!";
              w)
            else (
              print_endline "Invalid world! All roads must connect.";
              edit_mode w));
        xywh = (660., 900., 100., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons edit_mode_buttons;
  (* wait for input *)
  let event = Graphics.wait_next_event [ Graphics.Button_down ] in
  if event.button then
    let mouse_pos = Graphics.mouse_pos () |> View.pixel_to_world in
    match mouse_pos |> hit_buttons world edit_mode_buttons with
    | exception _ -> edit_mode world
    | new_world ->
        if World.rep_ok new_world then (
          print_endline "Valid world!";
          new_world)
        else (
          print_endline "Invalid world! All roads must connect.";
          edit_mode new_world)
  else edit_mode world

(** [direction_mode world] prompts the user to select two locations and
    highlights the shortest path between them. Requires: [world] can be
    reduced into graph form *)
let direction_mode (world : World.wt) : World.wt =
  print_endline "Click on two locations to get directions between them.";
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  let start = nearest_loc world in
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  let finish = nearest_loc world in
  let path = World.directions world start finish in
  let _ = View.draw_path path in
  print_endline "Click the screen to clear the directions.";
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  world

let buttons =
  [
    (* { text = "Random Road"; action = (fun w -> print_endline "HELLO";
       let road = Road.create "" (40. +. Random.float 900., 40. +.
       Random.float 900.) (40. +. Random.float 900., 40. +. Random.float
       900.) in w |> World.add_road road); xywh = (40., 800., 200.,
       40.); enabled = true; }; *)
    {
      text = "Quit";
      action =
        (fun w ->
          let _ = exit 0 in
          w);
      xywh = (20., 900., 100., 40.);
      enabled = true;
    };
    {
      text = "Edit";
      action = (fun w -> w |> edit_mode);
      xywh = (140., 900., 100., 40.);
      enabled = true;
    };
    {
      text = "Directions";
      action = (fun w -> w |> direction_mode);
      xywh = (260., 900., 150., 40.);
      enabled = true;
    };
  ]

(** [loop world] is the main event loop of the application that manages
    user input and displays [world] *)
let rec loop (world : World.wt) =
  (* clear graph *)
  Graphics.clear_graph ();
  (* display world *)
  View.draw_world world;
  View.draw_buttons buttons;
  (* wait for next keypress event *)
  let event =
    Graphics.wait_next_event
      [ Graphics.Key_pressed; Graphics.Button_down ]
  in
  (* check for input *)
  if event.button then
    let mouse_pos = Graphics.mouse_pos () |> View.pixel_to_world in
    match mouse_pos |> hit_buttons world buttons |> loop with
    | exception _ -> loop world
    | new_world -> new_world
  else loop world

let start () =
  let _ = View.init in
  (* let road = Road.create "Jane St" (250., 250.) (750., 750.) in *)
  let _, world =
    (0, World.empty "Hello World")
    (* |> World.add_road road |> (* add a Wendy's 70% down Jane St *)
       World.add_loc "Wendy's" "restaurant" road 0.7 *)
    (* in let _, world = World.add_loc "Denny's" "restaurant" road 0.3
       world in let _, world = World.add_loc "Friendly's" "restaurant"
       road 1. world *)
  in
  loop world
