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

(** [file_browser] is the full path to a user-selected file (or None if
    they exit) in response to an interactive GUI file browser popup *)
let file_browser () : string option =
  let rec folder_browser wd cursor_pos =
    (* get contents of CWD *)
    let current_dir = Unix.getcwd () |> Unix.opendir in
    let children : string list =
      let rec children acc =
        try Unix.readdir current_dir :: acc |> children
        with End_of_file -> acc
        (* sort alphabetically and remove '.' entry *)
      in
      children [] |> List.sort String.compare |> List.tl
    in
    Unix.closedir current_dir;
    (* clear graph *)
    Graphics.clear_graph ();
    (* draw popup *)
    View.draw_file_browser children cursor_pos;
    (* get next key *)
    let event = Graphics.wait_next_event [ Graphics.Key_pressed ] in
    let key = event.key in
    if key = 'j' then
      folder_browser wd
      @@ min (cursor_pos + 1) (List.length children - 1)
    else if key = 'k' then folder_browser wd @@ max (cursor_pos - 1) 0
    (* escape key *)
    else if key = '\x1B' then None
    (* return key *)
    else if key = '\r' then (
      (* if selection is file then select otherwise explore folder *)
      let selection = List.nth children cursor_pos in
      let stats = Unix.stat selection in
      match stats.st_kind with
      | S_REG -> Some selection
      | S_DIR ->
          Unix.chdir selection;
          folder_browser (Unix.getcwd ()) 0
      | _ ->
          print_endline "Cannot open!";
          folder_browser wd cursor_pos)
    else folder_browser wd cursor_pos
  in
  folder_browser "." 0

let button_touching_point coord b =
  let x, y = coord in
  let x_r, y_r, w_r, h_r = b.xywh in
  x >= x_r && x <= x_r +. w_r && y >= y_r && y <= y_r +. h_r

let invoke_action w b = b.action w

(** [hit_buttons w btns coord] is the world [w] that may have been
    modified as a result of the user clicking a button in [btns] at
    world-space coordinate [coord]. Raises: [NoButtonFound] if [coord]
    is not on an enabled button *)
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
  (* get first click coords *)
  Graphics.wait_next_event [ Graphics.Button_down ] |> ignore;
  let coord1 = Graphics.mouse_pos () |> View.pixel_to_world in
  try coord1 |> hit_buttons world road_placement_mode_buttons
  with NoButtonFound -> (
    (* get second click coords *)
    Graphics.wait_next_event [ Graphics.Button_down ] |> ignore;
    let coord2 = Graphics.mouse_pos () |> View.pixel_to_world in
    try coord2 |> hit_buttons world road_placement_mode_buttons
    with NoButtonFound ->
      (* prompt for road name *)
      let rec try_road_name prompt =
        let name = input prompt "" in
        let new_road = Road.create name coord1 coord2 in
        try World.add_road new_road world
        with World.RoadNameConflict rn ->
          "A road named " ^ rn ^ " already exists. Try again"
          |> try_road_name
      in
      try_road_name "Enter new road name")

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

(** [locs_at_coord coord world] is a list of locations located at
    [coord] *)
let locs_at_coord coord world =
  List.filter
    (fun l ->
      let x, y = View.world_to_pixel coord in
      let cx, cy = View.world_to_pixel (World.loc_coord l) in
      ((x - cx) * (x - cx)) + ((y - cy) * (y - cy)) <= 15 * 15)
    (World.locations world)

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
          let selected_locs = locs_at_coord coord world in
          let world =
            List.fold_left World.delete_loc world selected_locs
          in
          world
      | w -> w
    in
    new_world
  else world

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
        text = "Rename";
        action = (fun w ->
          w |> World.name |> input "Rename world" |> World.rename w
          |> edit_mode);
        xywh = (660., 900., 100., 40.);
        enabled = true;
      };
      {
        text = "Clear";
        action = (fun w ->
          let choice = input "Clear everything? (y/n) " "" |> String.lowercase_ascii in
          if choice = "yes" || choice = "y"
          then World.name w |> World.empty |> edit_mode
          else w |> edit_mode);
        xywh = (770., 900., 100., 40.);
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
        xywh = (880., 900., 100., 40.);
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
  let _ = View.get_directions_instructions () in
  let direction_mode_buttons =
    [
      {
        text = "Done";
        action = (fun w -> w);
        xywh = (500., 850., 150., 40.);
        enabled = true;
      };
    ]
  in
  View.draw_buttons direction_mode_buttons;
  let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
  let start = nearest_loc world in
  let coord1 = Graphics.mouse_pos () |> View.pixel_to_world in
  let _ =
    match coord1 |> hit_buttons world direction_mode_buttons with
    | exception _ ->
        let _ = Graphics.wait_next_event [ Graphics.Button_down ] in
        let finish = nearest_loc world in
        let coord2 = Graphics.mouse_pos () |> View.pixel_to_world in
        let _ =
          match coord2 |> hit_buttons world direction_mode_buttons with
          | exception _ ->
              let path = World.directions world start finish in
              let _ = View.draw_path path in
              let _ =
                Graphics.wait_next_event [ Graphics.Button_down ]
              in
              world
          | w -> w
        in
        world
    | w -> w
  in
  world

(** [load_mode w] is a user-specified world loaded from JSON *)
let rec load_mode (world : World.wt) : World.wt =
  (* it is important to cd to [initial_dir] if loading fails so that the save
     button saves an open world to the proper CWD *)
  let initial_dir = Unix.getcwd () in
  match file_browser () with
  | None ->
      Unix.chdir initial_dir;
      world
  | Some filename ->
    (* return to main menu if user enters nothing *)
    try filename |> Yojson.Basic.from_file |> World.from_json with
    | Yojson.Json_error _ ->
        ("Invalid file! Try again." |> print_endline;
         Unix.chdir initial_dir;
         load_mode world)
    | World.ParseError s ->
        ("Parse Error: " ^ s |> print_endline;
         Unix.chdir initial_dir;
         load_mode world)

(** [save_world_file w] saves the world [w] with name [n] to a JSON file
    called [n].json *)
let save_world_file (world : World.wt) : unit =
  let filename = World.name world ^ ".json" in
  world |> World.to_json |> Yojson.Basic.to_file filename;
  "World saved as " ^ filename |> print_endline

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
      text = "Load";
      action = (fun w -> w |> load_mode);
      xywh = (140., 900., 100., 40.);
      enabled = true;
    };
    {
      text = "Save";
      action =
        (fun w ->
          save_world_file w;
          w);
      xywh = (260., 900., 100., 40.);
      enabled = true;
    };
    {
      text = "Edit";
      action = (fun w -> w |> edit_mode);
      xywh = (380., 900., 100., 40.);
      enabled = true;
    };
  ]

(** [loop world] is the main event loop of the application that manages
    user input and displays [world] *)
let rec loop (world : World.wt) =
  (* clear graph *)
  Graphics.clear_graph ();
  (* draw world *)
  Graphics.clear_graph ();
  View.draw_world world;
  let buttons =
    if List.length (World.locations world) >= 2 then
      {
        text = "Directions";
        action = (fun w -> w |> direction_mode);
        xywh = (500., 900., 150., 40.);
        enabled = true;
      }
      :: buttons
    else buttons
  in
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
