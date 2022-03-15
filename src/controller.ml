(** [loop] is the main event loop of the application *)
let rec loop () =
    (* wait for next keypress event *)
    let event = Graphics.wait_next_event [Graphics.Key_pressed] in
    (* check for quit key else loop again *)
    if event.key == 'q' then exit 0
    else loop ()

let start () =
    let _ = View.init in
    let _ = World.empty "Hello World" in
    loop ()
