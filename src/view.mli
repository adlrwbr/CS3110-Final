(** represents the view in MVC pattern *)

type button

val buttons : button list

val init : unit
(** [init] instantiates a blank map *)

val draw : World.wt -> bool -> unit
(** [draw world display_controls] draws [world] onto the GUI and
    displays the control instructions based on [display_controls] *)

val draw_input_popup : string -> string -> unit
(** [draw_input_popup prompt input] draws an input textfield with prompt
    text [prompt] and pending input text [input] *)

val draw_edit_mode : unit -> unit
(** [draw_edit_mode] draws the GUI overlay for edit mode *)

val world_to_pixel : float * float -> int * int
(** [world_to_pixel (x, y)] is an integer coordinate pair in pixel space
    from the float coordinates in World *)

val pixel_to_world : int * int -> float * float
(** [pixel_to_world (x, y)] is a float coordinate pair in world space
    from the integer pixel coordinates in View *)

val button_touching_point : float * float -> button -> bool
(** [button_touching_point coord b] is whether or not the point [coord]
    is touching the button [b] *)

val button_enabled : button -> bool
(** [button enabled b] is whether or not [b] is enabled *)

val hit_buttons : World.wt -> float * float -> World.wt
(** [hit_buttons coord] attempts to hit all enabled buttons that are at
    point [coord] *)

val display_button : button -> unit
(** [display_button b] displays [button] as a rectangle *)

val display_buttons : unit -> unit
(** [display_buttons () ] displays all enabled buttons on the screen *)
