(** Representation of the button used in the GUI.

    This module represents a button with its function, text, position
    and dimensions. *)

type button = {
  text : string;
  action : World.wt -> World.wt;
  xywh : float * float * float * float;
  enabled : bool;
}
(** The representation of the button type. *)

val button_enabled : button -> bool
(** [button enabled b] is whether or not [b] is enabled *)
