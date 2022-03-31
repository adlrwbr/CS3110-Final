type button = {
  text : string;
  action : World.wt -> World.wt;
  xywh : float * float * float * float;
  enabled : bool;
}

val button_enabled : button -> bool
(** [button enabled b] is whether or not [b] is enabled *)
