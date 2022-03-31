type button = {
  text : string;
  action : World.wt -> World.wt;
  xywh : float * float * float * float;
  enabled : bool;
}

let button_enabled b = b.enabled
