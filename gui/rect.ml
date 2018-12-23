include RectType

let empty = {
    x = 0.; y = 0.;
    w = 0.; h = 0.;
}

let inset r amt =
    {x = r.x +. amt;
     y = r.y +. amt;
     w = r.w -. amt;
     h = r.h -. amt; }
;;

let outset r amt =
    {x = r.x -. amt;
     y = r.y -. amt;
     w = r.w +. amt;
     h = r.h +. amt; }
;;

let round r =
    { x = Float.round r.x;
      y = Float.round r.y;
      w = Float.round r.w;
      h = Float.round r.h; }
;;

let union r1 r2 = 
    let aabb1 = RectAabb.aabb_of_rect r1
    and aabb2 = RectAabb.aabb_of_rect r2 in
    Aabb.union aabb1 aabb2
    |> RectAabb.rect_of_aabb;
;;

let intersection r1 r2 =
    let aabb1 = RectAabb.aabb_of_rect r1
    and aabb2 = RectAabb.aabb_of_rect r2 in
    Aabb.intersect aabb1 aabb2
    |> RectAabb.rect_of_aabb
;;

(* Is r1 inside r2? *)
let inside r1 r2 =
    Aabb.inside (RectAabb.aabb_of_rect r1)
                (RectAabb.aabb_of_rect r2)
;;

let to_string r : string =
    Printf.sprintf "{x=%f y=%f w=%f h=%f}" r.x r.y r.w r.h
;;
