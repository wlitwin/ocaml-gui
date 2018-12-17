type rect = {
    x : float;
    y : float;
    w : float;
    h : float;
}

let empty_rect = {
    x = 0.; y = 0.;
    w = 0.; h = 0.;
}

type aabb = {
    x1 : float;
    y1 : float;
    x2 : float;
    y2 : float;
}

type color = {
    r : float;
    g : float;
    b : float;
    a : float;
}

let black = {r=0.; g=0.; b=0.; a=1.}
let red   = {r=1.; g=0.; b=0.; a=1.}
let blue  = {r=0.; g=0.; b=1.; a=1.}
let green = {r=0.; g=1.; b=0.; a=1.}
let white = {r=1.; g=1.; b=1.; a=1.}
let orange = {r=1.; g=0.5; b=0.; a=1.}
let yellow = {r=1.; g=1.; b=0.; a=1.}
let none  = {r=0.; g=0.; b=0.; a=0.}

let aabb_of_rect (r : rect) : aabb = {
    x1 = r.x;
    y1 = r.y;
    x2 = r.x +. r.w;
    y2 = r.y +. r.h;
}

let rect_of_aabb (a : aabb) : rect = {
    x = a.x1;
    y = a.y1;
    w = a.x2 -. a.x1;
    h = a.y2 -. a.y1;
}

let aabb_intersect a1 a2 = {
    x1 = Float.max a1.x1 a2.x1;
    y1 = Float.max a1.y1 a2.y1;
    x2 = Float.min a1.x2 a2.x2;
    y2 = Float.min a1.y2 a2.y2;
}

let aabb_union a1 a2 = {
    x1 = Float.min a1.x1 a2.x1;
    y1 = Float.min a1.y1 a2.y1;
    x2 = Float.max a1.x2 a2.x2;
    y2 = Float.max a1.y2 a2.y2;
}

let union r1 r2 =
    aabb_union (aabb_of_rect r1)
               (aabb_of_rect r2)
    |> rect_of_aabb
;;

let intersection r1 r2 =
    aabb_intersect (aabb_of_rect r1)
                   (aabb_of_rect r2)
    |> rect_of_aabb
;;

(* Is r1 inside r2? *)
let aabb_inside a b =
    let open Float in
    a.x1 >= b.x1 && a.x2 <= b.x2
    && a.y1 >= b.y1 && a.y2 <= b.y2

let inside r1 r2 =
    aabb_inside (aabb_of_rect r1)
                (aabb_of_rect r2)

type pos = {
    x : float;
    y : float;
}

type size = {
    w : float;
    h : float;
}

let add_size (s1 : size) (s2 : size) : size = {
    w = s1.w +. s2.w;
    h = s1.h +. s2.h;
}

let max_size (s1 : size) (s2 : size) : size = {
    w = Float.max s1.w s2.w;
    h = Float.max s1.h s2.h;
}

let min_size (s1 : size) (s2 : size) : size = {
    w = Float.min s1.w s2.w;
    h = Float.min s1.h s2.h;
}

let sub a b = {
    x = b.x -. a.x;
    y = b.y -. a.y;
}

let dot a b = a.x*.b.x +. a.y*.b.y

let len v = Float.sqrt (dot v v)

let zero_size : size = {
    w=0.; h=0.;
}

let center (r : rect) : pos = {
    x=r.x +. r.w*.0.5;
    y=r.y +. r.h*.0.5; 
}

let pos_of_rect (r : rect) : pos = {
    x = r.x;
    y = r.y;
}

let size_of_rect (r : rect) : size = {
    w = r.w;
    h = r.h;
}

let rect_of_size (s : size) : rect = {
    x = 0.;
    y = 0.;
    w=s.w;
    h=s.h;
}

let str_of_size (s : size) : string =
    Printf.sprintf "{w=%f h=%f}" s.w s.h

let str_of_rect (r : rect) : string =
    Printf.sprintf "{x=%f y=%f w=%f h=%f}" r.x r.y r.w r.h
