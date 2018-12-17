let aabb_of_rect (r : RectType.t) : Aabb.t = {
    x1 = r.x;
    y1 = r.y;
    x2 = r.x +. r.w;
    y2 = r.y +. r.h;
}

let rect_of_aabb (a : Aabb.t) : RectType.t = {
    x = a.x1;
    y = a.y1;
    w = a.x2 -. a.x1;
    h = a.y2 -. a.y1;
}

