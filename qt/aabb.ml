type t = {
    x1 : float;
    y1 : float;
    x2 : float;
    y2 : float;
}

let intersect a1 a2 = {
    x1 = Float.max a1.x1 a2.x1;
    y1 = Float.max a1.y1 a2.y1;
    x2 = Float.min a1.x2 a2.x2;
    y2 = Float.min a1.y2 a2.y2;
}

let union a1 a2 = {
    x1 = Float.min a1.x1 a2.x1;
    y1 = Float.min a1.y1 a2.y1;
    x2 = Float.max a1.x2 a2.x2;
    y2 = Float.max a1.y2 a2.y2;
}

let inside a b =
    let open Float in
    a.x1 >= b.x1 && a.x2 <= b.x2
    && a.y1 >= b.y1 && a.y2 <= b.y2

