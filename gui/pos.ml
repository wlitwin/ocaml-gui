type t = {
    x : float;
    y : float;
}

let zero = {
    x = 0.;
    y = 0.;
}

let add a b = {
    x = a.x +. b.x;
    y = a.y +. b.y;
}

let sub a b = {
    x = b.x -. a.x;
    y = b.y -. a.y;
}

let dot a b = a.x*.b.x +. a.y*.b.y

let len v = Float.sqrt (dot v v)

let equal (a, b) =
    Float.(a.x = b.x && a.y = b.y)
