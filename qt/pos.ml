type t = {
    x : float;
    y : float;
}

let sub a b = {
    x = b.x -. a.x;
    y = b.y -. a.y;
}

let dot a b = a.x*.b.x +. a.y*.b.y

let len v = Float.sqrt (dot v v)

