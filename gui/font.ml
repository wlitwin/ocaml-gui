type weight = Normal
            | Bold

type metrics = {
    width : float;
    x_bearing : float;
    x_advance : float;
    ascent : float;
    descent : float;
}

type t = {
    mutable size : float;
    mutable font : string;
    mutable weight : weight;
}

let default_font = {
    size = 24.0;
    font = "Ubuntu Mono";
    weight = Normal;
}
