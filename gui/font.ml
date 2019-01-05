type weight = Normal
            | Bold

type metrics = {
    width : float;
    x_bearing : float;
    x_advance : float;
    ascent : float;
    descent : float;
}

let empty_metrics = {
    width = 0.;
    x_bearing = 0.;
    x_advance = 0.;
    ascent = 0.;
    descent = 0.;
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
