type t = {
    w : float;
    h : float;
}

let zero = {
    w=0.; h=0.;
}

let add_size s1 s2 = {
    w = s1.w +. s2.w;
    h = s1.h +. s2.h;
}

let max_size  s1 s2 = {
    w = Float.max s1.w s2.w;
    h = Float.max s1.h s2.h;
}

let min_size  s1 s2 = {
    w = Float.min s1.w s2.w;
    h = Float.min s1.h s2.h;
}

let str_of_size s : string =
    Printf.sprintf "{w=%f h=%f}" s.w s.h

