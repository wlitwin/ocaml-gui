type t = {
    w : float;
    h : float;
}

let zero = {
    w=0.; h=0.;
}

let inset sz amt = {
    w = sz.w -. amt;
    h = sz.h -. amt;
}

let outset sz amt = {
    w = sz.w +. amt;
    h = sz.h +. amt;
}

let add s1 s2 = {
    w = s1.w +. s2.w;
    h = s1.h +. s2.h;
}

let max s1 s2 = {
    w = Float.max s1.w s2.w;
    h = Float.max s1.h s2.h;
}

let min s1 s2 = {
    w = Float.min s1.w s2.w;
    h = Float.min s1.h s2.h;
}

let to_string s : string =
    Printf.sprintf "{w=%f h=%f}" s.w s.h

