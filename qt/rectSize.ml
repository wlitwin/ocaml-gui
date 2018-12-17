let size_of_rect (r : Rect.t) : Size.t = {
    w = r.w;
    h = r.h;
}

let rect_of_size (s : Size.t) : Rect.t = {
    x = 0.;
    y = 0.;
    w=s.w;
    h=s.h;
}

