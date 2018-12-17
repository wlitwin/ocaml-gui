let center (r : Rect.t) : Pos.t = {
    x=r.x +. r.w*.0.5;
    y=r.y +. r.h*.0.5; 
}

let pos_of_rect (r : Rect.t) : Pos.t = {
    x = r.x;
    y = r.y;
}

