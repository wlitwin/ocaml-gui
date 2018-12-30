module Graphics = Platform.Windowing.Graphics

let degrees = Float.pi /. 180.0

let rounded_rect cr (rect : Rect.t) radius = 
    (*
    let open Graphics in
    let open Float in
    let r = rect in
    Path.sub cr;
    arc cr (r.x + r.w - radius) (r.y + radius) radius (~-90. *. degrees) 0.;
    arc cr (r.x + r.w - radius) (r.y + r.h - radius) radius 0. (90. *. degrees);
    arc cr (r.x + radius) (r.y + r.h -radius) radius (90. *. degrees) (180. *. degrees);
    arc cr (r.x + radius) (r.y + radius) radius (180. *. degrees) (270. *. degrees);
    Path.close cr;
    *)
    ()
;;
