type color = {
    r : float;
    g : float;
    b : float;
    a : float;
}

let gray = {r=242./.255.; g=241./.255.; b=240./.255.; a=1.}
let black = {r=0.; g=0.; b=0.; a=1.}
let red   = {r=1.; g=0.; b=0.; a=1.}
let blue  = {r=0.; g=0.; b=1.; a=1.}
let green = {r=0.; g=1.; b=0.; a=1.}
let white = {r=1.; g=1.; b=1.; a=1.}
let orange = {r=1.; g=0.5; b=0.; a=1.}
let yellow = {r=1.; g=1.; b=0.; a=1.}
let none  = {r=0.; g=0.; b=0.; a=0.}

let set cr color =
    Cairo.set_source_rgba cr color.r color.g color.b color.a
;;
