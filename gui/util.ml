let dummy_ctx =
    let module CI = Cairo.Image in
    let img = CI.create CI.ARGB32 1 1 in
    Cairo.create img

let clamp value minVal maxVal =
    let open Float in
    if value < minVal then minVal
    else if value > maxVal then maxVal
    else value
;;

let timeit name f =
    let start = Unix.time() in
    let res = f() in
    let end_ = Unix.time() in
    Stdio.printf "%s time: %f\n%!" name (end_ -. start);
    res
;;

let iceil = Fn.compose Int.of_float Caml.ceil

let strLeft str =
    let len = String.length str in
    if len = 0 then str
    else String.sub str 0 (len - 1)
;;

let strRight str =
    let len = String.length str in
    if len = 0 then str
    else String.sub str 1 (len - 1)
;;

