let clamp value minVal maxVal =
    let open Float in
    if value < minVal then minVal
    else if value > maxVal then maxVal
    else value
;;

let timeit name f =
    let start = Unix.gettimeofday() in
    let res = f() in
    let end_ = Unix.gettimeofday() in
    (*Caml.print_endline Printf.(sprintf "%s time: %fms\n" name ((end_ -. start) *. 1000.));*)
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

