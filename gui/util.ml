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
    Caml.print_endline Printf.(sprintf "%s time: %fms\n" name ((end_ -. start) *. 1000.));
    res
;;

let time f =
    let start = Unix.gettimeofday() in
    let res = f() in
    let end_ = Unix.gettimeofday() in
    res, (end_ -. start)*.1000.0
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

let array_concat arr =
    let sum = Array.fold ~init:0 ~f:(fun s a -> s + Array.length a) arr in
    if sum = 0 then [||]
    else begin
        let init =
            let rec loop i =
                if i >= sum then failwith "Impossible"
                else begin
                    if Array.length(arr.(i)) > 0 then
                        arr.(i).(0)
                    else loop (i+1)
                end
            in
            loop 0
        in
        let out = Array.create ~len:sum init in
        let idx = ref 0 in
        for i = 0 to sum-1 do
            let src = arr.(i) in
            let len = Array.length src in
            Array.blit ~src ~src_pos:0 ~dst:out ~dst_pos:(!idx) ~len;
            idx := len + !idx
        done;
        out
    end
;;

let dynarray_sort (arr, cmp) =
    let get = DynArray.get arr in
    let set (idx, v) = DynArray.set arr idx v in
    let swap(i, j) =
        let tmp = get i in
        set (i, (get j));
        set (j, tmp);
    in
    let incr r = r := !r + 1 in
    let decr r = r := !r - 1 in
    let partition (lo, hi) =
        let pivot = get ((lo + hi)/2) in
        let i = ref (lo - 1)
        and j = ref (hi + 1) in
        let rec loop() =
            incr i;
            while cmp(get !i, pivot) < 0 do
                incr i;
            done;
            decr j;
            while cmp(get !j, pivot) > 0 do
                decr j;
            done;
            if !i >= !j then (
                !j
            ) else (
                swap(!i, !j);
                loop();
            )
        in
        loop()
    in
    let rec quicksort (lo, hi) =
        if lo < hi then (
            let p = partition(lo, hi) in
            quicksort(lo, p);
            quicksort(p+1, hi);
        )
    in
    if DynArray.length arr > 1 then
        quicksort(0, DynArray.length arr - 1)
;;

