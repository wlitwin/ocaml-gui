type state = Uncreated of int
           | Created

type 'a t = {
    mutable size : int;
    mutable storage : 'a array;
    mutable state : state;
}

let create ?(capacity=10) () = {
    size = 0;
    storage = [||];
    state = Uncreated (max capacity 1);
}

let clear t = t.size <- 0

let empty t = t.size = 0

let get t idx = 
    assert Poly.(t.state = Created);
    assert (idx >= 0 && idx < t.size);
    t.storage.(idx)

let set t idx v = 
    assert Poly.(t.state = Created);
    assert (idx >= 0 && idx < t.size);
    t.storage.(idx) <- v

let length t = t.size

let iter (f : 'a -> unit) t : unit =
    for i=0 to t.size-1 do
        f t.storage.(i)
    done
;;

let exists f t =
    let rec loop idx =
        if idx >= t.size then false
        else (
            if f t.storage.(idx) then true
            else loop (idx+1)
        )
    in
    loop 0
;;

let delete t idx =
    assert Poly.(t.state = Created);
    assert (idx >= 0 && idx < t.size);
    assert (t.size > 0);
    for i=idx to t.size-2 do
        t.storage.(i) <- t.storage.(i+1) 
    done;
    t.size <- t.size - 1
;;

let filter f t =
    let write = ref 0 in
    for idx=0 to t.size-1 do
        match f t.storage.(idx) with
        | true -> (* keep *)
            t.storage.(!write) <- t.storage.(idx);
            write := !write + 1
        | false -> (* discard *) ()
    done;
    t.size <- !write
;;

let fold_left f init t =
    let acc = ref init in
    for i=0 to t.size-1 do
        acc := f !acc t.storage.(i)
    done;
    !acc
;;

let add t item =
    begin match t.state with
    | Uncreated cap -> 
        t.storage <- Array.create ~len:cap item;
        t.state <- Created;
    | Created -> 
        let len = Array.length t.storage in
        if t.size = len then (
            let newer = Array.create ~len:(len*2) item  in
            Array.blit ~src:t.storage ~src_pos:0 ~dst:newer ~dst_pos:0 ~len:t.size;
            t.storage <- newer;
        );
        t.storage.(t.size) <- item;
    end;
    t.size <- t.size + 1;
;;

let capacity t = Array.length t.storage

let map f t =
    let dest = create ~capacity:(capacity t) () in
    for i=0 to t.size-1 do
        add dest (f t.storage.(i))
    done;
    dest
;;

let of_list lst =
    let arr = create() in
    let rec loop = function
        | [] -> ()
        | hd :: tl ->
            add arr hd;
            loop tl;
    in
    loop lst;
    arr
;;

let iteri f t =
    for i=0 to t.size-1 do
        f i t.storage.(i)
    done;
;;
