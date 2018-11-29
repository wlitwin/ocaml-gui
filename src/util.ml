let ( % ) f g x = f (g x)

let dummy_ctx =
    let module CI = Cairo.Image in
    let img = CI.create CI.ARGB32 1 1 in
    Cairo.create img

let flip f x y = f y x

let id (a : 'a) : 'a = a

exception Break
let get_first_hashtbl tbl =
    let res = ref None in
    try
        Hashtbl.iter ~f:(fun v ->
            res := Some v;
            raise Break
        ) tbl;
        !res
    with _ -> !res

let is_some = function
    | Some _ -> true
    | None -> false

let is_none a = not (is_some a)

let some = function
    | Some v -> v
    | None -> failwith "None type"

let hash_of_list lst =
    let hash = Hashtbl.Poly.create () in
    List.iter ~f:(fun (k, v) -> Hashtbl.Poly.set hash k v) lst;
    hash
;;

let merge_hash h1 h2 =
    Hashtbl.Poly.iteri ~f:(fun ~key ~data ->
        if not (Hashtbl.mem h1 key) then
            Hashtbl.set h1 key data
    ) h2
;;

let find_opt tbl key =
    if Hashtbl.mem tbl key then
        Some (Hashtbl.find tbl key)
    else
        None

let lift_opt_1 (f : 'a -> 'b) (a : 'a option) : 'b option =
    match a with
    | Some a -> Some (f a)
    | None -> None

let app_opt f a b =
    match a, b with
    | None, None -> None
    | Some a, None -> Some a
    | None, Some b -> Some b
    | Some a, Some b -> if f a b then Some a else Some b
;;

let tee f v =
    f v;
    v
;;

let rec zip l1 l2 =
    match l1, l2 with
    | [], _ -> []
    | _, [] -> []
    | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
;;

let pair a b = (a, b)

let rec fold_until (f : 'a -> 'b -> ('a * bool)) (acc, ok) = function
    | [] -> (acc, ok)
    | hd :: tl ->
            let acc, ok = f acc hd in
            if not ok then
                (acc, ok)
            else
                fold_until f (acc, ok) tl
;;

let rec iter2 (f : 'a -> 'b -> unit) = function
    | [], _ -> ()
    | _, [] -> ()
    | h1 :: t1, h2 :: t2 -> f h1 h2; iter2 f (t1, t2)
;;

let rec replicate times item =
    if times = 0 then []
    else item :: replicate (times - 1) item
;;

let rec peek f = function
    | [] -> []
    | hd :: tl -> f hd; hd :: peek f tl
;;

let rest = function
    | [] -> []
    | _ :: tl -> tl
;;

let take n lst =
    let rec loop acc amt = function
        | tl when amt = n -> acc, tl
        | [] -> acc, []
        | hd :: tl -> loop (hd :: acc) (amt + 1) tl
    in
    let taken, rest = loop [] 0 lst in
    List.rev taken, rest
;;

let drop n lst = take n lst |> snd

let iceil = Int.of_float % Caml.ceil

let get_opt opt default =
    match opt with
    | Some value -> value
    | None -> default
;;

let timeit name f =
    let start = Unix.time() in
    let res = f() in
    let end_ = Unix.time() in
    Stdio.printf "%s time: %f\n%!" name (end_ -. start);
    res
;;

let lift_opt f default opt =
    match opt with
    | Some value -> f value
    | None -> default
;;

let rec until (pred : 'a -> bool) (f : 'a -> 'a) (arg : 'a) : 'a =
    if pred arg then arg else until pred f (f arg)
;;

let findIdx f lst =
    let rec findit idx = function
        | [] -> -1
        | hd :: _ when f hd -> idx
        | _ :: tl -> findit (idx + 1) tl
    in
    findit 0 lst
;;

let removeIdx idx lst =
    let before, after = take idx lst in
    before @ List.tl_exn after
;;

let clamp value minVal maxVal =
    let open Float in
    if value < minVal then minVal
    else if value > maxVal then maxVal
    else value
;;

let clampi value minVal maxVal =
    if value < minVal then minVal
    else if value > maxVal then maxVal
    else value
;;

let read_line () =
    Stdio.In_channel.input_line Stdio.stdin |> some
;;

let rec prompt_int str =
    try
        Stdio.printf "%s%!" str;
        Int.of_string (read_line())
    with _ ->
        Stdio.print_endline "Please enter a number";
        prompt_int str
;;

let rec prompt_range str min max =
    let v = prompt_int str in
    if v < min || v > max then begin
        Stdio.print_endline ("Number was not in range (" ^ Int.to_string min ^ ", " ^ Int.to_string max ^ ")");
        prompt_range str min max
    end else
        v
;;

let rec printTimes times str =
    if times > 0 then begin
        Stdio.printf "%s" str; printTimes (times - 1) str
    end
;;

let rec indent = function 
    | n when n <= 0 -> "" 
    | n -> " " ^ indent (n - 1)
;;

let printIndent amount = Stdio.printf "%s%!" (indent amount)

let prompt str =
    Stdio.printf "%s%!" str;
    read_line()
;;

let prompt_choices str choices =
    let len = List.length choices in
    Stdio.print_endline 
        (String.concat ~sep:"" (List.mapi ~f:(fun idx item -> Printf.sprintf "%d) %s\n" (idx + 1) item) choices));
    let idx = prompt_range str 1 len in
    List.nth choices (idx - 1)
;;

let prompt_choices2 str str_f choices =
    let len = List.length choices in
    Stdio.print_endline (String.concat ~sep:"" (List.mapi ~f:(fun idx item -> Printf.sprintf "%d) %s\n" (idx + 1) (str_f item)) choices));
    let idx = prompt_range str 1 len in
    List.nth choices (idx - 1)
;;

let rec prompt_until str f =
    Stdio.printf "%s%!" str;
    let ans = read_line() in
    if f ans then ans
    else prompt_until str f
;;

let replace idx item lst =
    let rec rep curIdx = function
        | [] -> []
        | hd :: tl when curIdx = idx -> item :: tl
        | hd :: tl -> hd :: rep (curIdx + 1) tl
    in
    rep 0 lst
;;

let insert idx item lst =
    let rec rep curIdx = function
        | [] -> [item]
        | hd :: tl when curIdx = idx -> item :: hd :: tl
        | hd :: tl -> hd :: rep (curIdx + 1) tl
    in
    rep 0 lst
;;

let rec splice lst = function
    | [] -> []
    | hd :: [] -> [hd]
    | hd1 :: hd2 :: [] -> [hd1] @ lst @ [hd2]
    | hd :: tl -> [hd] @ lst @ splice lst tl
;;

let swap item1 item2 lst =
    assert (List.exists ~f:(fun i -> phys_equal i item1) lst);
    assert (List.exists ~f:(fun i -> phys_equal i item2) lst);
    List.map ~f:(fun slot ->
        if phys_equal slot item1 then item2
        else if phys_equal slot item2 then item1
        else slot
    ) lst
;;

let swapIdx idx1 idx2 lst =
    let item1 = List.nth_exn lst idx1
    and item2 = List.nth_exn lst idx2 in
    swap item1 item2 lst
;;

let strRight str =
    let len = String.length str in
    if len = 0 then str
    else String.sub str 1 (len - 1)
;;

let strLeft str =
    let len = String.length str in
    if len = 0 then str
    else String.sub str 0 (len - 1)
;;

let hashtbl_values tbl =
    Hashtbl.fold ~f:(fun ~key:_ ~data acc ->
        data :: acc
    ) ~init:[] tbl
;;

let assoc_of_hashtbl tbl =
    Hashtbl.fold tbl [] (fun ~key ~data acc ->
        (key, data) :: acc
    ) 
;;

let startsWithCI str sub =
    String.is_prefix (String.lowercase str) (String.lowercase sub)

let strContains str sub =
    ExtLib.String.exists str sub

let strContainsCI str sub =
    strContains (String.lowercase str) (String.lowercase sub)

let strContainsAll str lst =
    List.for_all ~f:(strContains str) lst

let strContainsAllCI str lst =
    List.for_all ~f:(strContainsCI str) lst

