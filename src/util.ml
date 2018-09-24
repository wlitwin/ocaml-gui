let ( % ) f g x = f (g x)

let flip f x y = f y x

let getChar () =
    flush stdout;
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
        { termio with Unix.c_icanon = false; Unix.c_echo = false; }
    in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res
;;

let id (a : 'a) : 'a = a

exception Break
let get_first_hashtbl tbl =
    let res = ref None in
    try
        Hashtbl.iter (fun _ v ->
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
    let hash = Hashtbl.create 10 in
    List.iter (fun (k, v) -> Hashtbl.replace hash k v) lst;
    hash
;;

let merge_hash h1 h2 =
    Hashtbl.iter (fun k v ->
        if not (Hashtbl.mem h1 k) then
            Hashtbl.replace h1 k v
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

let iceil = int_of_float % ceil

let find f lst =
    try
        Some (List.find f lst)
    with Not_found -> None
;;

let get_opt opt default =
    match opt with
    | Some value -> value
    | None -> default
;;

let timeit name f =
    let start = Sys.time() in
    let res = f() in
    let end_ = Sys.time() in
    Printf.printf "%s time: %f\n" name (end_ -. start);
    flush stdout;
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
    before @ List.tl after
;;

let clamp value minVal maxVal =
    if value < minVal then minVal
    else if value > maxVal then maxVal
    else value
;;

let rec prompt_int str =
    try
        print_string str;
        int_of_string (read_line())
    with _ ->
        print_endline "Please enter a number";
        prompt_int str
;;

let rec prompt_range str min max =
    let v = prompt_int str in
    if v < min || v > max then begin
        print_endline ("Number was not in range (" ^ string_of_int min ^ ", " ^ string_of_int max ^ ")");
        prompt_range str min max
    end else
        v
;;

let rec printTimes times str =
    if times > 0 then begin
        print_string str; printTimes (times - 1) str
    end
;;

let rec indent = function 
    | n when n <= 0 -> "" 
    | n -> " " ^ indent (n - 1)
;;

let printIndent amount = print_string (indent amount)

let prompt str =
    print_string str;
    read_line()
;;

let prompt_choices str choices =
    let len = List.length choices in
    print_endline (String.concat "" (List.mapi (fun idx item -> Printf.sprintf "%d) %s\n" (idx + 1) item) choices));
    let idx = prompt_range str 1 len in
    List.nth choices (idx - 1)
;;

let prompt_choices2 str str_f choices =
    let len = List.length choices in
    print_endline (String.concat "" (List.mapi (fun idx item -> Printf.sprintf "%d) %s\n" (idx + 1) (str_f item)) choices));
    let idx = prompt_range str 1 len in
    List.nth choices (idx - 1)
;;

let rec prompt_until str f =
    print_string str;
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
    assert (List.exists (fun i -> i == item1) lst);
    assert (List.exists (fun i -> i == item2) lst);
    List.map (fun slot ->
        if slot == item1 then item2
        else if slot == item2 then item1
        else slot
    ) lst
;;

let swapIdx idx1 idx2 lst =
    let item1 = List.nth lst idx1
    and item2 = List.nth lst idx2 in
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
    Hashtbl.fold (fun _ v acc ->
        v :: acc
    ) tbl []
;;

let assoc_of_hashtbl tbl =
    Hashtbl.fold (fun k v acc ->
        (k, v) :: acc
    ) tbl []
;;

let rec startsWith str sub =
    if String.length str = 0 ||
       String.length sub = 0
    then true
    else if String.get str 0 = String.get sub 0 then
        startsWith (strRight str) (strRight sub)
    else
        false
;;

let startsWithCI str sub =
    startsWith (String.lowercase_ascii str) (String.lowercase_ascii sub)

let strContains str sub =
    ExtLib.String.exists str sub

let strContainsCI str sub =
    strContains (String.lowercase_ascii str) (String.lowercase_ascii sub)

let strContainsAll str lst =
    List.for_all (strContains str) lst

let strContainsAllCI str lst =
    List.for_all (strContainsCI str) lst

module Queue = struct

let mk vals =
    ref vals
;;

let fst (a, _) = a
let snd (_, b) = b

let pop queue =
    match !queue with
    | [] -> None
    | hd :: tl -> queue := tl; Some hd
;;

let push queue value =
    queue := !queue @ [value]
;;

let rec next visited queue =
    match pop queue with
    | None -> None
    | Some s ->
        match List.mem s visited with
        | exception Not_found -> s
        | _ -> next visited queue
;;

end
