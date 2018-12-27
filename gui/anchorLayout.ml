type item = Mixins.layoutable

type loc = (* Relative to the window/layout rect top/bot/etc *)
         | WTop of float
         | WBottom of float
         | WLeft of float
         | WRight of float
         (* Relative to an item *)
         | ITop of item * float
         | IBottom of item * float
         | ILeft of item * float
         | IRight of item * float
         (* Preferred *)
         | PreferredW
         | PreferredH

module Location = struct
    type t = {
        top : loc;
        left : loc;
        right : loc;
        bottom : loc;
    }
end

type constraints = {
    item : item;
    loc : Location.t;
}

type dep_field = DLeft
               | DRight
               | DTop
               | DBottom

type field_dep = {
    source : dep_field;
    depends_on : dep_field;
    from : item;
}

let get_dep = function
    | ITop (item, _) -> Some (item, DTop)
    | IBottom (item, _) -> Some (item, DBottom)
    | ILeft (item, _) -> Some (item, DLeft)
    | IRight (item, _) -> Some (item, DRight)
    | _ -> None
;;

module HP = Hashtbl.Poly

type field_tuple = item * loc * dep_field

type dep_graph = {
    constraints : (item, constraints) HP.t;
    top_sort : (item * dep_field) list;
}

(*
 L ← Empty list that will contain the sorted nodes
 while there are unmarked nodes do
   select an unmarked node n
   visit(n)

 function visit(node n)
    if n has a permanent mark then return
    if n has a temporary mark then stop   (not a DAG)
    mark n temporarily
    for each node m with an edge from n to m do
        visit(m)
    mark n permanently
    add n to head of L
*)

type item_dep = item * dep_field

let depth_sort (tbl : (item_dep, item_dep list) HP.t) (all : field_tuple list) : item_dep list =
    let sorted = ref [] in
    let temp : (item_dep, unit) HP.t = HP.create () in
    let perm : (item_dep, unit) HP.t = HP.create () in
    let has_temp (i : item_dep) = HP.find temp i |> Option.is_some in
    let has_perm (i : item_dep) = HP.find perm i |> Option.is_some in
    let mark_temp i = HP.set temp i () in
    let mark_perm i = HP.set perm i () in
    let rec visit (item, dep as key : item_dep) =
        if has_perm key then ()
        else if has_temp key then (failwith "Not a DAG")
        else begin
            mark_temp key;
            let lst = HP.find_exn tbl key in
            List.iter lst visit;
            mark_perm key;
            sorted := key :: !sorted;
        end
    in
    List.iter all (fun (i, _, d) -> visit (i, d));
    List.rev !sorted
;;

(* Need to build dependency graph *)
let calc_dep_graph lst =
    (* Split all constraints into item+field *)
    let c_table = HP.create() in
    List.iter lst (fun c -> 
        HP.set c_table c.item c;
    );
    let lst = List.fold lst ~init:[] ~f:(fun accum c ->
        (c.item, c.loc.top, DTop)
        :: (c.item, c.loc.bottom, DBottom)
        :: (c.item, c.loc.left, DLeft)
        :: (c.item, c.loc.right, DRight)
        :: accum)
    |> List.map ~f:(function
        (* Hacky, dummy to force the field update order *)
        | item, PreferredW, DRight -> item, ILeft (item, 1.), DRight
        | item, PreferredH, DBottom -> item, ITop  (item, 1.), DBottom
        | item, PreferredW, _
        | item, PreferredH, _ -> failwith "PreferredW/H dep on not right/bottom"
        | tup -> tup
    )
    in
    Stdio.printf "LIST SIZE %d\n" (List.length lst);
    let tbl = HP.create() in
    List.iter lst (fun (item, loc, depField) ->
        let key = (item, depField) in
        match get_dep loc with
        | None -> HP.set tbl key []
        | Some depInfo ->
            match HP.find tbl key with
            | Some lst -> HP.set tbl key (depInfo :: lst)
            | None -> HP.set tbl key [depInfo]
    );
    {
        constraints = c_table;
        top_sort = depth_sort tbl lst;
    }
;;

let calc_loc (screen : Rect.t) loc field_tbl item =
    let lookup t = HP.find_exn field_tbl t in
    match loc with
    | WTop amt -> screen.y +. amt
    | WLeft amt -> screen.x +. amt
    | WRight amt -> screen.x +. screen.w +. amt
    | WBottom amt -> screen.y +. screen.h +. amt
    | ITop (item, amt) -> lookup (item, DTop) +. amt
    | ILeft (item, amt) -> lookup (item, DLeft) +. amt
    | IRight (item, amt) -> lookup (item, DRight) +. amt
    | IBottom (item, amt) -> lookup (item, DBottom) +. amt
    (* Hacky - dynamically recalculate the size *)
    | PreferredW -> lookup (item, DLeft) +. item#preferredSize.Size.w
    | PreferredH -> lookup (item, DTop) +. item#preferredSize.Size.h
;;

let layout_single_item screen deps field_tbl (item, field as key) =
    let c = HP.find_exn deps item in
    let loc = match field with
            | DTop -> c.loc.top
            | DBottom -> c.loc.bottom
            | DLeft -> c.loc.left
            | DRight -> c.loc.right
    in
    let res = calc_loc screen loc field_tbl item in
    HP.set field_tbl key res;
;;

let set_rectangle tbl item =
    let lookup field = HP.find_exn tbl (item, field) in
    let rect = Aabb.{
        x1 = lookup DLeft;
        y1 = lookup DTop;
        x2 = lookup DRight;
        y2 = lookup DBottom;
    } |> RectAabb.rect_of_aabb in
    Stdio.printf "RECTANGLE %f %f %f %f\n" rect.x rect.y rect.w rect.h;
    item#resize rect
;;

let layout_deps rect deps =
    Stdio.printf "Deps! %d\n" (List.length deps.top_sort);
    let field_tbl = HP.create() in
    List.iter deps.top_sort (layout_single_item rect deps.constraints field_tbl);
    HP.iter_keys deps.constraints (set_rectangle field_tbl)
;;

type rules = constraints list

class anchorLayout rules =
    let deps = calc_dep_graph rules in
    let items = List.map rules (fun r -> r.item) in
object(self)
    inherit Mixins.layout as super

    val id = 0
    val mutable eventHandlers = []
    
    method items = items
    method preferredSize = Size.{w=400.; h=400.}
    method addLayoutable _ = ()
    method removeLayoutable _ = ()

    method layout rect =
        layout_deps rect deps
end
