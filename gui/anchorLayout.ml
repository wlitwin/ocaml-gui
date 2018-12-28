type item = Mixins.layoutable

type dep_field = DLeft
               | DRight
               | DTop
               | DBottom

type item_dep = item * dep_field

module HP = Hashtbl.Poly

type field_tbl = (item_dep, float) HP.t

type loc = (* Relative to the window/layout rect top/bot/etc *)
         | WTop
         | WBottom
         | WLeft
         | WRight
         (* Relative to an item *)
         | ITop of item
         | IBottom of item
         | ILeft of item
         | IRight of item
         (* Preferred *)
         | PreferredW of item
         | PreferredH of item
         (* Combinations *)
         | Add of loc list
         | Sub of loc list
         | Mul of loc list
         | Div of loc list
         | Max of loc list
         | Min of loc list
         | Fun of (field_tbl -> float)
         | FunDep of item_dep list * (field_tbl -> float)
         | Const of float

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

let rec get_deps = function
    | ITop item -> [item, DTop]
    | IBottom item -> [item, DBottom]
    | ILeft item -> [item, DLeft]
    | IRight item -> [item, DRight]
    | Add lst
    | Sub lst
    | Mul lst
    | Div lst
    | Max lst
    | Min lst -> List.map lst get_deps |> List.concat
    | FunDep (lst, _) -> lst
    | PreferredW item -> [(item, DLeft)]
    | PreferredH item -> [(item, DTop)]
    | WTop
    | WLeft
    | WRight
    | WBottom
    | Fun _
    | Const _ -> []
;;

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
        :: accum
    ) in
    let tbl = HP.create() in
    List.iter lst (fun (item, loc, depField) ->
        let key = (item, depField) in
        let dep_lst = get_deps loc in
        match HP.find tbl key with
        | Some lst -> HP.set tbl key (dep_lst @ lst)
        | None -> HP.set tbl key dep_lst 
    );
    {
        constraints = c_table;
        top_sort = depth_sort tbl lst;
    }
;;

let sel_field tbl (item, field) =
    let c = HP.find_exn tbl item in
    match field with
    | DTop -> c.loc.top
    | DBottom -> c.loc.bottom
    | DLeft -> c.loc.left
    | DRight -> c.loc.right
;;

let rec calc_loc (screen : Rect.t) field_tbl deps loc =
    let lookup t = HP.find_exn field_tbl t in
    let recur = calc_loc screen field_tbl deps in
    let app ~init ~f lst = 
        lst
        |> List.map ~f:recur
        |> List.fold ~init ~f
    in
    match loc with
    | WTop -> screen.y
    | WLeft -> screen.x
    | WRight -> screen.x +. screen.w
    | WBottom -> screen.y +. screen.h
    | ITop item -> lookup (item, DTop)
    | ILeft item -> lookup (item, DLeft)
    | IRight item -> lookup (item, DRight)
    | IBottom item -> lookup (item, DBottom)
    (* Hacky - dynamically recalculate the size *)
    | PreferredW item -> lookup (item, DLeft) +. item#preferredSize.Size.w
    | PreferredH item -> lookup (item, DTop) +. item#preferredSize.Size.h
    | Max lst -> app ~init:Float.min_value ~f:Float.max lst
    | Min lst -> app ~init:Float.max_value ~f:Float.min lst
    | Add lst -> app ~init:0. ~f:Float.add lst
    | Mul lst -> app ~init:1. ~f:Float.( * ) lst
    | Sub lst -> app ~init:(recur (List.hd_exn lst)) ~f:Float.sub (List.drop lst 1)
    | Div lst -> app ~init:(recur (List.hd_exn lst)) ~f:Float.(/) (List.drop lst 1)
    | Fun f -> f field_tbl
    | FunDep (_, f) -> f field_tbl
    | Const c -> c
;;

let layout_single_item screen deps field_tbl (item, field as key) =
    let loc = sel_field deps key in
    let res = calc_loc screen field_tbl deps loc in
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
    item#resize rect
;;

let layout_deps rect deps =
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
