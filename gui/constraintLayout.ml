type item = Mixins.layoutable

type dep_field = DLeft
               | DRight
               | DTop
               | DBottom

type item_dep = item * dep_field

module PolyHash = Hashtbl.Poly

type field_tbl = (item_dep, float) PolyHash.t

module Constraint = struct
    type t = (* Relative to the window/layout rect top/bot/etc *)
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
           | Add of t list
           | Sub of t list
           | Mul of t list
           | Div of t list
           | Max of t list
           | Min of t list
           | FunDep of item_dep list * (field_tbl -> float)
           | Const of float

    let rec gather_dependencies = function
        | ITop item -> [item, DTop]
        | IBottom item -> [item, DBottom]
        | ILeft item -> [item, DLeft]
        | IRight item -> [item, DRight]
        | Add lst
        | Sub lst
        | Mul lst
        | Div lst
        | Max lst
        | Min lst -> List.map lst gather_dependencies |> List.concat
        | FunDep (lst, _) -> lst
        | PreferredW item -> [(item, DLeft)]
        | PreferredH item -> [(item, DTop)]
        | WTop
        | WLeft
        | WRight
        | WBottom
        | Const _ -> []
;;

    (* Lots of common helper methods *) 
    let relative t amt = Add [t; Const amt]

    let wTop = relative WTop
    let wLeft = relative WLeft
    let wRight = relative WRight
    let wBottom = relative WBottom

    let topOf item = relative (ITop item)
    let leftOf item = relative (ILeft item)
    let rightOf item = relative (IRight item)
    let bottomOf item = relative (IBottom item)
end

module Bounds = struct
    type t = {
        top : Constraint.t;
        left : Constraint.t;
        right : Constraint.t;
        bottom : Constraint.t;
    }
end

type item_constraints = {
    item : item;
    loc : Bounds.t;
}

module DependencyGraph = struct
    type t = {
        constraints : (item, item_constraints) PolyHash.t;
        top_sort : (item * dep_field) list;
    }

    type field_tuple = item * Constraint.t * dep_field
    type visited_set = (item_dep, unit) PolyHash.t

    (* Topological sort, based on depth-first search *)
    let depth_sort (tbl : (item_dep, item_dep list) PolyHash.t) (all : field_tuple list) : item_dep list =
        let sorted = ref [] in
        let temp = PolyHash.create () in
        let perm = PolyHash.create () in
        let has_temp i = PolyHash.find temp i |> Option.is_some in
        let has_perm i = PolyHash.find perm i |> Option.is_some in
        let mark_temp i = PolyHash.set temp i () in
        let mark_perm i = PolyHash.set perm i () in
        let rec visit (item, dep as key : item_dep) =
            if has_perm key then ()
            else if has_temp key then (failwith "Not a DAG")
            else begin
                mark_temp key;
                let lst = PolyHash.find_exn tbl key in
                List.iter lst visit;
                mark_perm key;
                sorted := key :: !sorted;
            end
        in
        List.iter all (fun (i, _, d) -> visit (i, d));
        List.rev !sorted
    ;;

    let calculate_dependency_graph (lst : item_constraints list) : t =
        let constraint_table = PolyHash.create() in
        List.iter lst (fun c -> 
            PolyHash.set constraint_table c.item c;
        );
        let lst = List.fold lst ~init:[] ~f:(fun accum c ->
            (c.item, c.loc.top, DTop)
            :: (c.item, c.loc.bottom, DBottom)
            :: (c.item, c.loc.left, DLeft)
            :: (c.item, c.loc.right, DRight)
            :: accum
        ) in
        let tbl = PolyHash.create() in
        List.iter lst (fun (item, loc, depField) ->
            let key = (item, depField) in
            let dep_lst = Constraint.gather_dependencies loc in
            match PolyHash.find tbl key with
            | Some lst -> PolyHash.set tbl key (dep_lst @ lst)
            | None -> PolyHash.set tbl key dep_lst 
        );
        {
            constraints = constraint_table;
            top_sort = depth_sort tbl lst;
        }
    ;;
end

let sel_field tbl (item, field) =
    let c = PolyHash.find_exn tbl item in
    match field with
    | DTop -> c.loc.top
    | DBottom -> c.loc.bottom
    | DLeft -> c.loc.left
    | DRight -> c.loc.right
;;

let rec calc_loc (screen : Rect.t) field_tbl loc =
    let lookup t = PolyHash.find_exn field_tbl t in
    let recur = calc_loc screen field_tbl in
    let app ~init ~f lst = 
        lst
        |> List.map ~f:recur
        |> List.fold ~init ~f
    in
    let open Constraint in
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
    | FunDep (_, f) -> f field_tbl
    | Const c -> c
;;

let layout_single_item screen deps field_tbl key =
    let loc = sel_field deps key in
    let res = calc_loc screen field_tbl loc in
    PolyHash.set field_tbl key res;
;;

let set_rectangle tbl item =
    let lookup field = PolyHash.find_exn tbl (item, field) in
    let rect = Aabb.{
        x1 = lookup DLeft;
        y1 = lookup DTop;
        x2 = lookup DRight;
        y2 = lookup DBottom;
    } |> RectAabb.rect_of_aabb in
    item#resize rect
;;

let layout rect deps =
    let open DependencyGraph in
    let field_tbl = PolyHash.create() in
    List.iter deps.top_sort (layout_single_item rect deps.constraints field_tbl);
    PolyHash.iter_keys deps.constraints (set_rectangle field_tbl)
;;

class constraintLayout rules =
    let deps = DependencyGraph.calculate_dependency_graph rules in
    let items = List.map rules (fun r -> r.item) |> DynArray.of_list in
object(self)
    inherit Mixins.layout as super

    val id = 0
    val mutable eventHandlers = []
    val mutable snoopers = []
    
    method items = items
    method preferredSize = Size.{w=400.; h=400.}
    method addLayoutable _ = ()
    method removeLayoutable _ = ()

    method layout rect =
        layout rect deps
end
