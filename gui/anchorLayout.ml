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

let get_dep = function
    | ITop (item, _)
    | IBottom (item, _)
    | ILeft (item, _)
    | IRight (item, _) -> Some item
    | _ -> None
;;

let get_loc_deps Location.{top;left;right;bottom} =
    [top;left;right;bottom]
    |> List.map ~f:get_dep 
    |> List.filter ~f:Option.is_some
    |> List.map ~f:(fun o -> Option.value_exn o)
;;

module HP = Hashtbl.Poly
type dep_graph = {
    constraints : (item, constraints) HP.t;
    top_sort : item list;
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

let depth_sort tbl (all : item list) : item list =
    let sorted : item list ref = ref [] in
    let temp : (item, unit) HP.t = HP.create () in
    let perm : (item, unit) HP.t = HP.create () in
    let has_temp i = HP.find temp i |> Option.is_some in
    let has_perm i = HP.find perm i |> Option.is_some in
    let mark_temp i = HP.set temp i () in
    let mark_perm i = HP.set perm i () in
    let rec visit (item : item) =
        if has_perm item then ()
        else if has_temp item then (failwith "Not a DAG")
        else begin
            mark_temp item;
            let lst = HP.find_exn tbl item in
            List.iter lst visit;
            mark_perm item;
            sorted := item :: !sorted;
        end
    in
    List.iter all visit;
    List.rev !sorted
;;

(* Need to build dependency graph *)
let calc_dep_graph lst =
    let deps = {
        constraints = HP.create();
        top_sort = [];
    } in
    let tbl = HP.create() in
    List.iter lst (fun c -> 
        HP.set tbl c.item [];
        HP.set deps.constraints c.item c;
    );
    List.iter lst (fun c ->
        match get_loc_deps c.loc with
        | [] -> ()
        | lst ->
            let lst = HP.find_exn tbl c.item @ lst in
            let lst = List.dedup_and_sort (fun a b -> compare (Caml.Obj.magic a) (Caml.Obj.magic b)) lst in
            HP.set tbl c.item lst;
    );
    let items = List.map lst (fun c -> c.item) in
    {deps with top_sort=depth_sort tbl items}
;;

let calc_loc (screen : Rect.t) loc pref =
    match loc with
    | WTop amt -> screen.y +. amt
    | WLeft amt -> screen.x +. amt
    | WRight amt -> screen.x +. screen.w +. amt
    | WBottom amt -> screen.y +. screen.h +. amt
    | ITop (item, amt) -> item#rect.y +. amt
    | ILeft (item, amt) -> item#rect.x +. amt
    | IRight (item, amt) -> item#rect.x +. item#rect.w +. amt
    | IBottom (item, amt) -> item#rect.y +. item#rect.h +. amt
    | PreferredW -> pref.Size.w
    | PreferredH -> pref.Size.h
;;

let layout_single_item screen deps item =
    let constraints = HP.find_exn deps.constraints item in
    let item = constraints.item in 
    let pref = item#preferredSize in
    let aabb = Aabb.{
        x1 = calc_loc screen constraints.loc.left pref;
        y1 = calc_loc screen constraints.loc.top pref;
        x2 = calc_loc screen constraints.loc.right pref;
        y2 = calc_loc screen constraints.loc.bottom pref;
    } in
    let aabb = 
        match constraints.loc.right with
        | PreferredW -> {aabb with x2 = aabb.x1 +. pref.Size.w}
        | _ -> aabb
    in
    let aabb =
        match constraints.loc.bottom with
        | PreferredH -> {aabb with y2 = aabb.y1 +. pref.Size.h}
        | _ -> aabb
    in
    let rect = RectAabb.(rect_of_aabb aabb) in
    Stdio.printf "Resizing to %f %f %f %f\n" rect.x rect.y rect.w rect.h;
    item#resize rect
;;

let layout_deps rect deps =
    Stdio.printf "Deps! %d\n" (List.length deps.top_sort);
    List.iter deps.top_sort (layout_single_item rect deps)
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
