type z = Z : z
type 'n s = S : 'n s

type 'a leaf_rec = {
    mutable l_bounds : Rect.t;
    mutable data : (Rect.t * 'a) DynArray.t;
}

type 'a node_rec = {
    mutable n_bounds : Rect.t;
    mutable children : 'a rtree DynArray.t;
}

and 'a rtree =
    | RLeaf : { mutable bounds:Rect.t; mutable data:(Rect.t * 'a) DynArray.t } -> ('a * z) rtree
    | RNode : { mutable bounds:Rect.t; mutable children: 'a any_tree DynArray.t } -> ('a * 'n s) rtree

and 'a any_tree = Ex : ('a * 'b) rtree -> 'a any_tree

type 'a any_node = N : ('a * 'b s) rtree -> 'a any_node

type 'a t = {
    mutable root : 'a any_tree;
}

let bounds : type a. a rtree -> Rect.t  = function
    | RLeaf {bounds} -> bounds
    | RNode {bounds} -> bounds
;;
    
let enlargement_amount (rect1, rect2) =
    let union = Rect.union rect1 rect2 in
    union.w -. rect1.w +. union.h -. rect1.h, union.w*.union.h
;;

type 'a index_list = (int * 'a any_node) list

let min_child_size = 2
let max_child_size = 10

let choose_leaf : type a b c. (a * b) rtree * Rect.t -> (a index_list * (a * z) rtree) = function
    | tree, rect ->
        let cmp(((enlarge1, area1)),
                ((enlarge2, area2))) =
            let cmp1 = Float.to_int (enlarge1 -. enlarge2) in
            if cmp1 = 0 then Float.to_int (area1 -. area2)
            else cmp1
        in
        let best_node nodes =
            let Ex node_0 = DynArray.get nodes 0 in
            let index = ref 0 in
            let best_enlargement = ref (enlargement_amount (bounds node_0, rect)) in
            let best_node = ref (Ex node_0) in
            for i=1 to DynArray.length nodes do
                let Ex node = DynArray.get nodes i in
                let enlarge = enlargement_amount (bounds node, rect) in
                if cmp(enlarge, !best_enlargement) < 0 then (
                    index := i;
                    best_node := Ex node;
                    best_enlargement := enlarge;
                )
            done;
            !index, !best_node
        in
        let rec loop : type a b c. (a index_list * (a * b) rtree) -> (a index_list * (a * z) rtree) = function
            | path, (RLeaf _ as leaf) -> path, leaf
            | path, (RNode {children} as node) -> 
                let index, Ex best_node = best_node children in
                let path = (index, N node) :: path in
                loop (path, best_node)
        in
        (loop ([], tree))
;;

(* Calculate pair with largest union'd area *)
let pick_seeds (nodes, bounds) =
    let get = DynArray.get nodes in
    let calc_cost (i, j) =
        let rect1 = bounds (get i)
        and rect2 = bounds (get j) in
        let total_rect = Rect.union rect1 rect2 in
        (Rect.area total_rect) -. (Rect.area rect1) -. (Rect.area rect2)
    in
    let best_area = ref Float.min_value in
    let best_pair = ref (0, 0) in
    let len = DynArray.length nodes in
    for i=0 to len-1 do
        for j=0 to len-1 do
            if i <> j then (
                let area = calc_cost(i, j) in 
                if Float.(area > !best_area) then (
                    best_area := area;        
                    best_pair := (i, j);
                )
            )
        done; 
    done;
    !best_pair
;;

type diff = {
    idx : int;
    diff : float;
}

let pick_all (group1, group2, bounds, rest) =
    let b1 = ref (bounds (DynArray.get group1 0))
    and b2 = ref (bounds (DynArray.get group2 0)) in
    let calc_diff idx =
        let b = bounds (DynArray.get rest idx) in
        let g1 = Rect.union b !b1
        and g2 = Rect.union b !b2 in
        let increase1 = Rect.area g1 -. Rect.area !b1
        and increase2 = Rect.area g2 -. Rect.area !b2 in
        let diff = increase1 -. increase2 in
        {idx; diff}
    in
    let update_group (diff, group, rect) =
        let value = DynArray.get rest diff.idx in
        DynArray.add group value;
        DynArray.delete rest diff.idx;
        rect := Rect.union !rect (bounds value);
    in
    while not (DynArray.empty rest) do
        (* Calculate which item has the
         * largest difference between the
         * groups, and put that one in the
         * respective group *)
        let len = DynArray.length rest in
        let diff = ref (calc_diff 0) in 
        for i=1 to len-1 do
            let new_diff = calc_diff i in
            if Float.(new_diff.diff > !diff.diff) then (
                diff := new_diff
            )
        done;
        (* Add the chosen diff to the proper group and update *)
        if Float.(!diff.diff > 0.) then (
            update_group (!diff, group1, b1);
        ) else (
            update_group (!diff, group2, b2);
        )
    done;
    !b1, !b2
;;

let too_many_children arr = DynArray.length arr > max_child_size
let too_few_children arr = DynArray.length arr < min_child_size

let has_too_many_children : type a. a rtree -> bool = function
    | RLeaf l1 -> too_many_children l1.data
    | RNode n1 -> too_many_children n1.children
;;

let split_quadratic : type a b. (a * b) rtree -> ((a * b) rtree * (a * b) rtree) = function
| node ->
    assert (has_too_many_children node);
    let do_split (get, arr, bounds) =
        let idx1, idx2 = pick_seeds (arr, bounds) in
        let group1 = DynArray.create ~capacity:max_child_size () in
        let group2 = DynArray.create ~capacity:max_child_size () in
        DynArray.add group1 (get idx1);
        DynArray.add group2 (get idx2);
        DynArray.delete arr idx1;
        DynArray.delete arr (if idx2 > idx1 then idx2-1 else idx2);
        let bounds1, bounds2 = pick_all (group1, group2, bounds, arr) in
        (group1, bounds1, group2, bounds2)
    in
    match node with
    | RLeaf l -> 
        let get = DynArray.get l.data in
        let g1, b1, g2, b2 = do_split (get, l.data, fst) in
        let l_new = RLeaf {bounds=b2; data=g2} in
        l.bounds <- b1;
        l.data <- g1;
        node, l_new
    | RNode n ->
        let get = DynArray.get n.children in
        let g1, b1, g2, b2 = do_split (get, n.children, (fun (Ex n) -> bounds n)) in
        let n_new = RNode {bounds=b2; children=g2} in
        n.bounds <- b1;
        n.children <- g1;
        node, n_new
;;

let split_node = split_quadratic

let rec propagate_bounds_upward = function
    | [], _ -> ()
    | (_, N (RNode node)) :: tl, rect ->
        node.bounds <- Rect.union node.bounds rect;
        propagate_bounds_upward (tl, node.bounds)
;;

let rec adjust_tree : type a b c d. (int * a any_node) list * (a * b) rtree * (a * b) rtree -> (a * d s) rtree = 
    let create_new_root (l, ll, bounds1, bounds2) =
        let children = DynArray.create ~capacity:max_child_size () in
        DynArray.add children (Ex l);
        DynArray.add children (Ex ll);
        RNode {
            bounds = Rect.union bounds1 bounds2;
            children;
        }
    in
    function
    | [], l, ll -> create_new_root (l, ll, bounds l, bounds ll)
    | (index, N (RNode n_rec as node)) :: tl, l, ll ->
        (* Need to remove l and ll from the parent? *)
        DynArray.delete n_rec.children index;
        DynArray.add n_rec.children (Ex l);
        DynArray.add n_rec.children (Ex ll);
        if too_many_children n_rec.children then (
            let left, right = split_node node in
            adjust_tree (tl, left, right)
        ) else (
            node
        )
;;

let insert = function 
| tree, rect, obj ->
    assert (not (Rect.is_empty rect));
    match tree.root with
    | Ex (RLeaf {data}) when DynArray.length data < max_child_size ->
        DynArray.add data (rect, obj);
    | Ex (RLeaf _ as leaf) ->
         let RLeaf left, RLeaf right = split_node leaf in
         tree.root <- Ex (RNode {
             bounds = Rect.union left.bounds right.bounds;
             children = DynArray.of_list [Ex (RLeaf left); Ex (RLeaf right)]
         })
    | Ex node ->
        match choose_leaf (node, rect) with
        | path, (RLeaf data as node) ->
            DynArray.add data.data (rect, obj);
            data.bounds <- Rect.union data.bounds rect;
            if too_many_children data.data then (
                let left, right = split_node node in
                let new_root = adjust_tree (path, left, right) in
                tree.root <- Ex new_root;
            ) else (
                propagate_bounds_upward (path, data.bounds)
            )
;;

let rec find_leaf : type a b c. (a * b) rtree * Rect.t * (a -> bool) -> (int * a index_list * (a * z) rtree) option = function
| node, rect, pred ->
    let rec until (arr, f) =
        let len = DynArray.length arr in
        let rec loop idx =
            if idx >= len then None
            else (
                let item = DynArray.get arr idx in
                match f (idx, item) with
                | Some _ as s -> s
                | None -> loop (idx+1)
            )
        in
        loop 0
    in
    let rec loop : (a index_list * a any_tree) -> (int * a index_list * (a * z) rtree) option 
    = function
    | path, Ex (RLeaf {data} as leaf) ->
            until (data, fun (idx, (_, item)) ->
                if pred item then Some (idx, path, leaf)
                else None
            )
    | path, Ex (RNode {bounds; children} as n) ->
        if Rect.overlaps (bounds, rect) then (
            until (children, fun (idx, node) ->
                loop ((idx, N n) :: path, node)
            )
        ) else
            None
    in
    loop ([], Ex node)
;;

let bounding_box_of_array (nodes, bounds) =
    if DynArray.empty nodes then Rect.empty
    else (
        let rect = ref (DynArray.get nodes 0 |> bounds) in
        let len = DynArray.length nodes in
        for i=1 to len-1 do
            rect := Rect.union !rect (DynArray.get nodes i |> bounds)
        done;
        !rect
    )
;;


let shrink_bounds : type a. a rtree -> unit = function
    | RLeaf l -> l.bounds <- bounding_box_of_array(l.data, fst)
    | RNode n -> n.bounds <- bounding_box_of_array(n.children, fun (Ex n) -> bounds n)
;;

let search : 'a t * Rect.t * 'a DynArray.t -> unit = function
| tree, rect, output ->
    DynArray.clear output;
    let rec loop = function
        | Ex (RNode {bounds; children}) ->
            if Rect.overlaps(rect, bounds) then 
                DynArray.iter loop children
        | Ex (RLeaf {data}) ->
            DynArray.iter (fun (bounds, data) ->
                if Rect.overlaps(rect, bounds) then
                    DynArray.add output data
            ) data
    in
    loop tree.root
;;

(*
let shrink_tree tree = function
    | [] -> ()
    | Ex (RNode node) :: tl -> ()
    | Ex (RLeaf node) :: tl -> ()
    (* Check if root only has one child *)
    match tree.root with
    | Some (Leaf {data}) when DynArray.empty data ->
        tree.root <- None
    | Some (Node {children}) when DynArray.length children = 1 ->
        let child = DynArray.get children 0 in
        begin match child with
        | Leaf l -> l.parent <- None
        | Node n -> n.parent <- None
        end;
        tree.root <- Some child
    | _ -> ()
;;
*)
let condense_tree : type a. a t * a index_list * (a * z) rtree -> unit = function 
| tree, [], (RLeaf l) -> l.bounds <- bounding_box_of_array (l.data, fst)
| tree, path, (RLeaf l as leaf) ->
    if too_few_children l.data then (
        (* Need to eliminate this leaf node and propagate upwards *)
        ()
    ) else (
        (* Just update the parent rects *)
        l.bounds <- bounding_box_of_array (l.data, fst);
        List.iter path (fun (_, N (RNode n)) ->
            n.bounds <- bounding_box_of_array (n.children, fun (Ex n) -> bounds n);
        )
    )

    let eliminations = DynArray.create ~capacity:5 () in
    let rec loop leaf =
        let eliminate (parent, children) =
            DynArray.add eliminations leaf;
            shrink_bounds parent;
            let before = DynArray.length children in
            DynArray.filter (fun item -> not (phys_equal item leaf)) children;
            let after = DynArray.length children in
            assert (after = before - 1 || after = before);
            loop parent;
        in
        match leaf with
        | RLeaf {parent=None}
        | RNode {parent=None} -> ()
        | RNode ({parent=Some (Node p as parent)} as n) when too_few_children n.children ->
            n.parent <- None;
            eliminate (parent, p.children);
        | RLeaf ({parent=Some (Node p as parent); data} as l) when too_few_children data ->
            l.parent <- None;
            eliminate (parent, p.children);
        | RNode {parent=Some (Node _)}
        | RLeaf {parent=Some (Node _)} ->
            condense_parent_rects (Some leaf)
        | _ -> failwith "impossible condense_tree"
    in
    loop leaf;
    insert_eliminations (tree, eliminations);
;;

let delete : 'a t * Rect.t * ('a -> bool) -> unit = function
| tree, rect, pred ->
    let Ex root = tree.root in
    match find_leaf (root, rect, pred) with
    | None -> ()
    | Some (idx, path, (RLeaf l as leaf)) ->
        DynArray.delete l.data idx;
        shrink_bounds leaf;
        condense_tree (tree, path, leaf);
;;
