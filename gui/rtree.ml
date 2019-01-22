type 'a tree =
          | Node of { 
              mutable parent : 'a tree option;
              mutable bounds : Rect.t; 
              mutable children : 'a tree DynArray.t 
          }
          | Leaf of {
              mutable parent : 'a tree option;
              mutable bounds : Rect.t;
              mutable data : (Rect.t * 'a) DynArray.t;
          }

type 'a t = {
    mutable root : 'a tree option;
}

let create () = {
    root = None;
}

let min_child_size = 2
let max_child_size = 4

let bounds = function
    | Leaf l -> l.bounds
    | Node n -> n.bounds
;;

let set_parent p = function
    | Leaf l -> l.parent <- Some p
    | Node n -> n.parent <- Some p
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

let search (tree, rect, output) =
    DynArray.clear output;
    let rec loop = function
        | Node {bounds; children} ->
            if Rect.overlaps(rect, bounds) then 
                DynArray.iter loop children
        | Leaf {data} ->
            DynArray.iter (fun (bounds, data) ->
                if Rect.overlaps(rect, bounds) then
                    DynArray.add output data
            ) data
    in
    match tree.root with
    | None -> ()
    | Some root -> loop root
;;

let enlargement_amount (rect1, rect2) =
    let union = Rect.union rect1 rect2 in
    union.w -. rect1.w +. union.h -. rect1.h, union.w*.union.h
;;

let choose_leaf (tree, rect) =
    let scratch = DynArray.create ~capacity:max_child_size () in
    let cmp(((enlarge1, area1), node1),
            ((enlarge2, area2), node2)) =
        let cmp1 = Float.to_int (enlarge1 -. enlarge2) in
        if cmp1 = 0 then Float.to_int (area1 -. area2)
        else cmp1
    in
    let best_node nodes =
        DynArray.clear scratch;
        DynArray.iter (function
            | Node {bounds}
            | Leaf {bounds} as node -> 
                DynArray.add scratch
                    (enlargement_amount(bounds, rect), node)
        ) nodes;
        Util.dynarray_sort(scratch, cmp);
        snd (DynArray.get scratch 0)
    in
    let rec loop = function
        | Leaf _ as leaf -> leaf
        | Node {children} -> 
            loop (best_node children)
    in
    loop tree
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
    let i, j = !best_pair in
    Stdio.printf "BEST PAIR %d %d\n" i j;
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

let has_too_many_children = function
    | Leaf l1 -> too_many_children l1.data
    | Node n1 -> too_many_children n1.children
;;

let split_quadratic node =
    (* When splitting set both parents to be
     * the same. The adjust_tree will handle
     * any parents that now have too many children *)
    assert (has_too_many_children node);
    let do_split (get, arr, bounds, create) =
        let idx1, idx2 = pick_seeds (arr, bounds) in
        let group1 = DynArray.create ~capacity:max_child_size () in
        let group2 = DynArray.create ~capacity:max_child_size () in
        DynArray.add group1 (get idx1);
        DynArray.add group2 (get idx2);
        DynArray.delete arr idx1;
        DynArray.delete arr (if idx2 > idx1 then idx2-1 else idx2);
        let bounds1, bounds2 = pick_all (group1, group2, bounds, arr) in
        let parent, new_obj = create(group1, bounds1, group2, bounds2) in
        begin match parent with
        | Some (Node p) -> DynArray.add p.children new_obj
        | None -> ()
        | _ -> failwith "Impossible split_quadratic"
        end;
        (node, new_obj)
    in
    match node with
    | Leaf l -> 
        let get = DynArray.get l.data in
        do_split (get, l.data, fst, fun (g1, b1, g2, b2) ->
            let l_new = Leaf {
                parent=l.parent;
                bounds=b2;
                data=g2;
            } in
            l.bounds <- b1;
            l.data <- g1;
            l.parent, l_new
        )
    | Node n ->
        let get = DynArray.get n.children in
        do_split (get, n.children, bounds, fun (g1, b1, g2, b2) ->
            let n_new = Node {
                parent=n.parent;
                bounds=b2;
                children=g2;
            } in
            DynArray.iter (fun obj ->
                set_parent n_new obj
            ) g2;
            n.bounds <- b1;
            n.children <- g1;
            n.parent, n_new
        )
;;

let split_node = split_quadratic

let rec fixup_parent_rects rect = function
    | None -> ()
    | Some (Leaf _) -> failwith "Impossible fixup_parent_rects"
    | Some (Node values) ->
        values.bounds <- Rect.union values.bounds rect;
        fixup_parent_rects values.bounds values.parent
;;

let rec root_of = function
    | Node {parent=None} as n -> Some n
    | Leaf {parent=None} as l -> Some l
    | Leaf {parent=Some p} -> root_of p
    | Node {parent=Some p} -> root_of p
;;

let rec adjust_tree (l, ll) =
    let create_new_root (l, ll, bounds1, bounds2) =
        let children = DynArray.create ~capacity:max_child_size () in
        DynArray.add children l;
        DynArray.add children ll;
        let new_root = Node {
            parent=None;
            bounds = Rect.union bounds1 bounds2;
            children;
        } in
        set_parent new_root l;
        set_parent new_root ll;
        Some new_root
    in
    let add_to_parent = function 
    | (Node p as parent, bounds) ->
        if too_many_children p.children then (
            (Node p)
            |> split_node
            |> adjust_tree
        ) else (
            fixup_parent_rects bounds (Some parent);
            root_of l
        )
    | _ -> failwith "Impossible adjust_tree"
    in
    match l, ll with
    | Node ({parent=None} as n1), Node n2 ->
            create_new_root (l, ll, n1.bounds, n2.bounds)
    | Leaf ({parent=None} as l1), Leaf l2 ->
            create_new_root (l, ll, l1.bounds, l2.bounds)
    | Leaf ({parent=Some (Node p)} as l1), Leaf l2 ->
            let bounds = Rect.union l1.bounds l2.bounds in
            add_to_parent (Node p, bounds);
    | Node ({parent=Some (Node p)} as n1), Node n2 ->
            let bounds = Rect.union n1.bounds n2.bounds in
            add_to_parent (Node p, bounds)
    | _ -> failwith "Impossible adjust_tree 2"
;;

let insert (tree, rect, obj) : unit =
    assert (not (Rect.is_empty rect));
    match tree.root with
    | None ->
        let arr = DynArray.create ~capacity:max_child_size () in
        DynArray.add arr (rect, obj);
        tree.root <- Some (Leaf {
            parent=None;
            bounds=rect;
            data=arr;
        })
    | Some root ->
        let leaf = choose_leaf (root, rect) in
        match leaf with
        | Node _ -> failwith "Shouldn't stop at an intermediate node"
        | Leaf data as node ->
            DynArray.add data.data (rect, obj);
            data.bounds <- Rect.union data.bounds rect;
            if too_many_children data.data then (
                let a, b = split_node node in
                let new_root = adjust_tree (a, b) in
                tree.root <- new_root;
                (*
                tree.root <-
                    node
                    |> split_node
                    |> adjust_tree
                    *)
            ) else (
                (* Walk up the tree updating all parents bounds *)
                fixup_parent_rects data.bounds data.parent
            )
;;

let rec find_leaf (node, rect, pred) : (int * 'a tree) option =
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
    match node with 
    | Leaf {data} ->
        until (data, fun (idx, item) -> 
            if pred (snd item) then (Some (idx, node))
            else None
        )
    | Node {bounds; children} ->
            let open Rect in
        Stdio.printf "here %f %f %f %f - %f %f %f %f\n"
        bounds.x bounds.y bounds.w bounds.h
        rect.x rect.y rect.w rect.h;
        Stdio.printf "OVERLAPS %b\n" (Rect.overlaps (bounds, rect));
        if Rect.overlaps (bounds, rect) then (
            Stdio.printf "overlaps\n";
            until (children, fun (_, node) ->
                find_leaf (node, rect, pred)
            )
        ) else
            None
;;

let shrink_bounds = function
    | Leaf l -> l.bounds <- bounding_box_of_array(l.data, fst)
    | Node n -> n.bounds <- bounding_box_of_array(n.children, bounds)
;;

let rec condense_parent_rects = function
    | None -> ()
    | Some (Leaf l) -> 
        l.bounds <- bounding_box_of_array(l.data, fst);
        condense_parent_rects l.parent
    | Some (Node n) ->
        n.bounds <- bounding_box_of_array(n.children, bounds);
        condense_parent_rects n.parent
;;

let depth_node n =
    let rec loop d = function
        | Leaf _ -> d
        | Node {children} ->
            loop (d+1) (DynArray.get children 0)
    in
    loop 1 n
;;

let depth tree =
    match tree.root with
    | None -> 0
    | Some n -> depth_node n
;;

let best_parent (insert_bounds, children) =
    let len = DynArray.length children in
    let calc_increase idx =
        let node = DynArray.get children 0 in
        let node_bounds = bounds node in
        let union = Rect.union insert_bounds node_bounds in
        {idx; diff=Rect.area union -. Rect.area node_bounds}
    in
    let best = ref (calc_increase 0) in
    for idx=1 to len-1 do
        let diff = calc_increase idx in
        if Float.(diff.diff < !best.diff) then
            best := diff
    done;
    DynArray.get children !best.idx
;;

let insert_eliminations (tree, eliminations) =
    (* Reinsert any eliminations *)
    let tree_depth = depth tree in
    let root = Option.value_exn tree.root in
    let rec insert_node = function
        | Node n as node ->
            let node_depth = depth_node node in
            let diff = tree_depth - node_depth in
            let rec drill (d, parent) =
                if d <= 0 then (
                    n.parent <- Some parent;
                    match parent with 
                    | Node pn ->
                        DynArray.add pn.children node
                    | _ -> failwith "cannot be leaf"
                ) else (
                    (* Figure out which path is best *)
                    let new_parent =
                        match parent with 
                        | Node pn -> best_parent (n.bounds, pn.children)
                        | _ -> failwith "cannot be leaf"
                    in
                    drill (d-1, new_parent)
                )
            in
            drill (diff, root)
        | Leaf {data} -> 
            DynArray.iter (fun (rect, obj) ->
                insert (tree, rect, obj);
            ) data
    in
    let len = DynArray.length eliminations in
    for i=0 to len-1 do
        match DynArray.get eliminations i with 
        | Leaf _ as leaf -> insert_node leaf
        | Node {children} ->
            DynArray.iter (fun child ->
                (* Just add to parent who has same depth *)
                insert_node child;
            ) children

    done;
    (*
    DynArray.iter (fun item ->
        Stdio.printf "ITER\n%!";
    ) eliminations;
    *)
;;

let shrink_tree tree =
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

let condense_tree (tree, leaf) =
    let eliminations = DynArray.create ~capacity:5 () in
    let rec loop leaf =
        let eliminate (parent, children) =
            DynArray.add eliminations leaf;
            shrink_bounds parent;
            let before = DynArray.length children in
            DynArray.filter (fun item -> not (phys_equal item leaf)) children;
            let after = DynArray.length children in
            Stdio.printf "BEFORE %d AFTER %d\n%!" before after;
            assert (after = before - 1 || after = before);
            loop parent;
        in
        match leaf with
        | Leaf {parent=None}
        | Node {parent=None} -> ()
        | Node ({parent=Some (Node p as parent)} as n) when too_few_children n.children ->
            Stdio.printf "Eliminate node\n%!";
            n.parent <- None;
            eliminate (parent, p.children);
        | Leaf ({parent=Some (Node p as parent); data} as l) when too_few_children data ->
            Stdio.printf "Eliminate leaf\n%!";
            l.parent <- None;
            eliminate (parent, p.children);
        | Node {parent=Some (Node _)}
        | Leaf {parent=Some (Node _)} ->
            Stdio.printf "Condense parent rects\n%!";
            condense_parent_rects (Some leaf)
        | _ -> failwith "impossible condense_tree"
    in
    loop leaf;
    insert_eliminations (tree, eliminations);
    shrink_tree tree;
;;

let delete (tree, rect, pred) =
    match tree.root with
    | None -> ()
    | Some root ->
        match find_leaf (root, rect, pred) with
        | None -> ()
        | Some (idx, (Leaf l as leaf)) ->
            Stdio.printf "Found node to delete\n%!";
            DynArray.delete l.data idx;
            shrink_bounds leaf;
            condense_tree (tree, leaf);
        | Some _ -> failwith "find_leaf error"
;;
