type 'a tree =
          | Node of { 
              mutable parent : 'a tree option;
              mutable bounds : Rect.t; 
              children : 'a tree DynArray.t 
          }
          | Leaf of {
              mutable parent : 'a tree option;
              mutable bounds : Rect.t;
              data : (Rect.t * 'a) DynArray.t;
          }

type 'a t = {
    mutable root : 'a tree option;
}

let max_child_size = 4

let bounding_box_of_nodes nodes =
    if DynArray.empty nodes then Rect.empty
    else (
        let rect = ref (DynArray.get nodes 0 |> fst) in
        let len = DynArray.length nodes in
        for i=1 to len-1 do
            rect := Rect.union !rect (DynArray.get nodes i |> fst)
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
    loop tree
;;

let enlargement_amount (rect1, rect2) =
    let union = Rect.union rect1 rect2 in
    union.w -. rect1.w +. union.h -. rect1.h, union.w*.union.h
;;

let choose_leaf (tree, rect) =
    let scratch = DynArray.make max_child_size in
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
let pick_seeds nodes =
    let get = DynArray.get nodes in
    let calc_cost (i, j) =
        let rect1 = get i 
        and rect2 = get j in
        let total_rect = Rect.union rect1 rect2 in
        (Rect.area total_rect) -. (Rect.area rect1) -. (Rect.area rect2)
    in
    let best_area = ref Float.max_value in
    let best_pair = ref (get 0, get 1) in
    let len = DynArray.length nodes in
    for i=0 to len-1 do
        for j=0 to len-1 do
            if i <> j then (
                let area = calc_cost(i, j) in 
                if Float.(area > !best_area) then (
                    best_area := area;        
                    best_pair := (get i, get j);
                )
            )
        done; 
    done;
    !best_pair
;;

let split_quadratic node =
    (* When splitting set both parents to be
     * the same. The adjust_tree will handle
     * any parents that now have too many children *)
    node, node
;;

let split_node = split_quadratic

let rec fixup_parent_rects rect = function
    | None -> ()
    | Some (Leaf _) -> failwith "Impossible fixup_parent_rects"
    | Some (Node values) ->
        values.bounds <- Rect.union values.bounds rect;
        fixup_parent_rects values.bounds values.parent
;;

let too_many_children arr = DynArray.length arr > max_child_size

let rec root_of = function
    | Node {parent=None} as n -> Some n
    | Leaf {parent=None} as l -> Some l
    | Leaf {parent=Some p} -> root_of p
    | Node {parent=Some p} -> root_of p
;;

let rec adjust_tree (l, ll) =
    let create_new_root (l, ll, bounds1, bounds2) =
        let children = DynArray.make max_child_size in
        DynArray.add children l;
        DynArray.add children ll;
        Some (Node {
            parent=None;
            bounds = Rect.union bounds1 bounds2;
            children;
        })
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
    match tree.root with
    | None ->
        let arr = DynArray.make max_child_size in
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
                tree.root <-
                    node
                    |> split_node
                    |> adjust_tree
            ) else (
                (* Walk up the tree updating all parents bounds *)
                fixup_parent_rects data.bounds data.parent
            )
;;
