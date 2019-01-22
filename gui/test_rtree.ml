type item = {
    id : int;
    bounds : Rect.t;
}

let str_of_obj p =
    Printf.sprintf "0x%x" (Int.rem (Caml.Obj.magic p) 8192)
;;

let str_parent = function
    | None -> "None"
    | Some p -> str_of_obj p

let str_node node = 
    let open Rtree in
    let rec loop idnt = 
        let pad = String.make idnt ' ' in
        function
        | Leaf {bounds; data; parent} as l ->
            Printf.sprintf "%sLeaf [%s / %s] {%.2f %.2f %.2f %.2f}\n%s"
            pad (str_parent parent) (str_of_obj l) bounds.x bounds.y bounds.w bounds.h 
            (DynArray.fold_left (fun str (_, item) ->
                str ^ Printf.sprintf "%s %d {%.2f %.2f %.2f %.2f}\n"
                pad item.id item.bounds.x item.bounds.y item.bounds.w item.bounds.h
            ) "" data)
        | Node {bounds; children; parent} as n ->
            Printf.sprintf "%sNode [%s / %s] {%.2f %.2f %.2f %.2f}\n%s"
            pad (str_parent parent) (str_of_obj n) bounds.x bounds.y bounds.w bounds.h 
            (DynArray.fold_left (fun str node -> 
                str ^ loop (idnt+2) node
            ) "" children)
    in
    loop 0 node
;;

let str_tree tree =
    match tree.Rtree.root with
    | None -> "Empty tree"
    | Some root -> str_node root
;;

let print_tree tree =
    Stdio.printf "%s\n%!" (str_tree tree)
;;

let mk id bounds = {
    id;
    bounds;
}

let insert tree item =
    Rtree.insert (tree, item.bounds, item)
;;

let delete tree item =
    Rtree.delete (tree, item.bounds, fun i -> i.id = item.id)
;;

let test1 = 
    let tree = Rtree.create() in
    let item1 = mk 0 Rect.{x=0.; y=0.; w=10.; h=10.} in
    let item2 = mk 1 Rect.{x=10.; y=10.; w=10.; h=10.} in
    let item3 = mk 2 Rect.{x=20.; y=20.; w=10.; h=10.} in
    let item4 = mk 3 Rect.{x=5.; y=5.; w=5.; h=5.} in
    let item5 = mk 4 Rect.{x=1.; y=2.; w=5.; h=5.} in
    let item6 = mk 5 Rect.{x=1.; y=3.; w=5.; h=5.} in
    let item7 = mk 6 Rect.{x=1.; y=4.; w=5.; h=5.} in
    let item8 = mk 7 Rect.{x=1.; y=5.; w=5.; h=5.} in
    let item9 = mk 8 Rect.{x=1.; y=6.; w=5.; h=5.} in
    let item10 = mk 9 Rect.{x=1.; y=7.; w=5.; h=5.} in
    let item11 = mk 10 Rect.{x=1.; y=8.; w=5.; h=5.} in
    let item12 = mk 11 Rect.{x=1.; y=9.; w=5.; h=5.} in
    let item13 = mk 12 Rect.{x=1.; y=10.; w=5.; h=5.} in
    let item14 = mk 13 Rect.{x=1.; y=11.; w=5.; h=5.} in
    insert tree item1;
    insert tree item2;
    insert tree item3;
    insert tree item4;
    insert tree item5;
    print_tree tree;
    delete tree item1;
    print_tree tree;
    insert tree item1;
    print_tree tree;
    insert tree item6;
    insert tree item7;
    insert tree item8;
    insert tree item9;
    insert tree item10;
    print_tree tree;
    insert tree item11;
    insert tree item12;
    insert tree item13;
    insert tree item14;
    print_tree tree;
;;

let _ = 
    let tree = Rtree.create() in
    let item1 = mk 0 Rect.{x=0.; y=0.; w=10.; h=10.} in
    let item2 = mk 1 Rect.{x=10.; y=10.; w=10.; h=10.} in
    let item3 = mk 2 Rect.{x=20.; y=20.; w=10.; h=10.} in
    let item4 = mk 3 Rect.{x=5.; y=5.; w=5.; h=5.} in
    let item5 = mk 4 Rect.{x=1.; y=2.; w=5.; h=5.} in
    let item6 = mk 5 Rect.{x=1.; y=3.; w=5.; h=5.} in
    let item7 = mk 6 Rect.{x=1.; y=4.; w=5.; h=5.} in
    let item8 = mk 7 Rect.{x=1.; y=5.; w=5.; h=5.} in
    let item9 = mk 8 Rect.{x=1.; y=6.; w=5.; h=5.} in
    let item10 = mk 9 Rect.{x=1.; y=7.; w=5.; h=5.} in
    let item11 = mk 10 Rect.{x=1.; y=8.; w=5.; h=5.} in
    let item12 = mk 11 Rect.{x=1.; y=9.; w=5.; h=5.} in
    let item13 = mk 12 Rect.{x=1.; y=10.; w=5.; h=5.} in
    let item14 = mk 13 Rect.{x=1.; y=11.; w=5.; h=5.} in
    let item15 = mk 14 Rect.{x=1.; y=12.; w=5.; h=5.} in
    let item16 = mk 15 Rect.{x=1.; y=13.; w=5.; h=5.} in
    let item17 = mk 16 Rect.{x=1.; y=14.; w=5.; h=5.} in
    let item18 = mk 17 Rect.{x=1.; y=15.; w=5.; h=5.} in
    insert tree item1;
    insert tree item2;
    insert tree item3;
    insert tree item4;
    insert tree item5;
    delete tree item1;
    insert tree item1;
    insert tree item6;
    insert tree item7;
    insert tree item8;
    insert tree item9;
    insert tree item10;
    insert tree item11;
    insert tree item12;
    insert tree item13;
    insert tree item14;
    print_tree tree;
    delete tree item1;
    print_tree tree;
    insert tree item15;
    insert tree item16;
    insert tree item17;
    insert tree item18;
    Stdio.printf "=====================================\n";
    Stdio.printf "=====================================\n";
    Stdio.printf "=====================================\n";
    Stdio.printf "=====================================\n\n\n\n%!";
    delete tree item2;
    delete tree item3;
    delete tree item4;
    delete tree item5;
    delete tree item6;
    delete tree item7;
    delete tree item8;
    delete tree item9;
    delete tree item10;
    delete tree item11;
    delete tree item12;
    delete tree item13;
    delete tree item14;
    delete tree item15;
    delete tree item16;
    delete tree item17;
    delete tree item18;
    print_tree tree;
    insert tree item1;
    insert tree item2;
    print_tree tree;

