module HP = Hashtbl.Poly

type ('a, 'b) t = {
    rtree : ('a * 'b) Rtree.t;
    object_table : ('a, Rect.t) HP.t;
    cmp : 'a -> ('a * 'b) -> bool;
}

let create cmp = {
    rtree = Rtree.create();
    object_table = HP.create();
    cmp;
}

let add (t, (key, obj), rect) =
    if not (Rect.is_empty rect) && not (HP.mem t.object_table key) then (
        HP.set t.object_table key rect;
        Rtree.insert (t.rtree, rect, (key, obj));
    )

let search (t, rect, results) =
    Rtree.search (t.rtree, rect, results)

let remove (t, key) =
    if HP.mem t.object_table key then (
        let rect = HP.find_exn t.object_table key in
        HP.remove t.object_table key;
        Rtree.delete (t.rtree, rect, t.cmp key)
    )
