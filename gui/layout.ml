open Mixins

class virtual linearLayout =
object
    inherit layout
    val id = 0
    val items : layoutable DynArray.t = DynArray.make 1

    method addLayoutable l = DynArray.add items l

    method removeLayoutable id =
        DynArray.filter (fun l -> l#id = id) items

    method items = DynArray.to_list items
end

class virtual gridLayout =
object

end

class horizontalLayout =
object(self)
    inherit linearLayout

    method preferredSize =
        DynArray.fold_left 
              (fun size item -> 
                   let pz = item#preferredSize in
                   Size.{w=size.w +. pz.w; h=Float.max size.h pz.h})
              Size.zero
              items

    method layout rect =
        (* Get ratios of items *)
        let get = DynArray.get in
        let sizes = DynArray.map (fun l -> l#preferredSize.Size.w) items in
        let sum   = DynArray.fold_left ( +. ) 0. sizes in
        let offset = ref rect.x in
        DynArray.iteri (fun idx item ->
            let ratio = (get sizes idx) /. sum in
            let width = ratio *. rect.w in
            item#resize {rect with x = !offset; w = width};
            Stdio.printf "H Layout size %f %f %f %f\n" rect.x rect.y rect.w rect.h;
            offset := !offset +. width;
        ) items
end

class verticalLayout =
object(self)
    inherit linearLayout

    method preferredSize =
        DynArray.fold_left 
            (fun size item -> 
                 let pz = item#preferredSize in
                 Size.{w=Float.max size.w pz.w; h=size.h +. pz.h})
              Size.zero
              items

    method layout rect =
        (* Get ratios of items *)
        let get = DynArray.get in
        let sizes = DynArray.map (fun l -> l#preferredSize.Size.h) items in
        let sum   = DynArray.fold_left ( +. ) 0. sizes in
        let offset = ref rect.y in
        DynArray.iteri (fun idx item ->
            let ratio = (get sizes idx) /. sum in
            let height = ratio *. rect.h in
            item#resize {rect with y = !offset; h = height};
            Stdio.printf "V Layout size %f %f %f %f\n" rect.x rect.y rect.w rect.h;
            offset := !offset +. height;
        ) items
end

class flowLayout =
object(self)
    inherit linearLayout

    method preferredSize =
        DynArray.fold_left
            (fun size item ->
                 let pz = item#preferredSize in
                 Size.{w=size.w +. pz.w; h=Float.max size.h pz.h})
            Size.zero
            items

    method layout r =
        let rowPos = ref Pos.{x=r.x; y=r.y} in
        let sizes = DynArray.map (fun l -> l#preferredSize) items in
        let maxy = ref 0. in
        let add sz = 
            rowPos := Pos.{!rowPos with x= !rowPos.x +. sz.Size.w;};
            maxy := Float.max !maxy sz.h;
        in
        DynArray.iteri (fun idx item ->
            let sz = DynArray.get sizes idx in
            if Float.(!rowPos.x +. sz.w > r.w && not (!rowPos.x = 0.)) then (
                rowPos := Pos.{x=0.; y= !rowPos.y +. !maxy};
                maxy := 0.;
            );
            item#resize Rect.{x= !rowPos.x; y= !rowPos.y; w=sz.w; h=sz.h};
            add sz;
        ) items
end

