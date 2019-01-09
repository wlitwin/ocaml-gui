open LinearLayout

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

    method private ratioLayout _ = ()
    method private preferredLayout _ = ()

    method! layout r =
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
            item#resize Rect.{x= !rowPos.x; y= !rowPos.y; w=sz.w; h=sz.h} |> ignore;
            add sz;
        ) items
end

