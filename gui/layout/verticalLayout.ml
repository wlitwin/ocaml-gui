class ['a, 'b] verticalLayout renderer =
object(self)
    inherit ['a, 'b] LinearLayout.linearLayout renderer

    method preferredSize =
        DynArray.fold_left 
            (fun size item -> 
                 let pz = item#preferredSize in
                 Size.{w=Float.max size.w pz.w; h=size.h +. pz.h})
              Size.zero
              items

    method private ratioLayout rect =
        (* Get ratios of items *)
        let get = DynArray.get in
        let sizes = DynArray.map (fun l -> l#preferredSize.Size.h) items in
        let sum   = DynArray.fold_left ( +. ) 0. sizes in
        let offset = ref rect.y in
        DynArray.iteri (fun idx item ->
            let ratio = (get sizes idx) /. sum in
            let height = ratio *. rect.h in
            item#resize {rect with y = !offset; h = height} |> ignore;
            offset := !offset +. height;
        ) items

    method private preferredLayout rect =
        let yOff = ref rect.y in
        DynArray.iter (fun item ->
            let height = item#preferredSize.Size.h in
            item#resize {rect with y = !yOff; h = height} |> ignore;
            yOff := height +. !yOff;
        ) items
end
