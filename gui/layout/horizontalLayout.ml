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

    method private ratioLayout rect =
        (* Get ratios of items *)
        let get = DynArray.get in
        let sizes = DynArray.map (fun l -> l#preferredSize.Size.w) items in
        let sum   = DynArray.fold_left ( +. ) 0. sizes in
        let offset = ref rect.x in
        DynArray.iteri (fun idx item ->
            let ratio = (get sizes idx) /. sum in
            let width = ratio *. rect.w in
            item#resize {rect with x = !offset; w = width} |> ignore;
            offset := !offset +. width;
        ) items

    method private preferredLayout rect =
        let xOff = ref rect.x in
        DynArray.iter (fun item ->
            let width = item#preferredSize.Size.w in
            item#resize {rect with x = !xOff; w = width} |> ignore;
            xOff := width +. !xOff;
        ) items

end

