open Mixins

class virtual linearLayout =
object
    inherit layout
    val id = 0
    val items : layoutable DynArray.t = DynArray.make 1

    method addLayoutable l = DynArray.add items l

    method removeLayoutable id =
        DynArray.filter (fun l -> l#id <> id) items

    method items = DynArray.to_list items
end

type grid_data = {
    pos : Pos.t;
    span : Size.t;
    item : layoutable;
}

class gridLayout dimx dimy =
object(self)
    inherit layout

    val id = 0
    val gridSize = Size.{w=Float.of_int dimx; h=Float.of_int dimy}

    val mutable items : grid_data list = []

    method items = List.map items (fun data -> data.item)

    method private calcAbsArrays =
        let max_x = Array.create dimx 0.
        and max_y = Array.create dimy 0. in
        List.iter items (fun data ->
            let sz = data.item#preferredSize in
            let sz = Size.{w=sz.w /. data.span.w; h=sz.h /. data.span.h} in
            let x_start = Int.of_float data.pos.x in
            let x_end   = x_start + Int.(of_float data.span.w) in
            let y_start = Int.of_float data.pos.y in
            let y_end   = y_start + Int.(of_float data.span.h) in
            for i=x_start to x_end-1 do
                max_x.(i) <- Float.max max_x.(i) sz.w
            done;
            for i=y_start to y_end-1 do
                max_y.(i) <- Float.max max_y.(i) sz.h
            done;
        );
        max_x, max_y

    method private calcRatioArrays =
        let max_x, max_y = self#calcAbsArrays in
        let sum_x = Array.fold max_x ~init:0. ~f:Float.(+) in
        let sum_y = Array.fold max_y ~init:0. ~f:Float.(+) in
        let ratio arr sum =
            Array.iteri arr (fun idx v ->
                arr.(idx) <- v /. sum)
        in
        ratio max_x sum_x, ratio max_y sum_y

    method preferredSize =
        let max_x, max_y = self#calcAbsArrays in
        (* Find the max dims in each row/col *)
        let sum_x = Array.fold max_x ~init:0. ~f:Float.(+) in
        let sum_y = Array.fold max_y ~init:0. ~f:Float.(+) in
        Size.{w=sum_x; h=sum_y}

    method removeLayoutable id =
        items <- List.filter items (fun data ->
            data.item#id <> id
        )

    method addLayoutable l =
        (* TODO - find next available cell *)
        failwith "Use addToCell"

    method addToCell ?(rows=1) ?(cols=1) (x : int) (y : int) item = 
        let pos = Pos.{x=Float.of_int x; y=Float.of_int y} in
        let span = Size.{w=Float.of_int cols; h=Float.of_int rows} in
        items <- {pos; span; item} :: items

    method layout r =
        let cellSize = Size.{w=r.w /. gridSize.w; h=r.h /. gridSize.h} in
        (* TODO - use the ratio arrays *)
        (*let xarr, yarr = self#calcRatioArrays in*)
        List.iter items (fun meta_data ->
            meta_data.item#resize {
                x=r.x +. cellSize.w*.meta_data.pos.x*.2.;
                y=r.y +. cellSize.h*.meta_data.pos.y*.2.;
                w=cellSize.w*.meta_data.span.w*.2.;
                h=cellSize.h*.meta_data.span.h*.2.;
            }
        )
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

