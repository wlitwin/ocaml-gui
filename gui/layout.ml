open Mixins

class virtual linearLayout =
object(self)
    inherit layout
    val id = 0
    val items : layoutable DynArray.t = DynArray.make 1
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable distributeEvenly = true

    method clear = DynArray.clear items
    method setDistributeEvenly b = distributeEvenly <- b

    method addLayoutable l = DynArray.add items (l :> layoutable)

    method removeLayoutable id =
        DynArray.filter (fun l -> l#id <> id) items

    method items = items 

    method private virtual ratioLayout : Rect.t -> unit
    method private virtual preferredLayout : Rect.t -> unit

    method layout rect =
        if distributeEvenly then self#ratioLayout rect
        else self#preferredLayout rect
end

type grid_data = {
    pos : Pos.t;
    span : Size.t;
    item : layoutable;
}

class fullLayout (item : Mixins.layoutable) =
object(self)
    inherit layout
    val id = 0
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable items = DynArray.of_list [item]

    method item = DynArray.get items 0

    method items = items

    method removeLayoutable _ =
        items <- DynArray.create()

    method preferredSize =
        self#preferredSize

    method addLayoutable l =
        DynArray.set items 0 (l :> layoutable)

    method layout r =
        self#item#postEvent (Resize r) |> ignore

end

class gridLayout dimx dimy =
object(self)
    inherit layout

    val id = 0
    val gridSize = Size.{w=Float.of_int dimx; h=Float.of_int dimy}
    val mutable eventHandlers = []
    val mutable snoopers = []

    val mutable items : grid_data DynArray.t = DynArray.create()

    method items = DynArray.map (fun data -> data.item) items

    method private calcAbsArrays =
        let max_x = Array.create dimx 0.
        and max_y = Array.create dimy 0. in
        DynArray.iter (fun data ->
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
        ) items;
        max_x, max_y

    method private calcRatioArrays =
        let max_x, max_y = self#calcAbsArrays in
        let sum_x = Array.fold max_x ~init:0. ~f:Float.(+) in
        let sum_y = Array.fold max_y ~init:0. ~f:Float.(+) in
        let ratio arr sum =
            Array.iteri arr (fun idx v ->
                arr.(idx) <- v /. sum);
            arr
        in
        ratio max_x sum_x, ratio max_y sum_y

    method preferredSize =
        let max_x, max_y = self#calcAbsArrays in
        (* Find the max dims in each row/col *)
        let sum_x = Array.fold max_x ~init:0. ~f:Float.(+) in
        let sum_y = Array.fold max_y ~init:0. ~f:Float.(+) in
        Size.{w=sum_x; h=sum_y}

    method removeLayoutable id =
        DynArray.filter (fun data ->
            data.item#id <> id
        ) items

    method addLayoutable l =
        (* TODO - find next available cell *)
        failwith "Use addToCell"

    method addToCell ?(rows=1) ?(cols=1) (x : int) (y : int) item = 
        let pos = Pos.{x=Float.of_int x; y=Float.of_int y} in
        let span = Size.{w=Float.of_int cols; h=Float.of_int rows} in
        DynArray.add items {pos; span; item}

    method layout r =
        let xarr, yarr = self#calcRatioArrays in
        let calc_prefix arr =
            let len = Array.length arr in
            let out = Array.create len 0. in
            for i=0 to len-1 do
                let sum = ref 0. in
                for j=i-1 downto 0 do
                    sum := !sum +. arr.(j)
                done;
                out.(i) <- !sum
            done;
            out
        in
        let calc_dims data = 
            let x = Int.of_float data.pos.x
            and y = Int.of_float data.pos.y
            and w = Int.of_float data.span.w
            and h = Int.of_float data.span.h in
            let sz_w = ref 0. in
            for i=x to x+w-1 do
                sz_w := !sz_w +. xarr.(i)
            done;
            let sz_h = ref 0. in
            for i=y to y+h-1 do
                sz_h := !sz_h +. yarr.(i)
            done;
            Size.{w = !sz_w; h = !sz_h}
        in
        let prefix_x = calc_prefix xarr
        and prefix_y = calc_prefix yarr in
        DynArray.iter (fun meta_data ->
            let dims = calc_dims meta_data in
            let xoff = prefix_x.(Int.(of_float meta_data.pos.x))
            and yoff = prefix_y.(Int.(of_float meta_data.pos.y)) in
            meta_data.item#resize {
                x=r.x +. xoff*.r.w;
                y=r.y +. yoff*.r.h; 
                w=dims.w*.r.w;
                h=dims.h*.r.h;
            } |> ignore
        ) items
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

