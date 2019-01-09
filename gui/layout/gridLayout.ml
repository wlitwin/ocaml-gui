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

