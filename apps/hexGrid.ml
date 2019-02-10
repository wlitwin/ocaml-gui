module Graphics = Platform.Windowing.Graphics

class ['a, 'b] hexGrid app = 
    let s60 = 0.8660254037844386 in
    let float_pair (x, y) = Float.(of_int x, of_int y) in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val hexObj = app#renderer#createUserObject
    val mutable index = 0
    val mutable dims = (5, 5)

    method setDims d = dims <- d
    method dims = dims

    method private calcSize (w, h) =
        let dim_x, dim_y = float_pair dims in
        let r_x = w /. (1.5 *. dim_x +. 0.5)
        and r_y = h /. (s60 *. (2.*.dim_y +. 1.)) in
        let sz = Float.min r_x r_y in
        Size.{w=sz*.dim_x; h=sz*.dim_y}, sz;

    method! preferredSize =
        let dx, dy = float_pair dims in
        let radius = 30. in
        Size.{w=radius*.(1.5*.dx+.0.5);
              h=radius*.(s60*. (2.*.dy +. 1.))}

    method! onResize (r : Rect.t) : unit =
        super#onResize r;
        hexObj#setBounds r;

    method! onKeyDown (key : Keys.key) : unit =
        index <- Int.rem (index + 1) (fst dims);
        hexObj#redraw

    initializer
        let c60 = 0.5 in
        hexObj#setZIndex 1;
        hexObj#setFunc (fun cr ->
            Graphics.save cr;
            Graphics.clip_rect cr rect;
            Graphics.set_color cr Color.white;
            Graphics.rectangle cr rect;
            Graphics.fill cr;

            let dim_x, dim_y = dims in
            let full_size, sz = self#calcSize (rect.w, rect.h) in
            let rounded f x y = f cr Float.(round_down x) Float.(round_down y) in
            let line_to_rnd = rounded Graphics.line_to in
            let move_to_rnd = rounded Graphics.move_to in
            let draw_hex (cx, cy, c) =
                Graphics.set_color cr c;
                Graphics.beginPath cr;
                move_to_rnd (cx+.c60*.sz) (cy+.s60*.sz);
                line_to_rnd (cx-.c60*.sz) (cy+.s60*.sz);
                line_to_rnd (cx-.sz) cy;
                line_to_rnd (cx-.c60*.sz) (cy-.s60*.sz);
                line_to_rnd (cx+.c60*.sz) (cy-.s60*.sz);
                line_to_rnd (cx+.sz) (cy);
                Graphics.closePath cr;
                Graphics.fill cr;
            in
            let r_x = sz
            and r_y = sz*.s60 in
            let cx, cy = 
                rect.x +. r_x,
                rect.y +. r_y
            in
            let gray = Color.{r=0.3; g=0.3; b=0.3; a=1.0} in
            for y=0 to dim_y-1 do
                let y_off = 2. *. r_y *. Float.(of_int y) in
                for x=0 to dim_x-1 do
                    let x_f = Float.(of_int x * 1.5 * sz) in
                    let color = if (x = index) then Color.orange else gray in
                    if Int.(rem x 2 = 0) then (
                        draw_hex Float.(cx+x_f, cy + y_off, color);
                    ) else (
                        draw_hex Float.(cx+x_f, cy + r_y + y_off, color);
                    )
                done;
            done;
            Graphics.restore cr;
        );
        renderObject#addChild hexObj#obj;
end
