module Graphics = Platform.Windowing.Graphics

class ['a, 'b] hexGrid app = 
    let s60 = 0.8660254037844386
    and c60 = 0.5 in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val hexObj = app#renderer#createUserObject
    val mutable index = 0
    val mutable dims = (5, 5)
    val mutable center = true

    method setDims d = dims <- d
    method dims = dims

    method center = center
    method setCentered b = center <- b

    method! onResize (r : Rect.t) : unit =
        super#onResize r;
        hexObj#setBounds r;

    method! onKeyDown (key : Keys.key) : unit =
        index <- Int.rem (index + 1) (fst dims);
        hexObj#redraw

    initializer
        hexObj#setZIndex 1;
        hexObj#setFunc (fun cr ->
            Graphics.save cr;
            Graphics.clip_rect cr rect;
            Graphics.set_color cr Color.white;
            Graphics.rectangle cr rect;
            Graphics.fill cr;

            let dim_x, dim_y = dims in
            let r_count_x = 1.5 *. Float.(of_int dim_x) +. 0.5 in
            let r_x = Float.round_down (rect.w /. r_count_x) in
            let r_count_y = 1.5 *. Float.(of_int dim_y) +. 2.0 in
            let r_y = Float.round_down (rect.h /. r_count_y) in
            let sz = Float.min r_x r_y in
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
            let cx, cy = if center then (
                rect.x +. (rect.w -. sz *. Float.(of_int dim_x) -. sz) *. 0.5,
                rect.y +. (rect.h -. sz *. Float.(of_int dim_y) -. 2.75*.sz) *. 0.5
            ) else 
                rect.x +. sz,
                rect.y +. sz -. (1.0 -. s60)*.sz
            in
            let gray = Color.{r=0.3; g=0.3; b=0.3; a=1.0} in
            for y=0 to dim_y-1 do
                let y_off = Float.(of_int y * 2. * sz * s60) in
                for x=0 to dim_x-1 do
                    let x_f = Float.of_int x in
                    let color = if (x = index) then Color.orange else gray in
                    if Int.(rem x 2 = 0) then (
                        draw_hex (cx+.1.5*.sz*.x_f, cy +. y_off, color);
                    ) else (
                        draw_hex (cx+.sz*.1.5*.x_f, cy+.sz*.s60 +. y_off, color);
                    )
                done;
            done;
            Graphics.restore cr;
        );
        renderObject#addChild hexObj#obj;
end

class ['a, 'b] game app =
    let hexGrid = new hexGrid app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        let cl = fun w -> (w :> ('a, 'b) Layoutable.layoutable) in
        let lGrid = cl hexGrid in
        let rules = ConstraintLayout.(Constraint.([
            {input_item=lGrid; input_loc={
                top=wTop 10.;
                left=wLeft 10.;
                right=wRight ~-.10.;
                bottom=wBottom ~-.10.;
            }}
        ])) in
        let layout = new ConstraintLayout.constraintLayout rules app#renderer in
        self#setLayout (layout :> ('a, 'b) Layout.layout);

    inherit ['a, 'b] Focusable.focusManager app [(hexGrid :> ('a, 'b) HandlesEvent.handles_event)]
end
