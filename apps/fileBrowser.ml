let strs = [|
    "1feowijfeowj";
    "2ofijwoiejfweoij";
    "3foewjfew";
    "4oieweofijwfj";
|]

let build_str () =
    let i1 = Random.int 4 in
    let i2 = Random.int 4 in
    strs.(i1) ^ strs.(i2)

let create_strings =
    let rec f acc n = 
        if n <= 0 then acc
        else f (build_str() :: acc) (n - 1)
    in
    "0000000000000000000000000000000001" :: f [] 10
;;

let readDir app path =
    try
        (*let dir = "." :: ".." :: Core.Sys.ls_dir path in
        dir*)
        create_strings
        |> List.sort ~compare:String.compare
        |> List.map ~f:(fun s -> new Label.label app ~text:s)
    with _ -> 
        [new Label.label app ~text:("Invalid path: " ^ path)]
;;

module Graphics = Platform.Windowing.Graphics

class ['a, 'b] hexObject app = 
    let s60 = 0.8660254037844386
    and c60 = 0.5 in
    let count = 5 in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val hexObj = app#renderer#createUserObject
    val mutable index = 0

    method! onResize (r : Rect.t) : unit =
        super#onResize r;
        hexObj#setBounds r;

    method! onKeyDown (key : Keys.key) : unit =
        index <- Int.rem (index + 1) count;
        hexObj#redraw

    initializer
        hexObj#setZIndex 1;
        hexObj#setFunc (fun cr ->
            Graphics.save cr;
            Graphics.clip_rect cr rect;
            Graphics.set_color cr Color.white;
            Graphics.rectangle cr rect;
            Graphics.fill cr;

            let r_count = 1.5 *. Float.(of_int count) +. 0.5 in
            let r = Float.round_down (rect.w /. r_count) in
            let max_rows = Float.(round_down (rect.h /. (r *. 1.5)) |> to_int) - 1 in
            let sz = r in
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
            let cx = rect.x +. r
            and cy = rect.y +. r -. (1.0 -. s60)*.r in
            let gray = Color.{r=0.3; g=0.3; b=0.3; a=1.0} in
            for y=0 to max_rows-1 do
                let y_off = Float.(of_int y * 2. * r * s60) in
                for x=0 to count-1 do
                    let x_f = Float.of_int x in
                    let color = if (x = index) then Color.orange else gray in
                    if Int.(rem x 2 = 0) then (
                        draw_hex (cx+.1.5*.r*.x_f, cy +. y_off, color);
                    ) else (
                        draw_hex (cx+.r*.1.5*.x_f, cy+.r*.s60 +. y_off, color);
                    )
                done;
            done;
            Graphics.restore cr;
        );
        renderObject#addChild hexObj#obj;
end

class ['a, 'b] fileBrowser app =
    let lblPath = new Label.label app ~text:"Path" in
    let txtPath = new TextBox.textBoxWidget app in
    let fileList = new ListBox.listBox app in
    let hexObj = new hexObject app in
    let scroll = new Scroll.scrollArea app (fileList :> ('a, 'b) Widget.basicWidget) in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    method private setPath p =
        txtPath#setText p;
        self#updatePath;

    method private updatePath =
        let lbls = 
            Util.timeit "read-dir" (fun _ ->
            readDir app txtPath#text) in
        fileList#setContents (List.map lbls (fun i -> 
            (i :> ('a, 'b) Layoutable.layoutable)));
        scroll#resize scroll#fullRect;

    initializer
        self#setPath "/";
        fileList#setBGColor Color.red;
        txtPath#events#addFn `KeyDown (object
            method call evt = match evt#arg with
            | `KeyDownArg Keys.Enter ->
                    self#updatePath
            | _ -> ()
        end);
        let cl = fun w -> (w :> ('a, 'b) Layoutable.layoutable) in
        let lPath = cl lblPath in
        let tPath = cl txtPath in
        let lScroll = cl scroll in
        let lCustom = cl hexObj in
        let from_bot = ConstraintLayout.(Constraint.(Add [|
            wBottom ~-.50.;
            (*Mul [| Const ~-.1.; preferredH txtPath |]*)
        |])) in
        let rules = ConstraintLayout.(Constraint.([
            {input_item=tPath; input_loc={top=from_bot; left=rightOf lPath 10.; right=wRight ~-.10.; bottom=preferredH txtPath}};
            {input_item=lPath; 
             input_loc={top=centerTop lPath tPath; left=wLeft 10.; bottom=preferredH lblPath; right=preferredW lblPath}};
            {input_item=lCustom; input_loc={top=wTop 10.; left=wLeft 10.;
                    right=Mul [|wBottom 0.; Const 0.5|];
                    bottom=Add [|ITop (lCustom :> item); widthOf lCustom|]}};
            {input_item=lScroll; input_loc={top=Mul [|wBottom 0.; Const 0.5|]; left=wLeft 0.; right=wRight 0.; bottom=topOf tPath ~-.10.}};
        ])) in
        let layout = new ConstraintLayout.constraintLayout rules app#renderer in
        self#setLayout (layout :> ('a, 'b) Layout.layout);
        (*layout#renderObject#setZIndex 2;*)

    inherit ['a, 'b] Focusable.focusManager app [(txtPath :> ('a, 'b) HandlesEvent.handles_event);
                                                 (scroll :> ('a, 'b) HandlesEvent.handles_event);
                                                 (hexObj :> ('a, 'b) HandlesEvent.handles_event)]
end
