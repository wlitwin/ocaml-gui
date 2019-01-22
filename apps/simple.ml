let _ = Random.init 102493

class ['a, 'b] tree_drawing app = 
object
    inherit ['a, 'b] Widget.basicWidget app as super

    method! onResize r =
        let mk_text cnt =
            let s = [|"H"; "e"; "l"; "l"; "o"; " "; "W"; "o"; "r"; "l"; "d"; "!"|] in
            let rec loop cnt acc =
                if cnt < 0 then acc
                else (
                    let rx = Random.float (r.w*.0.85) in
                    let ry = Random.float (r.h*.0.85) in
                    let str = s.(Random.int 12) ^ s.(Random.int 12) ^ s.(Random.int 12) ^ s.(Random.int 12) in
                    loop (cnt-1) (Rendering.fill_text str Font.default_font Color.black Pos.{x=r.x+.rx; y=r.y+.ry} :: acc)
                )
            in
            loop cnt []
        in
        renderObject#setContent (Group [
            (1, [Rendering.fill_rect r Color.green;
                 Rendering.stroke_rect r Color.black; 
                ]);
            (2, [Rendering.fill_text "Hello World!" Font.default_font Color.black Pos.{x=r.x; y=r.y+.20.};
                 Rendering.fill_rect Rect.{x=r.x; y=r.y+.r.h*.0.5; w=r.w; h=r.h*.0.5} Color.orange] @ mk_text 100);
        ])

    initializer
        renderObject#detach (bgRect :> Rendering.nodeObject)
end

class ['a, 'b] simple app =
    let label1 = new TextBox.textBoxWidget app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        let cl (i : ('a, 'b) #Layoutable.layoutable) = (i :> ('a, 'b) Layoutable.layoutable) in
        let widget1 = new Widget.basicWidget app in
        let widget2 = new Widget.basicWidget app in
        let widget3 = new Widget.basicWidget app in (*new tree_drawing app in *)
        let widget4 = new Widget.basicWidget app in
        let lWidget1 = cl widget1 in
        let lWidget2 = cl widget2 in
        let lWidget3 = cl widget3 in
        let lWidget4 = cl widget4 in
        let lbl = cl label1 in
        let open ConstraintLayout in
        let open ConstraintLayout.Constraint in
        let rules = [
            {input_item=lbl;
             input_loc={top=wTop 10.; left=wLeft 10.; right=wRight ~-.10.; bottom=preferredH lbl}};
            {input_item=lWidget1;
             input_loc={
                 top=bottomOf lbl 10.;
                 left=wLeft 10.;
                 right=leftOf lWidget2 ~-.10.;
                 bottom=Mul [|WBottom; Const 0.3|];
            }};
            {input_item=lWidget2;
             input_loc={
                 top=bottomOf lbl 10.;
                 left=Mul [|WRight; Const 0.3|];
                 right=wRight ~-.10.;
                 bottom=Mul [|WBottom; Const 0.6|];
            }};
            {input_item=lWidget3;
             input_loc={
                 top=bottomOf lWidget1 10.;
                 left=wLeft 10.;
                 right=rightOf lWidget1 0.;
                 bottom=wBottom ~-.20.
            }};
            {input_item=lWidget4;
             input_loc={
                 top=bottomOf lWidget2 10.;
                 left=leftOf lWidget2 0.;
                 right=wRight ~-.10.;
                 bottom=wBottom ~-.20.
            }};
        ] in
        let layout = new ConstraintLayout.constraintLayout rules app#renderer in
        widget1#setBGColor Color.red;
        widget2#setBGColor Color.yellow;
        widget3#setBGColor Color.green;
        widget4#setBGColor Color.blue;
        self#setLayout (layout :> ('a, 'b) Layout.layout)

    inherit ['a, 'b] Focusable.focusManager app [(label1 :> ('a, 'b) HandlesEvent.handles_event)]
end
