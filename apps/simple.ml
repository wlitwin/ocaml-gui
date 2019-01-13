class ['a, 'b] tree_drawing app = 
object
    inherit ['a, 'b] Widget.basicWidget app as super

    val mutable tree = Rendering.{
        parent=None;
        content=Group(0, [Rendering.fill_rect Rect.{x=10.;y=10.;w=10.;h=10.} Color.green]);
        children=[]
    }

    method! onResize r =
        (*super#resize r;*)
        tree <- Rendering.{
            parent=None;
            content=Group(0, [
                Rendering.fill_rect r Color.green
            ]);
            children=[{
                parent=None;    
                content=Group(1, [
                    Rendering.fill_text "Hello World!" Font.default_font Color.black Pos.{x=r.x; y=r.y+.20.} 
                ]);
                children=[];
            }];
        }

    method! onDraw cr =
       tree
       |> Rendering.sort_tree
       |> Rendering.draw_tree cr
end

class ['a, 'b] simple app =
    let label1 = new TextBox.textBoxWidget app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        let cl (i : ('a, 'b) #Layoutable.layoutable) = (i :> ('a, 'b) Layoutable.layoutable) in
        let widget1 = new Widget.basicWidget app in
        let widget2 = new Widget.basicWidget app in
        let widget3 = (*new Widget.basicWidget app in*) new tree_drawing app in
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
                 bottom=Mul [WBottom; Const 0.3];
            }};
            {input_item=lWidget2;
             input_loc={
                 top=bottomOf lbl 10.;
                 left=Mul [WRight; Const 0.3];
                 right=wRight ~-.10.;
                 bottom=Mul [WBottom; Const 0.6];
            }};
            {input_item=lWidget3;
             input_loc={
                 top=bottomOf lWidget1 10.;
                 left=wLeft 10.;
                 right=rightOf lWidget1 0.;
                 bottom=wBottom ~-.10.
            }};
            {input_item=lWidget4;
             input_loc={
                 top=bottomOf lWidget2 10.;
                 left=leftOf lWidget2 0.;
                 right=wRight ~-.10.;
                 bottom=wBottom ~-.10.
            }};
        ] in
        let layout = new ConstraintLayout.constraintLayout rules in
        widget1#style#setBGColor Color.red;
        widget2#style#setBGColor Color.yellow;
        widget3#style#setBGColor Color.green;
        widget4#style#setBGColor Color.blue;
        self#setLayout (layout :> ('a, 'b) Layout.layout)

    inherit ['a, 'b] Focusable.focusManager app [(label1 :> ('a, 'b) HandlesEvent.handles_event)]
end
