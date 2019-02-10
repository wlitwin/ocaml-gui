open HexGrid 

class ['a, 'b] game app =
    let hexGrid = new hexGrid app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        hexGrid#setDims (10, 10);
        let cl = fun w -> (w :> ('a, 'b) Layoutable.layoutable) in
        let lGrid = cl hexGrid in
        let open ConstraintLayout in
        let open Constraint in
        let rules = [
            {input_item=lGrid; input_loc={
                top=Const 0.;
                left=Const 0.;
                right=Min [|preferredW lGrid; wRight 0.|];
                bottom=Min [|preferredH lGrid; wBottom 0.|];
            }}
        ] in
        let layout = new ConstraintLayout.constraintLayout rules app#renderer in
        self#setLayout (layout :> ('a, 'b) Layout.layout);

    inherit ['a, 'b] Focusable.focusManager app [(hexGrid :> ('a, 'b) HandlesEvent.handles_event)]
end
