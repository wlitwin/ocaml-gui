open HexGrid 

class ['a, 'b] game app =
    let hexGrid = new hexGrid app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        hexGrid#setDims (16, 10);
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

(* Maybe can make the UI simpler ? 
 *
 * Make the dependencies a little more explicit?
 *
 * There's only a couple things that are actually needed in a UI:
 *    - Input
 *    - Layout
 *    - Rendering
 *
 * Layout doesn't really care about rendering. As long as what's rendered
 * can be described. For example border sizes.
 *
 * Input is basically keyboard up/down, mouse up/down/move
 *
 * Rendering just needs to be told to draw to the screen and it can handle
 * figuring out how to draw whatever changed. Just needs shapes and positions.
 *
 * From the outside world the following can happen:
 *    - Resize of viewport, layout + rendering care about this
 *    - Some kind of input event (maybe event network?)
 *    - Timer ?
 *    - Some other event?
 *
 * But effectively the application is static until some input prods it.
 *)
