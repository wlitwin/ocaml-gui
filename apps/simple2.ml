class ['a, 'b] simple2 app =
    let txt1 = new TextBox.textBoxWidget app in
    let txt2 = new TextBox.textBoxWidget app in
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    initializer
        let cl (i : ('a, 'b) #Layoutable.layoutable) = (i :> ('a, 'b) Layoutable.layoutable) in
        let lTxt1 = cl txt1 in
        let lTxt2 = cl txt2 in
        let open ConstraintLayout in
        let open ConstraintLayout.Constraint in
        let rules = [
            {input_item=lTxt1;
             input_loc={
                top = wTop 10.;
                bottom = preferredH lTxt1;
                right = wRight ~-.10.;
                left = wLeft 10.;
            }};
            {input_item=lTxt2;
             input_loc={
                top = bottomOf lTxt1 10.;
                bottom = preferredH lTxt2;
                right = wRight ~-.10.;
                left = wLeft 10.;
            }};

        ] in
        let layout = new ConstraintLayout.constraintLayout rules app#renderer in
        self#setLayout (layout :> ('a, 'b) Layout.layout)

    inherit ['a, 'b] Focusable.focusManager app [(txt1 :> ('a, 'b) HandlesEvent.handles_event);
                                                 (txt2 :> ('a, 'b) HandlesEvent.handles_event);]
end
