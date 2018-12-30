class simple app =
object(self)
    inherit Widget.basicWidget app as super

    initializer
        shouldClip <- false;
        let cl (i : #Mixins.layoutable) = (i :> Mixins.layoutable) in
        let widget1 = new Widget.basicWidget app in
        let widget2 = new Widget.basicWidget app in
        let widget3 = new Widget.basicWidget app in
        let widget4 = new Widget.basicWidget app in
        let lWidget1 = cl widget1 in
        let lWidget2 = cl widget2 in
        let lWidget3 = cl widget3 in
        let lWidget4 = cl widget4 in
        let open ConstraintLayout in
        let open ConstraintLayout.Constraint in
        let rules = [
            {item=lWidget1;
             loc={
                 top=wTop 10.;
                 left=wLeft 10.;
                 right=leftOf lWidget2 ~-.10.;
                 bottom=Mul [WBottom; Const 0.3];
            }};
            {item=lWidget2;
             loc={
                 top=wTop 10.;
                 left=Mul [WRight; Const 0.3];
                 right=wRight ~-.10.;
                 bottom=Mul [WBottom; Const 0.6];
            }};
            {item=lWidget3;
             loc={
                 top=bottomOf lWidget1 10.;
                 left=wLeft 10.;
                 right=rightOf lWidget1 0.;
                 bottom=wBottom ~-.10.
            }};
            {item=lWidget4;
             loc={
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
        self#setLayout (layout :> Mixins.layout)
end
