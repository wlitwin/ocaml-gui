class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in
    let grid = new Widget.basicWidget app in

    let lblName = new Label.label app ~text:"Name" in
    let lblPass = new Label.label app ~text:"Password" in
    let txtName = new TextBox.textBoxWidget app in
    let txtPass = new TextBox.textBoxWidget app in
object(self)
    inherit Widget.basicWidget app as super

    initializer
        let open AnchorLayout in
        let lName = (name :> Mixins.layoutable) in
        let lPass = (password :> Mixins.layoutable) in
        let lblName = (lblName :> Mixins.layoutable) in
        let lblPass = (lblPass :> Mixins.layoutable) in
        let txtName = (txtName :> Mixins.layoutable) in
        let txtPass = (txtPass :> Mixins.layoutable) in
        let lGrid = (grid :> Mixins.layoutable) in
        let rules = [
            { item=lName;
              loc={
                  top=WTop 10.;
                  left=WLeft 10.;
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
            };
            { item=lPass;
              loc={
                  top=IBottom (lName, 10.);
                  left=WLeft 10.;
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
            };
            { item=lblName;
              loc={
                  top=IBottom (lPass, 10.);
                  left=WLeft 10.;
                  right=IRight (lblPass, 0.);
                  bottom = PreferredH;
              };
            };
            { item=lblPass;
              loc={
                  top=IBottom (lblName, 10.);
                  left=WLeft 10.;
                  right=PreferredW;
                  bottom = PreferredH;
              };
            };
            { item=txtName;
              loc={
                  top=ITop (lblName, 0.);
                  left=IRight (lblName, 10.);
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
            };
            { item=txtPass;
              loc={
                  top=IBottom (txtName, 10.);
                  left=ILeft (txtName, 0.);
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
            };
            {
                item=lGrid;
                loc={
                    top=IBottom (txtPass, 10.);
                    left=ILeft (lblPass, 0.);
                    right=IRight (txtPass, 0.);
                    bottom=WBottom ~-.10.;
                };
            };
        ] in
        let lgrid = new Layout.gridLayout 3 3 in
        let item1 = new Widget.basicWidget app in
        let item2 = new Widget.basicWidget app in
        let item3 = new Widget.basicWidget app in
        let item4 = new Widget.basicWidget app in
        let item5 = new Widget.basicWidget app in
        let item6 = new Widget.basicWidget app in
        let item7 = new Widget.basicWidget app in
        let item8 = new Widget.basicWidget app in
        let item9 = new Widget.basicWidget app in
        item1#style#setBGColor Color.red;
        item2#style#setBGColor Color.orange;
        item3#style#setBGColor Color.yellow;
        item4#style#setBGColor Color.green;
        item5#style#setBGColor Color.blue;
        item6#style#setBGColor Color.white;
        item7#style#setBGColor Color.red;
        item8#style#setBGColor Color.orange;
        item9#style#setBGColor Color.yellow;
        lgrid#addToCell 0 0 (item1 :> Mixins.layoutable);
        lgrid#addToCell 0 1 (item2 :> Mixins.layoutable);
        lgrid#addToCell 0 2 (item3 :> Mixins.layoutable);
        lgrid#addToCell 1 0 (item4 :> Mixins.layoutable);
        lgrid#addToCell 1 1 (item5 :> Mixins.layoutable);
        lgrid#addToCell 1 2 (item6 :> Mixins.layoutable);
        lgrid#addToCell 2 0 (item7 :> Mixins.layoutable);
        lgrid#addToCell 2 1 (item8 :> Mixins.layoutable);
        lgrid#addToCell 2 2 (item9 :> Mixins.layoutable);
        grid#setLayout (lgrid :> Mixins.layout);
        grid#style#setBGColor Color.red;
        self#setLayout (new anchorLayout rules :> Mixins.layout);

    inherit Mixins.focusManager app [(name :> Mixins.handlesEvent);
                                     (password :> Mixins.handlesEvent);
                                     (txtName :> Mixins.handlesEvent);
                                     (txtPass :> Mixins.handlesEvent)]
end
