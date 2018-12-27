class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in

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
        let rules = [
            { item=lName;
              loc={
                  top=WTop 10.;
                  left=WLeft 10.;
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
            { item=lPass;
              loc={
                  top=IBottom (lName, 10.);
                  left=WLeft 10.;
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
            { item=lblName;
              loc={
                  top=IBottom (lPass, 10.);
                  left=WLeft 10.;
                  right=WLeft 70.;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
            { item=lblPass;
              loc={
                  top=IBottom (lblName, 10.);
                  left=WLeft 10.;
                  right=PreferredW;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
            { item=txtName;
              loc={
                  top=ITop (lblName, 0.);
                  left=IRight (lblPass, 10.);
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
            { item=txtPass;
              loc={
                  top=IBottom (txtName, 10.);
                  left=ILeft (txtName, 0.);
                  right=WRight ~-.10.;
                  bottom = PreferredH;
              };
              anchors={left=true; right=true; top=true; bottom=false}
            };
        ] in
        self#setLayout (new anchorLayout rules :> Mixins.layout)

    inherit Mixins.focusManager app [(name :> Mixins.handlesEvent);
                                     (password :> Mixins.handlesEvent);
                                     (txtName :> Mixins.handlesEvent);
                                     (txtPass :> Mixins.handlesEvent)]
end
