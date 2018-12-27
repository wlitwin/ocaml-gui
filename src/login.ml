class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in
object(self)
    inherit Widget.basicWidget app as super

    initializer
        let open AnchorLayout in
        let lName = (name :> Mixins.layoutable) in
        let lPass = (password :> Mixins.layoutable) in
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
        ] in
        self#setLayout (new anchorLayout rules :> Mixins.layout)

    inherit Mixins.focusManager app [(name :> Mixins.handlesEvent);
                                     (password :> Mixins.handlesEvent)]
end
