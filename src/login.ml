class loginWidget app = 
    let name = new LabeledTextBox.labeledTextBox app "Name" in
    let password = new LabeledTextBox.labeledTextBox app "Password" in
    let grid = new Widget.basicWidget app in

    let lblName = new Label.label app ~text:"Name" in
    let lblPass = new Label.label app ~text:"Password" in
    let txtName = new TextBox.textBoxWidget app in
    let txtPass = new TextBox.textBoxWidget app in
    let img = new Image.image app in
    let img_data = ByteArray.ByteArray.of_array [|
        0; 0; 255; 255; (**) 0; 255; 0; 255;
        255; 0; 0; 255; (**) 0; 255; 255; 255;
    |] in
    let _ = img#setImage img_data 2 2; img#setScale 10.; img#setKeepAspectRatio false in
    let _ = img#style#borderStyle#setStyle BorderStyle.Rectangle in
    let _ = img#style#borderStyle#setSize 10. in
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
        let lImg = (img :> Mixins.layoutable) in
        let top amt = Add [WTop; Const amt] in
        let left amt = Add [WLeft; Const amt] in
        let right amt = Add [WRight; Const amt] in
        let bottom amt = Add [WBottom; Const amt] in
        let topOf item amt = Add [ITop item; Const amt] in
        let leftOf item amt = Add [ILeft item; Const amt] in
        let rightOf item amt = Add [IRight item; Const amt] in
        let bottomOf item amt = Add [IBottom item; Const amt] in
        let rules = [
            { item=lName;
              loc={
                  top=top 10.;
                  left=left 10.;
                  right=right ~-.10.;
                  bottom = PreferredH lName;
              };
            };
            { item=lPass;
              loc={
                  top=bottomOf lName 10.;
                  left=left 10.;
                  right=right ~-.10.;
                  bottom = PreferredH lPass;
              };
            };
            { item=lblName;
              loc={
                  top=bottomOf lPass 10.;
                  left=left 10.;
                  right=IRight lblPass;
                  bottom=IBottom txtName;
              };
            };
            { item=lblPass;
              loc={
                  top=ITop txtPass;
                  left=left 10.;
                  right=PreferredW lblPass;
                  bottom=IBottom txtPass;
              };
            };
            { item=txtName;
              loc={
                  top=ITop lblName;
                  left=rightOf lblName 10.;
                  right=right ~-.10.;
                  bottom=PreferredH txtName;
              };
            };
            { item=txtPass;
              loc={
                  top=bottomOf txtName 10.;
                  left=ILeft txtName;
                  right=right ~-.10.;
                  bottom=PreferredH txtPass;
              };
            };
            {
                item=lGrid;
                loc={
                    top=bottomOf txtPass 10.;
                    left=ILeft lblPass;
                    right=ILeft lImg;
                    bottom=bottom ~-.10.;
                };
            };
            {
                item=lImg;
                loc={
                    top=ITop lGrid;
                    left=Add [ILeft txtPass; Mul [Const 0.5; Sub [IRight txtPass; ILeft txtPass]]];
                    right=right ~-.10.;
                    bottom=Add [ITop lGrid; Mul [Const 0.5; Sub [IBottom lGrid; ITop lGrid]]];
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
