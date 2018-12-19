class main app = 
    let tbox1 = new TextBox.textBoxWidget app in
    let tbox2 = new TextBox.textBoxWidget app in
    let tbox3 = new TextBox.textBoxWidget app in
    let tbox4 = new TextBox.textBoxWidget app in
    let lbl1  = new Label.label ~text:"Awesome!" app in
    let lbl2  = new Label.label ~text:"Another label with really really really really long text!" app in
    let scroller = new Scroll.scrollArea app (lbl2 :> Widget.basicWidget) in
    let dd    = DropDown.(new dropDown app (Simple ["Option 1"; "Option 2"; "Option 3"; "Option 4"])) in
    let img   = new Image.image app in
    let img_data = ByteArray.ByteArray.of_array [|
        0; 0; 255; 255; (**) 0; 255; 0; 255;
        255; 0; 0; 255; (**) 0; 255; 255; 255;
    |] in
    let _ = img#setImage img_data 2 2; img#setScale 10. in
object(self)
    inherit Widget.basicWidget app as super

    val v_layout = new Layout.verticalLayout
    val h_layout_1 = new Layout.horizontalLayout
    val h_layout_2 = new Layout.horizontalLayout
    val h_layout_3 = new Layout.horizontalLayout
    val h_layout_4 = new Layout.horizontalLayout

    method resize rect =
        super#resize rect;
        v_layout#layout rect;

    method! onDraw cr =
        tbox1#onDraw cr;
        tbox2#onDraw cr;
        tbox3#onDraw cr;
        tbox4#onDraw cr;
        lbl1#onDraw cr;
        scroller#onDraw cr;
        dd#onDraw cr;
        img#onDraw cr;

    initializer
        h_layout_1#addLayoutable (dd  :> Mixins.layoutable);
        h_layout_1#addLayoutable (img :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox1 :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox2 :> Mixins.layoutable);
        h_layout_3#addLayoutable (tbox3 :> Mixins.layoutable);
        h_layout_3#addLayoutable (tbox4 :> Mixins.layoutable);
        h_layout_4#addLayoutable (lbl1 :> Mixins.layoutable);
        h_layout_4#addLayoutable (scroller :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_1 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_2 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_3 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_4 :> Mixins.layoutable);

    inherit Mixins.focusManager app [(dd :> Mixins.handlesEvent); 
                                     (tbox1 :> Mixins.handlesEvent);
                                     (tbox2 :> Mixins.handlesEvent);
                                     (tbox4 :> Mixins.handlesEvent);
                                     (tbox3 :> Mixins.handlesEvent);
                                     (scroller :> Mixins.handlesEvent)]

end

let _ =
    let app = new Application.application Rect.{w=400.; h=400.} in
    let mainWidget = new main app in
    app#setWidget (mainWidget :> Widget.basicWidget);
    app#main
