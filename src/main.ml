class main app = 
    let tbox1 = new TextBox.textBoxWidget app in
    let tbox2 = new TextBox.textBoxWidget app in
    let tbox3 = new TextBox.textBoxWidget app in
    let tbox4 = new TextBox.textBoxWidget app in
    let lbl1  = new Label.label ~text:"Awesome!" app in
    let lbl2  = new Label.label ~text:"Another label!" app in

object(self)
    inherit Widget.basicWidget app as super

    val v_layout = new LayoutModule.verticalLayout
    val h_layout_1 = new LayoutModule.horizontalLayout
    val h_layout_2 = new LayoutModule.horizontalLayout

    method resize rect =
        super#resize rect;
        v_layout#layout rect;

    method! onDraw cr =
        tbox1#onDraw cr;
        tbox2#onDraw cr;
        tbox3#onDraw cr;
        tbox4#onDraw cr;
        lbl1#onDraw cr;
        lbl2#onDraw cr;

    initializer
        h_layout_1#addLayoutable (tbox1 :> Mixins.layoutable);
        h_layout_1#addLayoutable (tbox2 :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox3 :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox4 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_1 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_2 :> Mixins.layoutable);
        v_layout#addLayoutable (lbl1 :> Mixins.layoutable);
        v_layout#addLayoutable (lbl2 :> Mixins.layoutable);

    inherit Mixins.focusManager app [(tbox1 :> Mixins.handlesEvent);
                                     (tbox2 :> Mixins.handlesEvent);
                                     (tbox4 :> Mixins.handlesEvent);
                                     (tbox3 :> Mixins.handlesEvent)]

end

let _ =
    let app = new NewApplication.application Rect.{w=400.; h=400.} in
    let mainWidget = new main app in
    app#setWidget (mainWidget :> Widget.basicWidget);
    app#main
