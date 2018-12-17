
class main app = 
    let tbox = new TextBoxWidget.textBoxWidget app in
    let tbox2 = new TextBoxWidget.textBoxWidget app in
    let tbox3 = new TextBoxWidget.textBoxWidget app in
    let tbox4 = new TextBoxWidget.textBoxWidget app in

object(self)
    inherit Widget.basicWidget app as super

    val v_layout = new LayoutModule.verticalLayout
    val h_layout_1 = new LayoutModule.horizontalLayout
    val h_layout_2 = new LayoutModule.horizontalLayout

    method! onKeyDown k =
        self#focused#postEvent (Mixins.KeyDown k)

    method resize rect =
        super#resize rect;
        v_layout#layout rect;

    method! onDraw cr =
        tbox#onDraw cr;
        tbox2#onDraw cr;
        tbox3#onDraw cr;
        tbox4#onDraw cr;

    initializer
        h_layout_1#addLayoutable (tbox  :> Mixins.layoutable);
        h_layout_1#addLayoutable (tbox2 :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox3 :> Mixins.layoutable);
        h_layout_2#addLayoutable (tbox4 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_1 :> Mixins.layoutable);
        v_layout#addLayoutable (h_layout_2 :> Mixins.layoutable);

    inherit Mixins.focusManager app [(tbox :> Mixins.handlesEvent);
                                     (tbox2 :> Mixins.handlesEvent);
                                     (tbox4 :> Mixins.handlesEvent);
                                     (tbox3 :> Mixins.handlesEvent)]

end


let _ =
(*    let app = new Qt.application in
    let text = new TextBox.textBox app in
    app#openWindow Rect.{x=0.; y=0.; w=400.; h=400.}
                   (text :> Qt.control)
                   Qt.no_info |> ignore;
    app#main
    *)
    let app = new NewApplication.application Rect.{w=400.; h=400.} in
    let mainWidget = new main app in
    app#setWidget (mainWidget :> Widget.basicWidget);
    app#main
