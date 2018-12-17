
class main app = 
    let tbox = new TextBoxWidget.textBoxWidget app in
    let tbox2 = new TextBoxWidget.textBoxWidget app in

object(self)
    inherit Widget.basicWidget app as super

    val layout = new LayoutModule.verticalLayout

    method! onKeyDown k =
        Stdio.printf "Key down\n";
        self#focused#postEvent (Mixins.KeyDown k)

    method resize rect =
        Stdio.printf "Main application resized\n%!";
        super#resize rect;
        layout#layout rect

    method! onDraw cr =
        tbox#onDraw cr;
        tbox2#onDraw cr;

    initializer
        layout#addLayoutable (tbox :> Mixins.layoutable);
        layout#addLayoutable (tbox2 :> Mixins.layoutable);

    inherit Mixins.focusManager app [(tbox :> Mixins.handlesEvent);
                                     (tbox2 :> Mixins.handlesEvent)]

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
