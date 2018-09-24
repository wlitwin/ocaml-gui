let _ =
    let app = new Qt.application in
    let text = new TextBox.textBox app in
    app#openWindow Rect.{x=0.; y=0.; w=400.; h=400.}
                   (text :> Qt.control)
                   Qt.no_info |> ignore;
    app#main
