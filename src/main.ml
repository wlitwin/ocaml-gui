let _ =
    let app = new Application.application Rect.{w=400.; h=400.} in
    let mainWidget = new Login.loginWidget app in
    app#setWidget (mainWidget :> Widget.basicWidget);
    app#main
