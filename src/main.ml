module P = Platform

let _ =
    let app = new Application.application Rect.{w=400.; h=400.} in
    (*let mainWidget = new Login.loginWidget app in*)
    (*let mainWidget = new Demo.main app in*)
    (*let mainWidget = new FileBrowser.fileBrowser app in*)
    let mainWidget = new Simple.simple app in
    app#setWidget (mainWidget :> Widget.basicWidget);
    app#main
