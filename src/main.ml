module R = Rendering

let coerce (v : ('a, 'b) #Widget.basicWidget) =
    (v :> ('a, 'b) Widget.basicWidget)
;;

(*
let _ =
    Platform.Windowing.run (fun context ->
        let app = new Application.application context Rect.{w=400.; h=400.} in
        (*let mainWidget = new Login.loginWidget app in*)
        (*let mainWidget = new Demo.main app in*)
        let mainWidget = new FileBrowser.fileBrowser app in
        let _ = new Simple.simple app in
        app#setWidget (coerce mainWidget);
        app#main
    )
    *)
