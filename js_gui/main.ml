open Js_of_ocaml

let _ =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        let app = new Application.application Size.{w=400.; h=400.} in
        let mainWidget = new Simple.simple app in
        app#setWidget (mainWidget :> Widget.basicWidget);
        app#main;
        Js._false
    )
