module PWG = Platform.Windowing.Graphics

class virtual drawable =
object(self)
    val virtual events : ([>`Paint], [>`PaintArg of PWG.context]) HandlesEvent.event_store

    method virtual onDraw : Platform.Windowing.Graphics.context -> unit 

    initializer
        let fn = function
            | `PaintArg cr -> self#onDraw cr
            | _ -> ()
        in
        events#addFn `Paint (object
            method call evt = fn evt#arg
        end)
end
