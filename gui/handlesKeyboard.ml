class virtual handlesKeyboard =
object(self)
    val virtual events : 
        ([> `KeyDown | `KeyUp], 
         [> `KeyDownArg of Keys.key | `KeyUpArg of Keys.key]) 
        HandlesEvent.event_store

    method onKeyDown (key : Keys.key) = ()
    method onKeyUp (key : Keys.key) = ()

    initializer
        let fn = function
            | `KeyDownArg key -> self#onKeyDown key
            | `KeyUpArg key -> self#onKeyUp key
            | _ -> ()
        in
        events#addFn `KeyUp (object
            method call evt = fn evt#arg
        end)
end

