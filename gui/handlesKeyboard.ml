class virtual handlesKeyboard =
object(self)
    val virtual events : 
        ([> `KeyDown | `KeyUp], 
         [> `KeyDownArg of Keys.key | `KeyUpArg of Keys.key]) 
        HandlesEvent.event_store

    method onKeyDown (key : Keys.key) = ()
    method onKeyUp (key : Keys.key) = ()

    initializer
        let fn = object 
            method call evt =
                match evt#arg with
                | `KeyDownArg key -> self#onKeyDown key
                | `KeyUpArg key -> self#onKeyUp key
                | _ -> ()
        end in
        events#addFn `KeyUp fn;
        events#addFn `KeyDown fn;
end

