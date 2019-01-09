class fullLayout (item : Mixins.layoutable) =
object(self)
    inherit layout
    val id = 0
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable items = DynArray.of_list [item]

    method item = DynArray.get items 0

    method items = items

    method removeLayoutable _ =
        items <- DynArray.create()

    method preferredSize =
        self#preferredSize

    method addLayoutable l =
        DynArray.set items 0 (l :> layoutable)

    method layout r =
        self#item#postEvent (Resize r) |> ignore

end

