type item = Mixins.layoutable

type anchor = Left
            | Top
            | Bottom
            | Right

type loc = (* Relative to the window/layout rect top/bot/etc *)
         | WTop of float
         | WBottom of float
         | WLeft of float
         | WRight of float
         (* Relative to an item *)
         | ITop of item * float
         | IBottom of item * float
         | ILeft of item * float
         | IRight of item * float

type constraint_ = Anchors of anchor list
                 | Loc of loc * loc

type rule = For of item * constraint_ list

let iLeft i amt = ILeft (i, amt)
let iRight i amt = IRight (i, amt)
let iTop i amt = ITop (i, amt)
let iBot i amt = IBottom (i, amt)

type lang = rule list

class anchorLayout rules =
object(self)
    inherit Mixins.layout as super

    val id = 0
    val mutable eventHandlers = []
    
    method items = []
    method preferredSize = Size.zero
    method addLayoutable _ = ()
    method removeLayoutable _ = ()

    method layout rect =
        ()

    initializer
        ()
end
