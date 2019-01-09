class virtual linearLayout =
object(self)
    inherit Layout.layout
    val id = 0
    val items : Layoutable.layoutable DynArray.t = DynArray.make 1
    val table = Hashtbl.Poly.create()
    val rev_table = Hashtbl.Poly.create()
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable distributeEvenly = true

    (*val events : ([>`Unit], [>`UnitArg]) HandlesEvent.handles*)
    val events = HandlesEvent.create()

    method clear = DynArray.clear items
    method setDistributeEvenly b = distributeEvenly <- b

    method addLayoutable l = DynArray.add items (l :> Layoutable.layoutable)

    method removeLayoutable id =
        DynArray.filter (fun l -> l#id <> id) items

    method items = items 

    method private virtual ratioLayout : Rect.t -> unit
    method private virtual preferredLayout : Rect.t -> unit

    method layout rect =
        if distributeEvenly then self#ratioLayout rect
        else self#preferredLayout rect
end

