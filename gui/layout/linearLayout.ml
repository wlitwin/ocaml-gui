class virtual ['a, 'b] linearLayout renderer =
object(self)
    inherit ['a, 'b] Layout.layout
    val id = 0
    val items : ('a, 'b) Layoutable.layoutable DynArray.t = DynArray.make 1
    val table = Hashtbl.Poly.create()
    val rev_table = Hashtbl.Poly.create()
    val mutable eventHandlers = []
    val mutable snoopers = []
    val mutable distributeEvenly = true
    val renderObject = renderer#createGroupObject

    val events = HandlesEvent.create()

    method clear = DynArray.clear items
    method setDistributeEvenly b = distributeEvenly <- b

    method addLayoutable l = 
        DynArray.add items (l :> ('a, 'b) Layoutable.layoutable);
        renderObject#attach l#renderObject

    method removeLayoutable id =
        DynArray.filter (fun l -> 
            if l#id <> id then true
            else (
                renderObject#detach l#renderObject;
                false
            )
        ) items;

    method items = items 

    method private virtual ratioLayout : Rect.t -> unit
    method private virtual preferredLayout : Rect.t -> unit

    method layout rect =
        if distributeEvenly then self#ratioLayout rect
        else self#preferredLayout rect

    initializer
        renderObject#setZIndex 1;
end

