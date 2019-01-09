
(*type 'a layoutSubclass = 'a constraint 'a = #Layoutable.layoutable*)

class virtual ['a, 'b] layout =
object(self)
    inherit ['a, 'b] Layoutable.layoutable as super
    val mutable rect = Rect.empty

   (* method virtual addLayoutable : 'a. 'a layoutSubclass -> unit*)
    method virtual addLayoutable : ('a, 'b) Layoutable.layoutable -> unit
    method virtual removeLayoutable : int -> unit
    method virtual layout : Rect.t -> unit
    method virtual items : ('a, 'b) Layoutable.layoutable DynArray.t

    method! onResize r =
        let res = super#onResize r in
        self#layout r;
        res 

    initializer
        let open Drawable in
        let fn evt = 
            match evt#arg with
            | `PaintArg cr ->
                DynArray.iter (fun item ->
                    item#events#handle evt
                ) self#items
            | _ -> ()
        in
        events#addFn `Paint (object method call = fn end)
end

