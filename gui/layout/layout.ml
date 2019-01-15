
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
        super#onResize r;
        self#layout r;
end

