open Rect
open Control

type space_behavior = Preferred
                    | Equal

type info = {
    mutable size : size;
    sb : space_behavior;
    control : control;
}

class hboxLayout parent = object(self)
    inherit layout parent

    val children : info DynArray.t = DynArray.create()

    method addControl control =
        self#addControlWith control Equal

    method addControlWith control sb =
        DynArray.add children {control; sb; size = {w=0.; h=0.}}

    method children = DynArray.map (fun i -> i.control) children

    method removeAllChildren =
        DynArray.clear children

    method sizeHint cr =
        let w, h =
            DynArray.fold_left (fun (w, h) c ->
                let sz = c.control#sizeHint cr in
                Float.(sz.w +. w, max sz.h h)
            ) (0., 0.) children
        in
        {w; h}

    method private len = DynArray.length children

    method private equalCount =
        DynArray.fold_left (fun cnt info ->
            if Poly.(=) info.sb Equal then cnt + 1 else cnt
        ) 0 children

    method private firstPass (cr : Cairo.context) : float =
        let eql_count = self#equalCount in
        let psize = children
            |> DynArray.copy
            |> Util.(%) Util.tee DynArray.filter (fun info -> Poly.(=) info.sb Preferred)
            |> DynArray.fold_left (fun width info ->
                let ctrlSize = info.control#sizeHint cr in
                width +. ctrlSize.w
            ) 0. in
        (* Output how much goes to the preferred vs the equals *)
        (parent#geom.w -. psize) /. (Float.of_int eql_count)

    method layout cr =
        let rect : rect = parent#geom in
        let eqlSize = self#firstPass cr in
        DynArray.fold_left (fun (x, idx) info ->
            let w =
                match info.sb with
                | Preferred -> (info.control#sizeHint cr).w
                | Equal -> eqlSize
            in
            let r = { x;
                      y=rect.y;
                      w;
                      h=rect.h; }
            in
            info.control#setGeometry r;
            info.control#relayout cr;
            x +. r.w, idx+1
        ) (rect.x, 0) children |> ignore
end

class vboxLayout (parent : control) = object(self)
    inherit layout parent

    val children : info DynArray.t = DynArray.create()

    method addControl control =
        self#addControlWith control Equal

    method addControlWith control sb =
        DynArray.add children {control; sb; size = {w=0.; h=0.}}

    method children = DynArray.map (fun i -> i.control) children

    method removeAllChildren =
        DynArray.clear children

    method sizeHint cr =
        let w, h =
            DynArray.fold_left (fun (w, h) c ->
                let sz = c.control#sizeHint cr in
                Float.(max sz.w w, sz.h +. h)
            ) (0., 0.) children
        in
        {w; h}

    method private len = DynArray.length children

    method private equalCount =
        DynArray.fold_left (fun cnt info ->
            if Poly.(=) info.sb Equal then cnt + 1 else cnt
        ) 0 children

    method private firstPass (cr : Cairo.context) : float =
        let eql_count = self#equalCount in
        let psize = children
            |> DynArray.copy
            |> Util.(%) Util.tee DynArray.filter (fun info -> Poly.(=) info.sb Preferred)
            |> DynArray.fold_left (fun height info ->
                let ctrlSize = info.control#sizeHint cr in
                height +. ctrlSize.h
            ) 0. in
        (* Output how much goes to the preferred vs the equals *)
        (parent#geom.h -. psize) /. (Float.of_int eql_count)

    method layout cr =
        let rect : rect = parent#geom in
        let eqlSize = self#firstPass cr in
        DynArray.fold_left (fun (y, idx) info ->
            let h =
                match info.sb with
                | Preferred -> (info.control#sizeHint cr).h
                | Equal -> eqlSize
            in
            let r = { x=rect.x;
                      y;
                      w=rect.w;
                      h; }
            in
            info.control#setGeometry r;
            info.control#relayout cr;
            y +. r.h, idx+1
        ) (rect.y, 0) children |> ignore
end

class stackLayout (parent : control) = object(self)
    inherit layout parent as super

    val items : control DynArray.t = DynArray.create()
    val mutable curIndex : int = 0

    method private len = DynArray.length items

    method count = self#len

    method children = items

    method addControl control =
        DynArray.add items control

    method removeControl ctrl =
        DynArray.filter (fun c -> not (phys_equal c ctrl)) items;
        self#setIndex curIndex;

    method removeAt idx =
        DynArray.delete items idx;
        self#setIndex curIndex;

    method removeLast =
        DynArray.delete_last items;
        self#setIndex curIndex

    method setIndex idx =
        curIndex <- Util.clampi idx 0 (self#len - 1)

    method curIndex = curIndex

    method currentControl = 
        DynArray.get items curIndex

    method sizeHint cr =
        self#currentControl#sizeHint cr

    method layout cr =
        self#currentControl#setGeometry parent#geom;
        self#currentControl#relayout cr
end
