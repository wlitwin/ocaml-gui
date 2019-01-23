module Graphics = Platform.Windowing.Graphics

type scrollBarType = VerticalScroller
                   | HorizontalScroller

class ['a, 'b] scrollBar app scrollType = object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val mutable pos = 0.
    val barSize = 10.
    val scrollType = scrollType
    val mutable ratio = 1.
    val scrollBar = app#renderer#createRectObject

    method setPosition (p : float) : unit =
        pos <- Util.clamp p 0. 1.

    method position = pos
    method barSize = barSize
    method ratio = ratio
    method setRatio new_ratio = ratio <- new_ratio

    method incrPos amt =
        self#setPosition (self#position +. amt);
        self#updateBar

    method decrPos amt =
        self#setPosition (self#position -. amt);
        self#updateBar

    method private updateBar =
        let open Float in
        match scrollType with
        | VerticalScroller ->
            let barHeight = min (ratio *. rect.h) rect.h in
            let y = rect.y +. pos *. (rect.h -. barHeight) in
            scrollBar#setRect Rect.{x=rect.x; y; w=barSize; h=barHeight};
        | HorizontalScroller -> 
            let barWidth  = min (ratio *. rect.w) rect.w in
            let x = rect.x +. pos *. (rect.w -. barWidth) in
            scrollBar#setRect Rect.{x; y=rect.y; w=barWidth; h=barSize};

    method! onResize r =
        super#onResize r;
        self#updateBar

    initializer
        self#setBGColor Color.white;
        scrollBar#setColor Color.black;
        scrollBar#setZIndex 3;
        renderObject#setZIndex 2;
        renderObject#attach (scrollBar :> Rendering.nodeObject);
        self#updateBar;
end

class ['a, 'b] scrollArea app cont =
object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val mutable cont : ('a, 'b) Widget.basicWidget = cont
    val mutable horiz = true
    val mutable vert = true
    val vertScroller = new scrollBar app VerticalScroller
    val horzScroller = new scrollBar app HorizontalScroller
    val translation = app#renderer#createTranslateObject

    method setControl c : unit =
        translation#detach cont#renderObject;
        cont <- c;
        translation#attach cont#renderObject;

    method! onKeyDown k =
        let ratio, fn = 
            let open Application in
            match k with
            | Keys.A when app#specialKeys.ctrlDown -> 0.0, vertScroller#setPosition
            | Keys.E when app#specialKeys.ctrlDown -> 1.0, vertScroller#setPosition
            | Keys.J when app#specialKeys.ctrlDown -> vertScroller#ratio*.0.5, vertScroller#incrPos
            | Keys.K when app#specialKeys.ctrlDown -> vertScroller#ratio*.0.5, vertScroller#decrPos
            | Keys.L when app#specialKeys.ctrlDown -> horzScroller#ratio*.0.5, horzScroller#incrPos
            | Keys.H when app#specialKeys.ctrlDown -> horzScroller#ratio*.0.5, horzScroller#decrPos
            | Keys.J -> vertScroller#ratio, vertScroller#incrPos
            | Keys.K -> vertScroller#ratio, vertScroller#decrPos
            | Keys.L -> horzScroller#ratio, horzScroller#incrPos
            | Keys.H -> horzScroller#ratio, horzScroller#decrPos
            | _ -> 1.0, fun _ -> ()
        in
        fn ratio;
        self#updateTranslation

    method private updateTranslation =
        translation#setTranslation (rect.x -. self#offsetX, rect.y -. self#offsetY)

    method! onResize r =
        super#onResize r |> ignore;
        let open Float in
        let sz = cont#preferredSize in
        let w = max rect.w sz.w
        and h = max rect.h sz.h in
        (* Position the scroll bars *)
        let barSizeH = vertScroller#barSize in
        let barSizeW = horzScroller#barSize in
        cont#onResize Rect.{x=0.; y=0.; w; h} |> ignore;
        vertScroller#setRatio (rect.h /. h);
        horzScroller#setRatio (rect.w /. w);
        vertScroller#resize Rect.{x=rect.x +. rect.w -. barSizeW; 
                                  y=rect.y; h=rect.h; w=barSizeW};
        horzScroller#resize Rect.{x=rect.x;
                                  y=rect.y +. rect.h -. barSizeH; h=barSizeH; w=rect.w};
        app#renderer#groupUpdates (fun _ ->
            self#updateScrollbarVisibility;
            self#updateTranslation
        )

    method private updateScrollbarVisibility =
        let updateBar b =
            if Float.(b#ratio < 1.0) then (renderObject#attach b#renderObject)
            else (renderObject#detach b#renderObject)
        in
        updateBar vertScroller;
        updateBar horzScroller;

    method ensureVisible (region : Rect.t) =
        let open Float in
        let cgeom = cont#rect in
        let r = Rect.intersection region cgeom in
        let ox = self#offsetX in
        let oy = self#offsetY in
        let v = { self#rect with x=ox; y=oy; } in
        if not (Rect.inside r v) then begin
            let oy1 = Float.abs ((r.y +. r.h) -. (v.y +. v.h))
            and oy2 = Float.abs (v.y -. r.y)
            and ox1 = Float.abs ((r.x +. r.w) -. (v.x +. v.w))
            and ox2 = Float.abs (v.x -. r.x) in
            let vdenom = max (cont#rect.h -. self#rect.h) 1. in
            let wdenom = max (cont#rect.w -. self#rect.w) 1. in
            let vpos = (if oy1 < oy2 then (oy +. oy1) else (oy -. oy2)) /. vdenom in
            let wpos = (if ox1 < ox2 then (ox +. ox1) else (ox -. ox2)) /. wdenom in
            app#renderer#groupUpdates (fun _ ->
                vertScroller#setPosition vpos;
                horzScroller#setPosition wpos;
            )
        end

    (* Can never be < 0 or > cont.w - rect.w *)
    method offsetX =
        horzScroller#position *. (cont#rect.w -. self#rect.w)

    (* Can never be < 0 or > cont.h - rect.h *)
    method offsetY =
        vertScroller#position *. (cont#rect.h -. self#rect.h)

    method! contentSize =
        cont#preferredSize

    initializer
        translation#setZIndex 1;
        translation#attach cont#renderObject;
        renderObject#detach (bgRect :> Rendering.nodeObject);
        renderObject#attach (translation :> Rendering.nodeObject);
end
