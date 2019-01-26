module Graphics = Platform.Windowing.Graphics

module Cached = struct
end

class ['a, 'b] textBoxWidget app = object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val mutable cursorLoc = 0
    val mutable showCursor = true
    val textObject = app#renderer#createTextObject
    val cursorObject = app#renderer#createRectObject
    val border = app#renderer#createRectObject

    method text = textObject#text
    method setText new_text =
        app#renderer#groupUpdates (fun _ ->
            self#setTextInternal new_text;
            self#moveCursorToEnd;
        )

    method private setTextInternal new_text =
        app#renderer#groupUpdates (fun _ ->
            textObject#setText new_text;
            self#updateCursor;
        );
        events#handle HandlesEvent.(mkEvent `TextChanged (`TextChangedP new_text));

    method showCursor = showCursor
    method setShowCursor b = showCursor <- b

    method contentSize = 
        let sz : Size.t = textObject#size in
        if Float.(sz.h = 0.) then
            Rendering.measure_text (textObject#font, "defaultText")
        else (
            sz
        )

    method private splitOnCursor =
        let text = self#text in
        let before = String.sub text 0 cursorLoc
        and after = String.sub text cursorLoc (String.length text - cursorLoc) in
        (before, after)

    method private insertAtCursor new_char =
        let before, after = self#splitOnCursor in
        cursorLoc <- cursorLoc + 1;
        self#setTextInternal (before ^ new_char ^ after);

    method private deleteAtCursor =
        let before, after = self#splitOnCursor in
        let before = String.sub before 0 (max 0 (String.length before - 1)) in
        cursorLoc <- (if cursorLoc > 0 then cursorLoc - 1 else 0);
        self#setTextInternal (before ^ after);

    method private moveCursorBack =
        cursorLoc <- (if cursorLoc > 0 then cursorLoc - 1 else 0);
        self#updateCursor

    method private moveCursorForward =
        let len = String.length self#text in
        cursorLoc <- if cursorLoc < len then cursorLoc + 1 else len;
        self#updateCursor

    method private moveCursorToBeginning =
        cursorLoc <- 0;
        self#updateCursor

    method private moveCursorToEnd =
        cursorLoc <- String.length self#text;
        self#updateCursor

    method private deleteWordBehindCursor =
        (* TODO just use a while loop *)
        match String.rindex_from self#text (max 0 (cursorLoc - 2)) ' ' with
        | None -> 
            let text = self#text in
            cursorLoc <- 0;
            self#setTextInternal (String.sub text cursorLoc (String.length text - cursorLoc));
        | Some idx ->
            let text = self#text in
            let before = String.sub text 0 (idx + 1)
            and after = String.sub text cursorLoc (String.length text - cursorLoc) in
            cursorLoc <- idx + 1;
            self#setTextInternal (before ^ after);

    method! onKeyDown key =
        let open Application in
        (match key with
        | Keys.Backspace -> self#deleteAtCursor
        | Keys.LArrow -> self#moveCursorBack
        | Keys.RArrow -> self#moveCursorForward
        | Keys.H when app#specialKeys.ctrlDown -> self#moveCursorBack
        | Keys.L when app#specialKeys.ctrlDown -> self#moveCursorForward
        | Keys.A when app#specialKeys.ctrlDown -> self#moveCursorToBeginning
        | Keys.E when app#specialKeys.ctrlDown -> self#moveCursorToEnd
        | Keys.W when app#specialKeys.ctrlDown -> self#deleteWordBehindCursor
        | key when Keys.is_printable key && app#specialKeys.shiftDown -> 
                self#insertAtCursor (Keys.to_string key |> String.uppercase)
        | key when Keys.is_printable key -> self#insertAtCursor (Keys.to_string key)
        | _ -> ());

    method updateCursor =
        let text = String.sub textObject#text 0 cursorLoc in
        let size : Size.t = Rendering.measure_text(textObject#font, text) in
        cursorObject#setRect Rect.{x=rect.x+.size.w; y=rect.y; w=2.; h=size.h}

    method! onFocused =
        renderObject#addChild cursorObject#obj;

    method! onUnfocused =
        renderObject#removeChild cursorObject#obj;

    method! onResize r =
        super#onResize r;
        border#setRect rect;
        app#renderer#groupUpdates (fun _ ->
            textObject#setPos Pos.{x=rect.x; y=rect.y};
            self#updateCursor
        )

    initializer
        self#setBGColor Color.white;
        border#setMode Rendering.Stroke;
        border#setColor Color.black;
        renderObject#addChild border#obj;
        renderObject#addChild textObject#obj;
        cursorObject#setId "cursor";
        textObject#setId "text";
        border#setId "border";
        border#setZIndex 1;
        textObject#setZIndex 2;
        cursorObject#setZIndex 2;
        cursorObject#setRect Rect.{x=0.; y=0.; w=1.; h=1.};
        cursorObject#setColor Color.black;
end
