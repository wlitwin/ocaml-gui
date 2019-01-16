module Graphics = Platform.Windowing.Graphics

module Cached = struct
end

class ['a, 'b] textBoxWidget app = object(self)
    inherit ['a, 'b] Widget.basicWidget app as super

    val mutable cursorLoc = 0
    val mutable showCursor = true
    val textObject = app#renderer#createTextObject

    method text = textObject#text
    method setText new_text =
        textObject#setText new_text;
        self#moveCursorToEnd;
        events#handle HandlesEvent.(mkEvent `TextChanged (`TextChangedP new_text));

    method showCursor = showCursor
    method setShowCursor b = showCursor <- b

    method contentSize = textObject#size
        (*
        let metrics = 
            if String.is_empty text then defaultCached
            else measuredCached 
        in
        Size.{w=metrics.width; h=metrics.ascent +. metrics.descent}
        *)

        (*
    method measureText cr text =
        let metrics = Graphics.measure_text cr style#fontInfo text in
        Size.{w=metrics.width; h=metrics.ascent +. metrics.descent}
        *)

    method private splitOnCursor =
        let text = self#text in
        let before = String.sub text 0 cursorLoc
        and after = String.sub text cursorLoc (String.length text - cursorLoc) in
        (before, after)

    method private insertAtCursor new_char =
        let before, after = self#splitOnCursor in
        textObject#setText (before ^ new_char ^ after);
        cursorLoc <- cursorLoc + 1

    method private deleteAtCursor =
        let before, after = self#splitOnCursor in
        let before = String.sub before 0 (max 0 (String.length before - 1)) in
        textObject#setText (before ^ after);
        self#moveCursorBack

    method private moveCursorBack =
        cursorLoc <- if cursorLoc > 0 then cursorLoc - 1 else 0

    method private moveCursorForward =
        let len = String.length self#text in
        cursorLoc <- if cursorLoc < len then cursorLoc + 1 else len

    method private moveCursorToBeginning =
        cursorLoc <- 0

    method private moveCursorToEnd =
        cursorLoc <- String.length self#text

    method private deleteWordBehindCursor =
        (* TODO just use a while loop *)
        match String.rindex_from self#text (max 0 (cursorLoc - 2)) ' ' with
        | None -> 
            let text = self#text in
            textObject#setText (String.sub text cursorLoc (String.length text - cursorLoc));
            cursorLoc <- 0;
        | Some idx ->
            let text = self#text in
            let before = String.sub text 0 (idx + 1)
            and after = String.sub text cursorLoc (String.length text - cursorLoc) in
            textObject#setText (before ^ after);
            cursorLoc <- idx + 1;

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
        | key when Keys.is_printable key -> self#insertAtCursor (Keys.to_string key)
        | _ -> ());

        (*
    method private drawCursor cr =
        if showCursor then begin
            Graphics.save cr;
            let text = String.sub self#renderText 0 cursorLoc in
            let size = Graphics.measure_text cr style#fontInfo text in
            let fgColor = style#fgColor in
            Graphics.set_color cr fgColor;
            Graphics.set_width cr 1.;
            Graphics.move_to cr (rect.x +. size.width) (rect.y +. rect.h);
            Graphics.line_to cr (rect.x +. size.width) rect.y;
            Graphics.stroke cr;
            Graphics.restore cr;
        end
*)

    val border = app#renderer#createRectObject

    method! onResize r =
        super#onResize r;
        border#setRect r;
        let fe : Font.font_extents = textObject#fontExtents in
        textObject#setPos Pos.{x=r.x; y=r.y+.fe.Font.ascent};

        (*
    method! paint cr =
        self#drawText cr;
        if isFocused then
            self#drawCursor cr
            *)

    initializer
        textObject#setText "";
        self#setBGColor Color.white;
        border#setMode Rendering.Stroke;
        border#setColor Color.black;
        renderObject#attach (border :> Rendering.nodeObject);
        renderObject#attach (textObject :> Rendering.nodeObject);
        style#borderStyle#setStyle Rectangle;
        style#borderStyle#setSize 2.0;
        style#borderStyle#setColor Color.black;
        self#setText ""
end
