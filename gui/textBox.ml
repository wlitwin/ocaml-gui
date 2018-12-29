class textBoxWidget app = object(self)
    inherit Widget.basicWidget app as super

    val mutable text : string = ""
    val mutable cursorLoc = 0
    val mutable showCursor = true
    val mutable passwordChar = 'x'
    val mutable hideText = false

    method setText new_text =
        text <- new_text;
        self#moveCursorToEnd

    method showCursor = showCursor
    method setShowCursor b = showCursor <- b

    method hideText = hideText
    method setHideText b = hideText <- b

    method passwordChar = passwordChar
    method setPasswordChar c = passwordChar <- c

    method text = text

    method contentSize =
        self#measureText Util.dummy_ctx 
        (if String.(=) text "" then "default_size" else text)

    method measureText cr text =
        Text.measure_text ~cr style#fontInfo text

    method drawText cr =
        Text.draw_text cr rect style self#renderText

    method private splitOnCursor =
        let before = String.sub text 0 cursorLoc
        and after = String.sub text cursorLoc (String.length text - cursorLoc) in
        (before, after)

    method private insertAtCursor new_char =
        let before, after = self#splitOnCursor in
        text <- before ^ new_char ^ after;
        cursorLoc <- cursorLoc + 1

    method private deleteAtCursor =
        let before, after = self#splitOnCursor in
        let before = String.sub before 0 (max 0 (String.length before - 1)) in
        text <- before ^ after;
        self#moveCursorBack

    method private moveCursorBack =
        cursorLoc <- if cursorLoc > 0 then cursorLoc - 1 else 0

    method private moveCursorForward =
        let len = String.length text in
        cursorLoc <- if cursorLoc < len then cursorLoc + 1 else len

    method private moveCursorToBeginning =
        cursorLoc <- 0

    method private moveCursorToEnd =
        cursorLoc <- String.length text

    method private deleteWordBehindCursor =
        (* TODO just use a while loop *)
        match String.rindex_from text (max 0 (cursorLoc - 2)) ' ' with
        | None -> 
            text <- String.sub text cursorLoc (String.length text - cursorLoc);
            cursorLoc <- 0;
        | Some idx ->
            let before = String.sub text 0 (idx + 1)
            and after = String.sub text cursorLoc (String.length text - cursorLoc) in
            text <- before ^ after;
            cursorLoc <- idx + 1;

    method onKeyDown key =
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
        self#invalidate;
        Mixins.Propagate

    method private renderText =
        if hideText then String.make (String.length text) passwordChar
        else text

    method private drawCursor cr =
        if showCursor then begin
            Cairo.save cr;
            let text = String.sub self#renderText 0 cursorLoc in
            let size = Text.measure_text ~cr style#fontInfo text in
            let fgColor = style#fgColor in
            Color.set cr fgColor;
            Cairo.set_line_width cr 1.;
            Cairo.move_to cr (rect.x +. size.w) (rect.y +. rect.h);
            Cairo.line_to cr (rect.x +. size.w) rect.y;
            Cairo.stroke cr;
            Cairo.restore cr;
        end

    method! paint cr =
        self#drawText cr;
        if isFocused then
            self#drawCursor cr

    initializer
        style#setBGColor Color.white;
        style#borderStyle#setStyle Rectangle;
        style#borderStyle#setSize 2.0;
        style#borderStyle#setColor Color.black;
end
