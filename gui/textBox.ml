class textBoxWidget app = object(self)
    inherit Label.label app as super

    method onKeyDown key =
        (match key with
        | Keys.Backspace -> text <- Util.strLeft text
        | key when Keys.is_printable key -> text <- text ^ Keys.to_string key
        | _ -> ());
        self#invalidate;
        Mixins.Propagate
end
