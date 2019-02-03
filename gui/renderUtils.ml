
let measure_text (font, text) : Size.t =
    let metrics = Platform.Windowing.Graphics.measure_text_no_context(font, text) in
    Size.{w=metrics.width; h=metrics.ascent+.metrics.descent}
;;
