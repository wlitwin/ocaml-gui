module type FontSig = sig
    type weight = Normal
                | Bold

    type metrics = {
        width : float;
        x_bearing : float;
        x_advance : float;
        ascent : float;
        descent : float;
    }

    type t = {
        mutable size : float;
        mutable font : string;
        mutable weight : weight;
    }

    val default_font : t
end

module type GraphicsSig = sig
    type context

    val save : context -> unit
    val restore : context -> unit

    val set_color : context -> Color.t -> unit
    val set_width : context -> float -> unit
    val move_to : context -> float -> float -> unit
    val line_to : context -> float -> float -> unit
    val stroke : context -> unit
    val fill : context -> unit
    val beginPath : context -> unit
    val closePath : context -> unit

    val translate : context -> float -> float -> unit
    val identity_transform : context -> unit

    val set_font_info : context -> Font.t -> unit
    val draw_text_ : context -> Pos.t -> string -> unit

    val measure_text : context -> Font.t -> string -> Font.metrics
    val measure_text_no_context : (Font.t * string) -> Font.metrics
    val font_extents_no_context : Font.t -> Font.font_extents
    val draw_text : context -> Font.t -> Rect.t -> string -> unit

    val clip_rect : context -> Rect.t -> unit
    val clip_reset : context -> unit

    val rectangle : context -> Rect.t -> unit
end

module type WindowingSig = sig
    type context  

    module Graphics : GraphicsSig

    val create : unit -> context 

    val init : 
        context
        -> title:string
        -> size:Size.t
        -> draw:(Graphics.context -> unit)
        -> resize:(Size.t -> unit)
        -> keyPress:(Keys.key -> unit)
        -> keyRelease:(Keys.key -> unit)
        -> unit

    val graphics_context : context -> Graphics.context

    val set_title : context -> string -> unit

    val request_redraw : context -> unit

    val run : (context -> unit) -> unit
end
