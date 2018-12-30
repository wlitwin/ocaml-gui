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

    val clip_rect : context -> Rect.t -> unit
    val clip_reset : context -> unit

    val measure_text : context -> string -> unit
    val draw_text : context -> string -> unit

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

    val set_title : context -> string -> unit

    val request_redraw : context -> unit

    val run : context -> unit
end
