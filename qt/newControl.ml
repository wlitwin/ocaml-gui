open Rect
open Mixins

class control =
object(self)
    inherit layoutable
    inherit styleable
    inherit handlesMouse
    inherit handlesKeyboard

    val id : int = 0
    val mutable eventHandlers = []
    val mutable rect : rect = Rect.{x=0.;y=0.;w=20.;h=20.}
    val style : Style.style = new Style.style 

    method preferredSize = self#size
end
