open Def

class spacer app size =
object(self)
    inherit control app as super

    method! sizeHint cr =
        size
end

