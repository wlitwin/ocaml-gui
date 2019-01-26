module PWG = Platform.Windowing.Graphics

class virtual drawable =
object(self)
    val virtual renderObject : Rendering.groupObject

(*    method renderObjects : Rendering.primitive list = []*)
    method renderObject = renderObject
end
