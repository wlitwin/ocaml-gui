module PWG = Platform.Windowing.Graphics

class virtual drawable =
object(self)
    val virtual renderObject : Rendering.nodeObject

(*    method renderObjects : Rendering.primitive list = []*)
    method renderObject = renderObject
end
