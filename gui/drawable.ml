module PWG = Platform.Windowing.Graphics

class virtual drawable =
object(self)
    val renderObject = new Rendering.nodeObject

    method renderObjects : Rendering.primitive list = []
    method renderObject = renderObject

    method onDraw = ()
end
