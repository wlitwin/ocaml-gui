module CI = Cairo.Image
module CP = Cairo.Pattern

module ByteArray = ByteArray.ByteArray

class image app =
object(self)
    inherit Widget.basicWidget app as super

    val mutable image = ByteArray.empty
    val mutable cimg = CI.create CI.ARGB32 0 0
    val mutable filter = CP.NEAREST
    val mutable keepAspectRatio = true
    val mutable scaleImage = 1.0
    
    method setImage img dimx dimy = 
        image <- img;
        cimg <- CI.create_for_data8 image CI.ARGB32 dimx dimy

    method setFilter f = filter <- f 

    method setKeepAspectRatio keepRatio = keepAspectRatio <- keepRatio

    method setScale s = scaleImage <- s

    method! contentSize = {
        w=Float.(of_int (CI.get_width cimg) *. scaleImage);
        h=Float.(of_int (CI.get_height cimg) *. scaleImage);
    }

    method! paint cr =
        let imgh = Float.of_int (CI.get_height cimg) in
        let imgw = Float.of_int (CI.get_width cimg) in
        let scaleX, scaleY =
            if keepAspectRatio then begin
                let open Float in
                let scale =
                    if rect.w < rect.h
                    then rect.w /. imgw
                    else rect.h /. imgh
                in
                scale, scale
            end else
                rect.w /. imgw, rect.h /. imgh
        in
        (*Stdio.printf "PAINTING %f %f %f %f - Scale is %f %f\n" rect.x rect.y rect.w rect.h scaleX scaleY;*)
        let pat = CP.create_for_surface cimg in
        CP.set_filter pat filter;
        Cairo.translate cr rect.x rect.y;
        Cairo.rectangle cr 0. 0. rect.w rect.h;
        Cairo.scale cr scaleX scaleY;
        Cairo.set_source cr pat;
        Cairo.fill cr;
end
