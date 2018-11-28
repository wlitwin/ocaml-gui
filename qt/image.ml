open Control

module CI = Cairo.Image
module CP = Cairo.Pattern

module ByteArray = ByteArray.ByteArray

class image app =
object(self)
    inherit control app as super

    val mutable image = ByteArray.empty
    val mutable cimg = CI.create CI.RGB24 0 0
    val mutable filter = CP.NEAREST
    val mutable keepAspectRatio = true
    val mutable scaleImage = 1.0
    
    method setImage img dimx dimy = 
        image <- img;
        cimg <- CI.create_for_data8 image CI.RGB24 dimx dimy

    method setFilter f = filter <- f 

    method setKeepAspectRatio keepRatio = keepAspectRatio <- keepRatio

    method setScale s = scaleImage <- s

    method! sizeHint cr = {
        w=float (CI.get_width cimg) *. scaleImage;
        h=float (CI.get_height cimg) *. scaleImage;
    }

    method! paint cr =
        let imgh = float (CI.get_height cimg) in
        let imgw = float (CI.get_width cimg) in
        let scaleX, scaleY =
            if keepAspectRatio then begin
                let scale =
                    if rect.w < rect.h
                    then rect.w /. imgw
                    else rect.h /. imgh
                in
                scale, scale
            end else
                rect.w /. imgw, rect.h /. imgh
        in
        Printf.printf "PAINTING %f %f %f %f - Scale is %f %f\n" rect.x rect.y rect.w rect.h scaleX scaleY;
        let pat = CP.create_for_surface cimg in
        CP.set_filter pat filter;
        Cairo.translate cr rect.x rect.y;
        Cairo.rectangle cr 0. 0. rect.w rect.h;
        Cairo.scale cr scaleX scaleY;
        Cairo.set_source cr pat;
        Cairo.fill cr;
end
