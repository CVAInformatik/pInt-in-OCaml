


type  button = { xpos : int ; ypos : int ; width : int ; height : int ; label : string }

(* type dropdown = { xpos : int ; ypos : int ; width : int ; height : int ; label : string; items : graphObjectList }
*)
type graphObject = 
       Bt of button 
    |  Dd of button * graphObject list
    
let mkButton  x y w h l = Bt {xpos = x; ypos = y; width = w; height = h; label = l}
  
type graphObjectList =  graphObject list
  
let renderGOaux  x y w  h l =
    Graphics.draw_rect x y w  h ;
    Graphics.moveto  (x+2) (y+2 );
    Graphics.draw_string l  ;;
  
let renderGO  go = 
     match go with
     Bt { xpos  ; ypos  ; width  ; height  ; label  }
      -> renderGOaux xpos ypos width height label 
     | Dd ( { xpos  ; ypos  ; width  ; height  ; label  }, items) 
         -> renderGOaux xpos ypos width height label 
       
let menuline = [ Bt {xpos =  50; ypos = 950; width = 50; height = 20; label = " a label"}; 
                 Bt {xpos = 100; ypos = 950; width = 50; height = 20; label = " b label"};
                 mkButton 150 950 50 20 "c label" ;
                 mkButton 200 950 50 20 "d label" ] ;;      