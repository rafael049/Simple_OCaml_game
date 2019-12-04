open Sdl

type background =
    { sprite: Sdltexture.t;
      px: float
    }

let new_background renderer =
    let sprite = Sprite.load_sprite renderer ~filename:"Sprites/background.bmp" in
    {sprite = sprite; px = 0.0 }


let update bg time = 
    { bg with px = float (time/100) }

let render bg renderer = 
    let x = int_of_float (-.bg.px) in
    let src_rect = Rect.make4 0 0 1280 360 in
    let dst_rect = Rect.make4 x 0 2560 720 in
    Render.set_scale renderer (1.0, 1.0);
    Render.copy renderer
        ~texture: bg.sprite
        ~src_rect
        ~dst_rect ();


