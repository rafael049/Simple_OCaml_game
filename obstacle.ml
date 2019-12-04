open Sdl


type obstacle =
    { sprite: Sdltexture.t;
      px: float;
      py: float;
      vx: float;
      vy: float;
      width: int;
      height: int;
    }

let new_obstacle renderer x y vx =
    let sprite = Sprite.load_sprite renderer ~filename:"Sprites/cacto.bmp"
    in
    { sprite = sprite;
      px = x;
      py = y;
      vx = vx;
      vy = 0.0;
      width = 64;
      height = 128;
    }

let spaw_obstacle renderer level time =
    let speed = -.( (float level) /. 4000.0 ) -. 4.0 in
    let period = 200 in
    let pos_x = 1280.0 +. ( float (Random.int 100) ) in
    if (time mod period ) == 0
        then Some(new_obstacle renderer pos_x 500.0 speed )
        else None

let render obs renderer time = 
    let x, y = int_of_float obs.px, int_of_float obs.py in
    let src_rect = Rect.make4 0 0 obs.width obs.height in
    let dst_rect = Rect.make4 x y obs.width obs.height in
    Render.set_scale renderer (1.0, 1.0);
    Render.copy renderer
        ~texture: obs.sprite
        ~src_rect
        ~dst_rect ();
    ()


let update obs = 
    let new_px = obs.px +. obs.vx in
    { obs with
      px = new_px
    }


