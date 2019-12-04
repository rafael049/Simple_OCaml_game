open Sdl


(* Constantes *)
let jump_force = 10.0
let gravity_force = 0.3
let run_speed = 4.0

(* Record que representa o player *)
type player = 
    { px: float
    ; py: float
    ; vx: float
    ; vy: float
    ; sprite:  Sdltexture.t list
    ; width: int
    ; height: int
    }



(* Cria uma nova record que representa o player *)
let new_player renderer x y = 
    let sprite = List.map ((fun x -> Sprite.load_sprite renderer ~filename:x )) 
        ["Sprites/camelo1.bmp";
         "Sprites/camelo2.bmp";
         "Sprites/camelo3.bmp";
         "Sprites/camelo4.bmp";
         "Sprites/camelo5.bmp";
        ]
    in
    { px = x
    ; py = y
    ; vx = 0.0
    ; vy = 0.0
    ; sprite = sprite
    ; width = 228
    ; height = 128
    }

(* Movimenta o player *)
let move player =
    let _px = player.px +. player.vx
    and _py = player.py +. player.vy
    in
    { player with
      px = _px
    ; py = _py
    ; vx = player.vx
    ; vy = player.vy

    }

(* Processa as entradas de teclado e altera a velocidade do player *)
let process_input player ~input = 
    let vx = 
        if input.(2) && player.px > 0.0
        then (-.run_speed)
        else if input.(3) && player.px < 1000.0
        then  run_speed
        else 0.0
    and (vy, py) =
        if input.(0) && player.py >= 500.0 
        then (-.jump_force, player.py -. gravity_force )
        else ( player.vy, player.py )
    in
    { player with
      py = py;
      vx = vx;
      vy = vy;
    }

(* Aplica forca da gravidade *)
let gravity player =
    if player.py < 500.0 then
        { player with 
          vy = player.vy +. gravity_force
        }
    else
        { player with 
          vy = 0.0;
          py = 500.0
        }


(* Desenha o player na tela *)
let render player renderer ticks =
    let x, y = int_of_float player.px, int_of_float player.py  in
    let src_rect = Rect.make4 0 0 player.width player.height in
    let dst_rect = Rect.make4 x y player.width player.height in
    Render.set_scale renderer (1.0, 1.0);
    Render.copy renderer
        ~texture:(List.nth player.sprite ((ticks/100) mod 4) )
        ~src_rect
        ~dst_rect ();
    ()

(* Aplica as função ao player *)
let update player input =
    gravity ( (move (process_input player input) ) )

