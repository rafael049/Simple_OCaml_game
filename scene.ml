open Player


(* Record que armazena todos os elementos do jogo *)
type scene =
    { player: Player.player;
      background: Background.background;
      obs: Obstacle.obstacle list;
      level: int;
    }

let create_initial_scene rndr =
    let player = Player.new_player rndr 0.0 500.0 in
    let background = Background.new_background rndr in
    {player; background; obs = []; level = 0 } 

let margin = 60.0

let check_player_collision player (obs_list:Obstacle.obstacle list)  =
    let collision p (o:Obstacle.obstacle) = 
        if (p.px +. (float p.width) -. margin ) > o.px &&
            p.px < (o.px +. (float o.width) -. margin ) &&
           (p.py +. (float p.height) -. margin ) > o.py &&
            p.py +. margin < (o.py +. (float o.height) -. margin )
        then true
        else false
    in
    if List.length (List.filter (collision player) obs_list) > 0
    then true
    else false
