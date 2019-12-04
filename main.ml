open Sdl
open Player
open Input
open Scene

let width, height = (1280, 720)


let () = 
    (* Inicializar SDL2 *)
    Sdl.init[`VIDEO];
    let window, rndr =
        Render.create_window_and_renderer
            ~width ~height ~flags:[]
    in
    
    (* Cria estado inicial *)
    let initial_scene = Scene.create_initial_scene rndr in

    (* Loop Principal *)
    let rec main_loop scene =
        (* Atualiza vetor de eventos *)
        let () = Input.event_loop()  in
        (* Obtem tempo de execução em milisegundos *)
        let ticks = Timer.get_ticks () in
        (* Obtem nivel de dificultade a partir do tempo de jogo *)
        let current_level = (ticks ) in

        (* Extrai membros da record Scene *)
        let player = scene.player in
        let background = scene.background in
        let obs_list = scene.obs in
        let new_obs_list =
            let maybe_obs = Obstacle.spaw_obstacle rndr current_level ticks in
            match maybe_obs with
            | Some obs -> [obs]
            | None     -> []
        in
        

        (* Atualiza "Objetos" *)
        let _player = Player.update player Input.inputs_state in
        let _obs = 
            if List.length obs_list > 10
                then List.append (List.map Obstacle.update (List.tl obs_list) ) new_obs_list
                else List.append (List.map Obstacle.update obs_list) new_obs_list
            in
        let _background = Background.update background ticks in
        let _scene = {scene with player = _player; background = _background; obs = _obs} in

        (* Desenha os "objetos" na tela *)

        Render.clear rndr;                                      (* Clear screen *)

        Background.render background rndr;                      (* Desenha Plano de fundo no buffer *)
        Player.render _player rndr ticks;                       (* Desenha o Camelo (player) no buffer *)
        List.iter (fun x -> Obstacle.render x rndr ticks) _obs; (* Itera sobre a lista de cactos, desenhando-os no buffer *)

        Render.render_present rndr; (* Render all *)            (* Desenha tudo *)
        Timer.delay 10;                                         (* Aguarda um tempo, evita sobrecarga de CPU *)


        (* Verifica se o camelo colidiu com algum cacto *)

        if Scene.check_player_collision _player _obs                     
        then
            (* Se colidiu, entrar no loop de game-over *)
            let rec gameover_loop () =
                let () = Input.event_loop () in                 (* Atualiza vetor de eventos *)
                if Input.inputs_state.(Input.key_r)             (* Verifica se R foi pressionado *)
                    then main_loop initial_scene                (* Se sim, reinicia o jogo *)
                    else gameover_loop () in                    (* Se não, continua no loop de game-over *)
            gameover_loop ()
        else 
            (* Se nao colidiu, continua no loop principal *)
            main_loop _scene
    in
    (* Chama o loop principal *)
    main_loop initial_scene                     
