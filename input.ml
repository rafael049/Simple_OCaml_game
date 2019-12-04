open Sdl

let key_up, key_down, key_left, key_right, key_space, key_r = 0, 1, 2, 3, 4, 5

let inputs_state = Array.make 10 false

let proc_events  = function
    | Event.KeyDown { Event.keycode = Keycode.W } -> Array.set inputs_state key_up true
    | Event.KeyUp   { Event.keycode = Keycode.W } -> Array.set inputs_state key_up false
    | Event.KeyDown { Event.keycode = Keycode.S } -> Array.set inputs_state key_down true
    | Event.KeyUp   { Event.keycode = Keycode.S } -> Array.set inputs_state key_down false
    | Event.KeyDown { Event.keycode = Keycode.A } -> Array.set inputs_state key_left true
    | Event.KeyUp   { Event.keycode = Keycode.A } -> Array.set inputs_state key_left false
    | Event.KeyDown { Event.keycode = Keycode.D } -> Array.set inputs_state key_right true
    | Event.KeyUp   { Event.keycode = Keycode.D } -> Array.set inputs_state key_right false
    | Event.KeyDown { Event.keycode = Keycode.R } -> Array.set inputs_state key_r true
    | Event.KeyUp   { Event.keycode = Keycode.R } -> Array.set inputs_state key_r false
    | Event.Quit _  -> Sdl.quit (); exit 0
    | _ -> ()

let rec event_loop () = 
    match Event.poll_event () with
    | None -> ()
    | Some ev ->
            let () = proc_events ev in
            event_loop ()

