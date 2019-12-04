open Sdl

let pixel_for_surface ~surface ~rgb = 
    let fmt = Surface.get_pixelformat_t surface in
    let pixel_format = Pixel.alloc_format fmt in
    let pixel = Pixel.map_RGB pixel_format rgb in
    Pixel.free_format pixel_format;
    (pixel)

let load_sprite renderer ~filename = 
    let surf = Surface.load_bmp ~filename in
    let rgb = (0x00, 0x7f, 0x7f) in
    let key = pixel_for_surface ~surface:surf ~rgb in
    Surface.set_color_key surf ~enable:true ~key;
    let tex = Texture.create_from_surface renderer surf in
    Surface.free surf;
    (tex)
