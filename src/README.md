# Library Design

## WITH-WINDOW, WITH-RENDER, WITH-UPDATE

The `WITH-WINDOW` macro should wrap the contents of your entry point function.
It creates a window using `glfw` and frees it afterwards. It takes arguments for what the window state should be, as well as the opengl version to use (default is 3.3).
* You can pass a `PRE-WINDOW-FN` function that takes no arguments. This is to call any glfw code that needs to run after the library initializes but before a window is created (ie glfw window hints).
* You can pass a `RESIZE-CALLBACK` function that takes a width and height. This will be called with the current window's width and height each time the window dimension changes. This is useful for updating projection matrices and recreating any framebuffers that are dependant on the current window resolution.


`WITH-RENDER` should wrap your rendering code `(with-render [rendering-code...])`. 
It calls `GLFW:SWAP-BUFFERS` after the body finishes. This swaps the backbuffer with the frontbuffer to display your rendered image to the window.


`WITH-UPDATE` should wrap your state updating code `(with-update () [updating-code...])`. It calls `GLFW:POLL-EVENTS` before the body, which checks for input and window events. 
It calls `UPDATE-RENDER-STATE` after the body, which updates the library state such as the current input state for `MAP-KEYS-DOWN`.
Optionally by using the form `(with-update (dt) )`, the variable `DT` (can be called anything) will store the seconds elapsed since the last frame. This is useful for framerate independant physics calculations.

## GL-OBJECT

OpenGL resources such as shaders, textures, renderbuffers, framebuffers or vertex-data can be allocated using the `MAKE-[resource-name]` functions. 
These functions always return a type that inherits from `GL-OBJECT`. The resources must be manually freed by calling `DELETE-GL` on the returned object.


`BIND-GL` can be called on instances of `GL-OBJECT`, it will call an opengl bind function the most obvious way for a particular resource.
* For textures, renderbuffer, framebuffers and vertex data `GL:BIND-[resource]` is called with the resource id
* For shaders `GL:USE-PROGRAM` is called with the shader id.

## VERTEX-DATA

## MAKE-TEXTURE

## VECTOR, MATRIX, QUATERNION

## KEY, MAP-KEYS
