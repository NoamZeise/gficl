# Gficl Examples

An explaination of what each sample does and which new techniques it demonstrates.
In order of complexity.

## Minimum

Draws a coloured triangle to the screen

Shows:
- update loop with input
- shader creation and usage
- loading and drawing vertex data
- freeing resources

## Quad Spin

An example of a 2d spinning quad.

Shows:
- using basic linear algebra operations
- multisample drawbuffers
- updating shader uniforms
- how to deal with resizing
- load a texture to the gpu
- dealing with mutisampling
- fullscreen
- update loop frame time variable

## Cube Wave

A field of waving coloured boxes

Shows:
- 3d camera maths
- instance rendering

## Post Processing

A spinning cube with poserization, edge highlighting, 
and a dot pattern applied. 

Shows:
- multiple shaders
- rendering to a framebuffer texture
- using a framebuffer texture in a shader

## Model Loading

Loads a bunny model and renders it with gooch shading and edge highlighting

Shows:
- using `gficl/load` to load an .obj file 

## Shadows

A scene that uses shadow mapping

Shows:
- using depth only framebuffer textures
- modifying framebuffer texture parameters
- blitting multisampled depth buffer
