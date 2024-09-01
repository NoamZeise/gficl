# Gficl Examples

## Quad Spin

An example of a 2d spinning quad.

Shows:
- using basic linear algebra operations
- multisample dramebuffers
- shader creation and uniform updating
- loading vertex data to the gpu
- how to deal with resizing
- load a texture to the gpu
- dealing with mutisampling
- draw to the screen
- cleanup resources after
- user input
- fullscreen

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
