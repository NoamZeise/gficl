# gficl

A common lisp library for simplifying the creation of opengl apps.
This does not replace opengl functions, instead making things like
framebuffer creation and vertex data loading easier.

## Requirements

* A Common Lisp Implementation (tested with SBCL)
* The glfw C library
* Quicklisp

## Building an Example 

To check all the requirements are installed properly, run `make`.
A portable executable version of one of the examples should be at `bin/cube-wave`.

## Using the Library

See `examples/` for how to use the library.

`gficl-examples.asd` shows you how to import the library into your project.
You will need to do `(load "gficl.asd")` before loading your own system,
so that the library is defined.

The basic outline of an app usign this library is a main loop that opens a 
window and updates and draws each frame.

```
(defun my-app ()
  (gficl:with-window
   (:title "My App" :width 500 :height 400 :resize-callback #'my-resize-fn)
   (my-setup-fn)
   (loop until (gficl:closed-p)
	 do (my-update-fn)
	 do (my-draw-fn))
   (my-cleanup-fn)))
```

where `my-setup-fn` and `my-cleanup-fn` would create and destroy any top level opengl resources
you might need, such as shaders, framebuffers, vertex data, textures, etc.

`my-resize-fn` takes a width and height, and is called any time the window is resized. 
This can be used for recreating the framebuffers to match the screen size, or updating
the projection matrix.
