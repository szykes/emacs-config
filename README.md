# emacs-config

Here is my config.

Some points why you should use this:
- the project perfectly fits in ```.emacs.d```
- standard keybindings
- the missing packages are installed automaically
- the ```rtags```, ```flycheck``` and ```company``` work perfectly together (C* languages)

## Table of contents

  * [Custom stuff](#Custom-stuff)
  * [Saving session](#Saving-session)
  * [Misc of emacs](#Misc-of-emacs)
  * [elisp](#elisp)
  * [C and Cpp](#C-and-Cpp)

## Custom stuff

The ```custom-set-variables``` and ```custom-set-faces``` have been moved to ```config/custom.el```.

You don`t need to modify it.

## Saving session

All pathes/references of buffers are stored to ```desktop/emacs.desktop``` file. Very useful if you are working on the same project and you don't want to open over-and-over again the same file.

The start-up can be longer than expected because the emacs loads the buffers at start-up.

> Use [emacs daemon](https://www.emacswiki.org/emacs/EmacsAsDaemon) instaed, for example: ```emacs --daemon=$USER``` and ```emacsclient -s $USER -t```.

## Misc of emacs

- ```helm``` (with default config and built-in fuzzy logic)
- ```helpful``` (with default config)
- ```anzu``` (with default config)
- ```magit``` (with default config)
- ```lice``` (with default config)
- turned off menubar
- highlight any kind of matching bracket

## elisp

Basic stuff:
- ```flycheck```
- ```company```
- ```eldoc```
- ```yasnippet``` (with very limited snippets right now)

## C and Cpp

Advanced stuff:
- smart major mode selector for header files
- ```rtags``` (with ```helm``` backend)
- ```company``` (with ```rtags``` integration)
- ```flycheck``` (with ```rtags``` integration)
- ```yasnippet``` (with very limited snippets right now)