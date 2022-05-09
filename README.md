# emacs-config

Here is my config.

Some points why you should use this:
- the project perfectly fits in ```.emacs.d```
- standard keybindings
- the missing packages are installed automaically
- the ```lsp-mode```, ```flycheck``` and ```company``` work perfectly together (C* languages)

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

## 24 bit color

This works with PuTTY.

```
$ cat terminfo-custom.src

xterm-24bit|xterm with 24-bit direct color mode,
   use=xterm-256color,
   sitm=\E[3m,
   ritm=\E[23m,
   setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
   setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,

$ tic -x -o ~/.terminfo terminfo-custom.src

$ TERM=xterm-24bit emacs
```

## Misc of emacs

- ```auto-compile``` (with default config)
- ```ivy``` (with default config and built-in fuzzy logic)
- ```helpful``` (with default config)
- ```anzu``` (with default config)
- ```magit``` (with default config)
- ```lice``` (with default config)
- ```which-key``` (with default config)
- ```projectile``` (with hybrid and chached finding files)
- ```lsp-mode``` (with default config)
- turned off menubar
- highlight any kind of matching bracket
- some MacOS related settings

## elisp

Basic stuff:
- ```flycheck```
- ```company```
- ```eldoc```
- ```yasnippet``` (with very limited snippets right now, try ```yas-describe-table-by-namehash```)

## C and Cpp

Advanced stuff:
- smart major mode selector for header files
- ```lsp-mode``` (with ```lsp-ui```)
- ```company``` (with ```lsp-mode``` integration)
- ```flycheck``` (with ```lsp-mode``` integration)
- ```yasnippet``` (with very limited snippets right now, try ```yas-describe-table-by-namehash```)

## YANG

Install: [pyang](https://github.com/mbj4668/pyang)
