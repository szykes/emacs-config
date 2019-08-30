# emacs-config

Here is my config.

Some points why you should use this:
- the project perfectly fits in ```.emacs.d```
- standard keybindings
- the missing packages are installed automaically

## Table of contents

  * [Custom stuff](#Custom-stuff)
  * [Saving session](#Saving-session)
  * [Misc of emacs](#Misc-of-emacs)

## Custom stuff

The ```custom-set-variables``` and ```custom-set-faces``` have been moved to ```config/custom.el```.

You don`t need to modify or open it.

## Saving session

All pathes/references of buffers are stored to ```desktop/emacs.desktop``` file. Very useful if you are working on the same project and you don't want to open over-and-over again the same file.

The start-up can be longer than expected because the emacs loads the buffers at start-up.

> Use [emacs daemon](https://www.emacswiki.org/emacs/EmacsAsDaemon) instaed, for example: ```emacs --daemon=$USER``` and ```emacsclient -s $USER -t```.

## Misc of emacs

- turned off menubar
- highlight any kind of matching bracket