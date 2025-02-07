# emacs-config

Here is my emacs config with settings on macOS (iTerm2) and Linux.

Some points why you should use this:
- Proper macOS (iTerm2) settings to use emacs in CLI
- the project perfectly fits in ```.emacs.d```
- standard keybindings
- the missing packages are installed automaically
- the ```lsp-mode```, ```flycheck``` and ```company``` work perfectly together (C* languages)
- etc.

## Table of contents
  * [24 bit color](#24-bit-color)
  * [Shell environment](#Shell-environment)
  * [macOS settings](#macOS-settings)
  * [Build emacs 29](#Build-emacs-29)
  * [Custom stuff](#Custom-stuff)
  * [Saving session](#Saving-session)
  * [Misc of emacs](#Misc-of-emacs)
  * [elisp](#elisp)
  * [C and Cpp](#C-and-Cpp)
  * [YANG](#YANG)

## 24 bit color

```
$ cat > terminfo-custom.src
xterm-24bit|xterm with 24-bit direct color mode,
   use=xterm-256color,
   sitm=\E[3m,
   ritm=\E[23m,
   setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
   setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,

$ tic -x -o ~/.terminfo terminfo-custom.src
$ rm terminfo-custom.src
```

## Shell environment

I use ```zsh``` recently and some emacs related config is in my ```.zshrc```, see: [zsh-config](https://github.com/szykes/zsh-config)

## macOS settings

Download Ubuntu fonts from here: [Ubuntu Fonts](https://design.ubuntu.com/resources) and add them to MacOS.

Install iTerm2.

### Keyboard shortcuts

Input Sources menu:
* Untick: Select the previous input source

App Shortcuts menu:
* Add: Application: iTerm, Menu title: Close, Press: shift + command + W

### iTerm2 profile setting

Change Default profile based on the following:

Colors tab:
* Just change to Dark Mode

Text tab:
* Font: Ubuntu Mono, Regular, 14, 100, 101

Terminal tab:
* Scrollback lines: 10 000
* Report terminal type: xterm-24bit

Keys tab:
* General subtab: Left Option key: Esc+

## Build emacs 29
On macOS:
```
brew tap d12frosted/emacs-plus
brew install emacs-plus@29 --without-cocoa --with-native-comp
```
More details: [homebrew-emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)

From source:
```
# Ubuntu 24.04
sudo apt-get install build-essential pkg-config libz-dev libgccjit-13-dev libgnutls28-dev libjansson-dev libncurses5-dev texinfo

export LSP_USE_PLISTS=true
./configure --prefix=/path/to/wherever/ --with-x-toolkit=no --without-x --with-xpm=ifavailable --with-gif=ifavailable --with-json --with-native-compilation
make -j$(nproc)
make install
```

## Custom stuff

The ```custom-set-variables``` and ```custom-set-faces``` have been moved to ```config/custom.el```.

You don`t need to modify it.

## Saving session

All pathes/references of buffers are stored to ```desktop/emacs.desktop``` file. Very useful if you are working on the same project and you don't want to open over-and-over again the same file.

The start-up can be longer than expected because the emacs loads the buffers at start-up.

> Use emacs as deamon to speed up the loading time. This command works pretty well: ```emacsclient --tty --alternate-editor="" -e '(switch-to-buffer nil)'``` because it runs the emacs server at first, if it is not running.

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
