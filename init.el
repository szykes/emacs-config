;;; init.el --- config file for emacs

;;; Commentary:

;;; Code:

;;; General

(defun is-executable-available (executable-name)
  "Check if an EXECUTABLE-NAME is available on the PATH."
  (if (executable-find executable-name)
      t
    nil))

;;; package & file

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq load-prefer-newer t)

(package-initialize)

(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load-file custom-file)
  (error "%s does not exists!" custom-file))

(defvar desktop-dirname (expand-file-name "desktop/" user-emacs-directory))

;; refresh the packages only the very first time
(when (not (file-directory-p desktop-dirname))
  (package-refresh-contents))

;; install only the missing packages based on `package-selected-packages'
(package-install-selected-packages)

;; Solves this issue: Source file `...' newer than byte-compiled file
(require 'auto-compile)
(auto-compile-on-save-mode)
(auto-compile-on-load-mode)

;; to see the hidden *Compile-log* call the `auto-compile-display-log'
(setq auto-compile-display-buffer nil)


;;; emacs core related

;; MacOS related settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'none)
  (setq mac-command-modifier 'meta))

;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(when (eq system-type 'darwin)
  (defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors)))

;; no need to have menubar
(menu-bar-mode -1)

;; show line number
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(setq column-number-mode t)

;; automatically save and restore sessions
;; server-client usage is preferred
(when (not (file-directory-p desktop-dirname))
  (progn
    (message "Create %s" desktop-dirname)
    (make-directory desktop-dirname)))
(defvar desktop-base-file-name      "emacs.desktop")
(when (not (file-regular-p (expand-file-name desktop-base-file-name desktop-dirname)))
  (write-region "" nil (expand-file-name desktop-base-file-name desktop-dirname)))
(defvar desktop-base-lock-name      "lock")
(defvar desktop-path                (list desktop-dirname))
(defvar desktop-save                t)
(defvar desktop-load-locked-desktop nil)
(defvar desktop-auto-save-timeout   10)
(defvar desktop-base-lock-name
  (convert-standard-filename (format ".emacs.desktop.lock-%d" (emacs-pid))))
(desktop-save-mode 1)

;; highlight matching bracket immediately
(show-paren-mode 1)
(defvar show-paren-delay 0)

(defvar ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

(ivy-mode 1)
(defvar ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; amx is installed and it provides history based listing of counsel-M-x. No config is required.

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key [remap isearch-forward] 'swiper) ; C-s
(global-set-key [remap isearch-backward] 'swiper) ; C-r
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

;; enable anzu
(global-set-key [remap query-replace] 'anzu-query-replace)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-anzu-mode +1)

;; performance settings for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 3 1024 1024))

(require 'which-key)
(which-key-mode)

;;; common

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; the indentation shall be space only
(setq-default indent-tabs-mode nil)
;; expect in the makefiles
(add-hook 'makefile-mode-hook '(lambda () (setq indent-tabs-mode t)))


;;; magit

(require 'magit)
(setq magit-branch-read-upstream-first 'fallback)


;;; projectile

(require 'projectile)
(projectile-mode +1)
;; you can set one directory for searching projects by calling `projectile-discover-projects-in-directory'
;; you set more directories by setting `projectile-project-search-path'

;; `counsel-git' is used as file search in the project
;; `counsel-git-grep' is used the search in files

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
;; .projectile file in the project will filter the findings

;; C-c p C-h for keybinding help
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'counsel-projectile)
(counsel-projectile-mode)
;; more info about the keybindings: https://github.com/ericdanan/counsel-projectile

;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setq lsp-use-plists 't)

(require 'lsp-mode)

(setq lsp-file-watch-threshold 5000)

(require 'lsp-ui)
;; `xref-pop-marker-stack' (M-,) works with lsp-ui
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "M-_") #'lsp-ui-peek-find-implementation)

;; showing always lsp ui doc is annoying
(setq lsp-ui-doc-enable nil)
;; sideline seems not work in no-window mode
(setq lsp-ui-sideline-enable nil)

;; lsp-ivy
;; call `lsp-ivy-workspace-symbol' or `lsp-ivy-global-workspace-symbol' to find a symbol

(require 'company)
(global-set-key (kbd "<C-tab>") (function company-complete))

;; enable fuzzy logic of company
(with-eval-after-load 'company
  (company-flx-mode +1))

(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.2)

;; load helper source code of snippets
(defvar snippet-helper-file (expand-file-name "snippets/snippet-helper.el" user-emacs-directory))
(if (file-exists-p snippet-helper-file)
    (load-file snippet-helper-file)
  (error "%s does not exists!" snippet-helper-file))

;; load the snippets database
(require 'yasnippet)
(yas-reload-all)

(defun revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.
They will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun count-buffers (&optional display-anyway)
  "Display or return the number of buffers."
  (interactive)
  (let ((buf-count (length (buffer-list))))
    (if (or (interactive-p) display-anyway)
    (message "%d buffers in this Emacs" buf-count)) buf-count))

;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)

(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)

;;; GitHub Copilot

;; https://github.com/copilot-emacs/copilot.el
;; To enable GH Copilot, just donwload the `copilot.el-main` in `~/.emacs.d`

(defun execute-if-gh-copilot-enabled (fn)
  "Execute the lambda function FN if the `copilot.el-main' is available."
  (if (file-exists-p "copilot.el-main")
      (funcall fn)))

(execute-if-gh-copilot-enabled (lambda ()(
  (add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/copilot.el-main"))
  (require 'copilot)

  (setq copilot-max-characters 500000)
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))))

;;; elisp

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))))

;;; all C* languages

(defun c-or-c++-header ()
  "Set `c++-mode', if the header has matching source file.
Otherwise `c-or-c++-mode' decides."
  (interactive)
  (let ((dir-path (file-name-directory (buffer-file-name)))
	(c-file-name (file-name-base (buffer-file-name)))
	(possbile-location '("" "../" "../src/" "../source/" "../sources/"))
	(found nil))
    (while (and possbile-location
		(not found))
      (let ((location (pop possbile-location))
	    (possible-extension '(".cc" ".cpp" ".CPP" ".c++" ".cp" ".cxx")))
	(while (and possible-extension
		    (not found))
	  (let ((extension (pop possible-extension)))
	    (when (file-exists-p (concat dir-path location c-file-name extension))
	      (setq found t))))))
    (if found
	(c++-mode)
      (c-or-c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-header))

(defvar c-default-style "linux")
(defvar c-basic-offset 2)

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(c-mode 2))))
(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(c++-mode 2))))

;; enable yasnippet mode
(add-hook 'c-mode-common-hook #'yas-minor-mode)

;; enable company mode
(add-hook 'c-mode-common-hook 'company-mode)
;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(add-hook 'c-mode-common-hook 'flycheck-mode)

;; you need to declare .clang-tidy file for the specific project. Otherwise the clang-tidy will not work.
(add-hook 'c-mode-common-hook #'lsp-deferred)

;;; shell script

(defvar sh-basic-offset 2)
(defvar sh-indentation 2)

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(sh-mode 2))))

(when (is-executable-available "npm")
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-bash/
  (add-hook 'sh-mode-hook #'lsp-deferred))

;; install shellcheck
(add-hook 'sh-mode-hook 'flycheck-mode)


;; python

(require 'lsp-pyright)

;; Prerequisites for Python LSP:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; https://emacs-lsp.github.io/lsp-pyright/

(setq lsp-pyright-use-library-code-for-types t) ;; set this to nil if getting too many false positive type errors
(setq lsp-pyright-stub-path (concat (getenv "HOME") "/tools/python-type-stubs"))

;; Prerequisites for Python checkers (they must be on PATH):
;; https://pypi.org/project/flake8/
;; https://pypi.org/project/mypy/
;; https://pypi.org/project/pylint/
;; https://pypi.org/project/pycompilation/

;; Issue: env: python: No such file or directory
;; This usually happens when only python3* is available.
;; Fix: change the shebang of .emacs.d/elpa/flycheck-pycheckers-20220923.2250/bin/pycheckers.py to python3

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; flycheck will rely on flycheck-pycheckers instead of lsp-diagnostic
(add-hook 'python-mode-hook
  (lambda ()
    (make-local-variable 'lsp-diagnostic-package)
    (setq lsp-diagnostic-package :none)
    ;; theoratically the following two lines not needed but it does not work well without them
    (lsp-deferred)
    (flycheck-mode)))

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq python-indent-offset 4)))

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(python-mode 4))))

;;; json


(require 'json)
(custom-set-variables '(js-indent-level 2))
(custom-set-variables '(json-reformat:indent-width 2))
(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(json-mode 2))))

;; https://www.flycheck.org/en/latest/languages.html#syntax-checker-json-python-json
(add-hook 'json-mode-hook
          (lambda ()
            (flycheck-select-checker 'json-python-json)
            (flycheck-mode 1)))

;;; cmake

;; default config, no magic is added
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
(add-hook 'cmake-mode-hook 'company-mode)

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(cmake-mode 2))))

;;; go

;; set up before-save hooks to format buffer and add/delete imports.
(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 2)
    (setq indent-tabs-mode 1)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(go-mode 2))))

(add-hook 'go-mode-hook #'yas-minor-mode)

(add-hook 'go-mode-hook 'company-mode)
;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

;;(custom-set-variables '(flycheck-golangci-lint-tests t))
;;(custom-set-variables '(flycheck-golangci-allow-serial-runners t))
;;(custom-set-variables '(flycheck-golangci-lint-enable-linters "asciicheck,cyclop"))

;; https://github.com/nametake/golangci-lint-langserver

;; https://github.com/weijiangan/flycheck-golangci-lint
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(add-hook 'go-mode-hook #'lsp-deferred)

;; https://www.flycheck.org/en/latest/languages.html#go
(add-hook 'go-mode-hook 'flycheck-mode)

;;; yang

(add-hook 'yang-mode-hook #'flycheck-mode)

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(yang-mode 2))))

;;; yaml

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(yaml-mode 2))))

;; Unlike python-mode, this mode follows the Emacs convention of not
;; binding the ENTER key to `newline-and-indent'.  To get this
;; behavior, add the key definition to `yaml-mode-hook':
(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(when (is-executable-available "npm")
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-yaml/
  (add-hook 'yaml-mode-hook #'lsp-deferred))

;; https://www.flycheck.org/en/latest/languages.html#syntax-checker-yaml-yamllint
;; brew install yamllint
(add-hook 'yaml-mode-hook (lambda ()
            (flycheck-select-checker 'yaml-yamllint)
            (flycheck-mode 1)))

;;; protobuf

;; https://github.com/protocolbuffers/protobuf/blob/main/editors/protobuf-mode.el
;; install: https://github.com/bufbuild/buf
(require 'protobuf-mode)

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(protobuf-mode 2))))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

;; https://www.flycheck.org/en/latest/languages.html#syntax-checker-protobuf-protoc
(add-hook 'protobuf-mode
          (lambda ()
            (flycheck-select-checker 'protobuf-protoc)
            (flycheck-mode 1)))

;;; Dockerfile

(execute-if-gh-copilot-enabled (lambda () (add-to-list 'copilot-indentation-alist '(dockerfile-mode 2))))

(when (is-executable-available "npm")
  ;; yasnippet for proper LSP working
  (add-hook 'dockerfile-mode-hook #'yas-minor-mode)
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-dockerfile/
  (add-hook 'dockerfile-mode-hook #'lsp-deferred))

;; https://www.flycheck.org/en/latest/languages.html#dockerfile
(add-hook 'dockerfile-mode-hook #'flycheck-mode)


;;; terraform

;; https://www.flycheck.org/en/latest/languages.html#terraform


;;; kubernetes



(provide 'init.el)
;;; init.el ends here
