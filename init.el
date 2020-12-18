;;; init.el --- config file for emacs

;;; Commentary:

;;; Code:

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

;; no need to have menubar
(menu-bar-mode -1)

;; automatically save and restore sessions
;; server-client usage is preferred
(when (not (file-directory-p desktop-dirname))
  (progn
    (message "Create %s" desktop-dirname)
    (make-directory desktop-dirname)))
(defvar desktop-base-file-name      "emacs.desktop")
(defvar desktop-base-lock-name      "lock")
(defvar desktop-path                (list desktop-dirname))
(defvar desktop-save                t)
(defvar desktop-load-locked-desktop nil)
(defvar desktop-auto-save-timeout   30)
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

;;(require 'ivy-explorer)
;;(ivy-explorer-mode 1)

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

;; delete trailing whitespace at file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; the indentation shall be space only
(setq indent-tabs-mode nil)
;; expect in the makefiles
(add-hook 'makefile-mode-hook '(lambda () (setq indent-tabs-mode t)))

;; TODO magit & its ivy

(require 'projectile)
(projectile-mode +1)
;; you can set one directory for searching projects by calling `projectile-discover-projects-in-directory'
;; you set more directories by setting `projectile-project-search-path'

;; `counsel-git' is used as file search in the project
;; `counsel-git-grep' is used the search in files

(setq projectile-enable-caching t)
(setq projectile-indexing-method 'hybrid)
;; .projectile file in the project will filter the findings

;; C-c p C-h for keybinding help
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'counsel-projectile)
(counsel-projectile-mode)
;; more info about the keybindings: https://github.com/ericdanan/counsel-projectile

(require 'lsp-mode)

;; you need to declare .clang-tidy file for the specific project. Otherwise the clang-tidy will not work.

(require 'lsp-ui)
;; `xref-pop-marker-stack' works with lsp-ui
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; showing always lsp ui doc is annoying
(setq lsp-ui-doc-enable nil)
;; sideline seems not work in no-window mode
(setq lsp-ui-sideline-enable nil)

;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)

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


;;; elisp

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)


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

;; enable yasnippet mode
(add-hook 'c-mode-common-hook #'yas-minor-mode)

;; enable company mode
(add-hook 'c-mode-common-hook 'company-mode)
;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook #'lsp)

;;; shell script

;(add-to-list 'company-backends '(company-shell company-shell-env))
;(add-hook 'shell-script-mode 'company-mode)

(add-hook 'shell-script-mode 'flycheck-mode)


;; python

(setq-local flycheck-python-pylint-executable "python3.8")
(add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)
(add-hook 'python-mode-hook 'flycheck-mode)


;;; json
(require 'json)


;;; cmake

;; default config, no magic is added
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
(add-hook 'cmake-mode-hook 'company-mode)


;;; go

;; set up before-save hooks to format buffer and add/delete imports.
(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 2)
    (setq indent-tabs-mode 1)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

;; enable yasnippet mode
(add-hook 'go-mode-hook #'yas-minor-mode)

;; enable company mode
(add-hook 'go-mode-hook 'company-mode)
;(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook #'lsp)


(provide 'init.el)
;;; init.el ends here
