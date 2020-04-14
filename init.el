;;; init.el --- config file for emacs

;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

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
;; do NOT forget the rtags has binaries!!!
(package-install-selected-packages)

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
(desktop-save-mode 1)

;; highlight matching bracket immediately
(show-paren-mode 1)
(defvar show-paren-delay 0)

(require 'helm-config)

;; fix of arrows in find-file
(customize-set-variable 'helm-ff-lynx-style-map t)

;; enable fuzzy matching
(customize-set-variable 'helm-mode-fuzzy-match t)
(customize-set-variable 'helm-completion-in-region-fuzzy-match t)

;; default config, no magic is added
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

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

;;; common

(require 'company)
(global-set-key (kbd "<C-tab>") (function company-complete))

;; enable fuzzy logic of company
(with-eval-after-load 'company
  (company-flx-mode +1))

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

;; projectile config
(projectile-mode +1)
(defvar projectile-mode-map) ;; elminate warning
(define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)

;; proctile with helm
(require 'helm-projectile)
(helm-projectile-on)

(require 'lsp-mode)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; company setup for lsp
(require 'company-lsp)
(push 'company-lsp company-backends)

;; it can be auto, t, or nil
(defvar company-lsp-cache-candidates auto)

;; more advanced trigger handling (case of e.g. std::)
(defvar company-lsp-enable-recompletion t)


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

;(require 'flycheck-rtags)
;(defun my-flycheck-rtags-setup ()
;  "Configure flycheck-rtags for better experience."
;  (flycheck-mode)
;  (flycheck-select-checker 'rtags)
;  (setq-local flycheck-check-syntax-automatically nil)
;  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;)
;(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook #'lsp)

;;; shell script

;(add-to-list 'company-backends '(company-shell company-shell-env))
;(add-hook 'shell-script-mode 'company-mode)

(add-hook 'shell-script-mode 'flycheck-mode)


;; python

(add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)


;;; json
(require 'json)


;;; cmake

;; default config, no magic is added
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
(add-hook 'cmake-mode-hook 'company-mode)


(provide 'init.el)
;;; init.el ends here
