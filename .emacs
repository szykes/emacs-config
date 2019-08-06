;;; .emacs --- config file for emacs

;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(c-basic-offset 2)
 '(custom-enabled-themes (quote (manoj-dark)))
 '(custom-safe-themes
   (quote
    ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" default)))
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(package-selected-packages
   (quote
    (company-flx helpful company-rtags cmake-font-lock flycheck-pycheckers python-mode flycheck-rtags flycheck company transient magit lice json-mode helm-rtags rtags helm dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; install only the missing packages based on `package-selected-packages'
;; do NOT forget the rtags has binaries!!!
(package-install-selected-packages)

;;; emacs core related

;; no need to have menubar
(menu-bar-mode -1)

;; automatically save and restore sessions
;; server-client usage is preferred
(defvar desktop-dirname             "~/.emacs.d/desktop/")
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


;;; common

(require 'company)
(global-set-key (kbd "<C-tab>") (function company-complete))

;; enable fuzzy logic of company
(with-eval-after-load 'company
  (company-flx-mode +1))


;;; elisp

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


;;; all C* languages

(require 'rtags)

;; enable logging, see: *RTags Log* buffer
(setq rtags-rc-log-enabled t)

;; make sure the rdm is running in all C* modes
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)

;; https://github.com/Andersbakken/rtags/wiki/Usage#fall-back-to-other-taggers
(rtags-enable-standard-keybindings)

;; helm is fancy
(setq rtags-display-result-backend 'helm)

(add-hook 'c-mode-common-hook 'company-mode)
(setq rtags-completions-enabled t)

;; bind rtags with company
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(setq rtags-autostart-diagnostics t)
(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
  (flycheck-mode)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
)
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

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


(provide '.emacs)
;;; .emacs ends here
