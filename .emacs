;;; .emacs --- config file for Emacs

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
    (cmake-font-lock flycheck-pycheckers python-mode company-shell flycheck-rtags flycheck company lice cmake-project transient magit json-mode helm-rtags rtags helm dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; emacs core related

(menu-bar-mode -1)

;; automatically save and restore sessions
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


;;; common

(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


;;; elisp

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
;(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)


;;; C and C++

(require 'rtags)
(setq rtags-display-result-backend 'helm)
;(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(setq rtags-rc-log-enabled t)
(rtags-enable-standard-keybindings)

(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(add-hook 'c-mode-common-hook 'flycheck-mode)

(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
 (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-highlighting-mode nil)) ;; RTags creates more accurate overlays.
;(defun my-flycheck-rtags-setup ()
;  "Own flycheck rtags setup."
;  (flycheck-select-checker 'rtags)
;  (setq-local flycheck-highlighting-mode nil)
;  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;(add-hook 'c-mode-common-hook 'company-mode)
;(add-hook 'c-mode-common-hook 'flycheck-mode)


;;; shell script

;(add-to-list 'company-backends '(company-shell company-shell-env))
(add-hook 'shell-script-mode 'company-mode)
(add-hook 'shell-script-mode 'flycheck-mode)


;; python

(add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)


;;; json
(require 'json)


(provide '.emacs)
;;; .emacs ends here
