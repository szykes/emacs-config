;;; .emacs --- init file for Emacs

;;; Commentary:

(require 'package)

;;; code:

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-style "Google")
 '(package-selected-packages
   (quote
    (cmake-ide diff-hl company-rtags helm-rtags flycheck-rtags flycheck helm-make format-all company clang-format google-c-style helpful lice rtags magit seq helm)))
 '(rtags-path
   "/home/buherton/tools/rtags/bin/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; emacs related

(menu-bar-mode -1)

;; Automatically save and restore sessions
(defvar desktop-dirname             "~/.emacs.d/desktop/")
(defvar desktop-base-file-name      "emacs.desktop")
(defvar desktop-base-lock-name      "lock")
(defvar desktop-path                (list desktop-dirname))
(defvar desktop-save                t)
(defvar desktop-load-locked-desktop nil)
(defvar desktop-auto-save-timeout   30)
(desktop-save-mode 1)


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

;; highlight matching bracket immediately
(show-paren-mode 1)
(defvar show-paren-delay 0)


;;; elisp

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)


;;; C and C++

(require 'rtags)
(setq rtags-display-result-backend 'helm)
(setq rtags-rc-log-enabled t)

(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(add-hook 'c-mode-common-hook 'flycheck-mode)

(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  "Own flycheck rtags setup."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

(cmake-ide-setup)


(provide '.emacs)
;;; .emacs ends here
