;;; custom.el --- custem sets are stored here

;;; Commentary:
;; Do _not_ modify!

;;; Code:
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
 '(lsp-clients-clangd-args (quote ("--background-index" "--clang-tidy" "-j=32")))
 '(package-selected-packages
   (quote
    (which-key lsp-ui auto-compile counsel counsel-projectile counsel-test ivy ivy-explorer lsp-ivy swiper company-lsp lsp-mode projectile ztree anzu yasnippet company-flx helpful cmake-font-lock flycheck-pycheckers python-mode flycheck company transient magit lice json-mode dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
;;; custom.el ends here
