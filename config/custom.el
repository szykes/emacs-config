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
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3"
    "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(manoj-dark))
 '(custom-safe-themes
   '("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e"
     default))
 '(flycheck-golangci-lint-disable-all t)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(lsp-buf-args '("beta" "lsp" "--timeout" "0" "--log-format" "text"))
 '(lsp-clients-clangd-args
   '("--background-index" "--clang-tidy" "-j=3"
     "--header-insertion=never"))
 '(lsp-ui-peek-enable t)
 '(package-selected-packages
   '(amx anzu auto-compile cmake-font-lock company company-flx counsel
         counsel-projectile counsel-test csv-mode dash dockerfile-mode
         editorconfig f flycheck flycheck-golangci-lint
         flycheck-pycheckers flycheck-yang gn-mode go-mode groovy-mode
         helpful ivy json-mode kubedoc kubernetes lice lsp-ivy
         lsp-mode lsp-pyright lsp-ui magit nlinum projectile
         protobuf-mode python-mode rg swiper transient which-key
         yaml-mode yang-mode yasnippet ztree))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((auto-save) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'custom)
;;; custom.el ends here
