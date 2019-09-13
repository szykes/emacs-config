;;; snippet-helper.el --- functions for more smart snippets

;;; Commentary:

;;; Code:

(defun snippet-generate-unique-symbol-to-header-guard ()
  "Generate unique symbol from name of module and name of hedaer."
  (let* ((header-path (file-name-directory (buffer-file-name)))
	 (directories (split-string header-path "/"))
	 (module-name (nth (- (safe-length directories) 3) directories))
	 (header-name (replace-regexp-in-string "-" "_" (file-name-base (buffer-file-name)))))
    (upcase (concat module-name "_" header-name "_H_"))))

(provide 'snippet-helper)
;;; snippet-helper.el ends here
