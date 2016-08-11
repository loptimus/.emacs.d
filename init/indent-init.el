;; Highlight-indentation
(defun indent-line
  (add-to-list 'load-path indentPath)
  (require 'highlight-indentation)
  (set-face-background 'highlight-indentation-face "#e3e3d3")
)

(defun indent-init ()
  (indent-line)
)

(provide 'indent-init)
