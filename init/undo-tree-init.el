;;; ======================================undo-tree=================================
(defun undo-tree () "启用undo-tree"
  (add-to-list 'load-path ".")
  (require 'undo-tree)
  (global-undo-tree-mode)
)

(defun undo-tree-init()
  (undo-tree)
)

(provide 'undo-tree-init)
