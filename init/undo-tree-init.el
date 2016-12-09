;;; ======================================undo-tree=================================

;; 快捷键初始化
(defun undo-tree-keymap-init () 
  ""
  (if (fboundp 'undo-tree-keymap)
    (undo-tree-keymap)
    )
)

(defun undo-tree-init ()
  "启用undo-tree"
  (require 'undo-tree)
  (global-undo-tree-mode)
  (undo-tree-keymap-init)
)

(provide 'undo-tree-init)
