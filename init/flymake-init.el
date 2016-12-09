
;;; ====================================flymake=================================

;; 快捷键初始化
(defun flymake-keymap-init () 
  ""
  (if (fboundp 'flymake-keymap)
    (flymake-keymap)
    )
)

(defun flymake-init ()
  ; (autoload 'flymake-find-file-hook "flymake" "" t)
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (setq flymake-gui-warnings-enabled nil)
  (setq flymake-log-level 0)
  (flymake-keymap-init)
)

(provide 'flymake-init)
