
;;; ====================================flymake=================================

(defun flymake-init ()
  (autoload 'flymake-find-file-hook "flymake" "" t)
  (add-hook 'find-file-hook 'flymake-find-file-hook)
  (setq flymake-gui-warnings-enabled nil)
  (setq flymake-log-level 0)
)

(provide 'flymake-init)
