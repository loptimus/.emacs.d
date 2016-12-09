;;; ======================================cscope=================================

;; 快捷键初始化
(defun cscope-keymap-init () 
  ""
  (if (fboundp 'cscope-keymap)
    (cscope-keymap)
    )
)

(defun cscope-init () "启用cscope"
  (setq cscope-path "~/.emacs.d/lisp/cscope")
  (setenv "PATH" (concat (getenv "PATH") ":" cscope-path "/bin"))
  (require 'xcscope)
  (setq cscope-do-not-update-database t)
  (cscope-keymap-init)
)

(provide 'cscope-init)
