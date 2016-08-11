;;; ======================================cscope=================================
(defun cscope () "启用cscope"
  ;(setenv "PATH" (concat (getenv "PATH") ":~/.emacs.d/commonIDE/cscope/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" cscopePath "/bin"))
  (add-to-list 'load-path cscopePath)
  (require 'xcscope)
  (setq cscope-do-not-update-database t)
  (cscope_keymap)
)

(defun cscope-init()
  (cscope)
)

(provide 'cscope-init)
