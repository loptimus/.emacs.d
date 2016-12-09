;;; ======================================yasnippet=================================
(defun yasnippet-init () 
	"Load yasnippet"
  (setq yasnippet-path "~/.emacs.d/lisp/yasnippet")
  (add-to-list 'load-path yasnippet-path)
  (require 'yasnippet)
  (yas/initialize)
  (setq yas/snippet-dirs (list (concat yasnippet-path "/snippets") (concat yasnippet-path "/extras/imported")))
  (yas/load-directory (concat yasnippet-path "/snippets"))
  (yas/global-mode 1)
)

(provide 'yasnippet-init)
