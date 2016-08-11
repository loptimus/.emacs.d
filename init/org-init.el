;;;================================= orgmode configure =================================
(defun org () "Load org-mode"
  (defvar orgModePath "~/.emacs.d/orgMode")
  (add-to-list 'load-path orgModePath)
  (require 'org-install)
)

(defun org-init()
  (org)
)

(provide 'org-init)
