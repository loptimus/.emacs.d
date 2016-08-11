;; ===============================PHP mode====================================

(defun php () "Load php-mode"
  (add-to-list 'load-path phpPath)
  (require 'php-mode)
)

(defun php-init()
  (php)
)

(provide 'php-init)
