;;;=================================loptimus' php_config=================================

;;
(provide 'php_config)

;; ===============================PHP mode====================================
(defun php () "Load php-mode"
  (add-to-list 'load-path phpPath)
  (require 'php-mode)
)


