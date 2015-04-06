;;;=================================loptimus' php_config=================================

;;
(provide 'php_conf)

;; ===============================PHP mode====================================
(defun php () "Load php-mode"
  (add-to-list 'load-path phpPath)
  (require 'php-mode)
)


