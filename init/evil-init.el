;; Evil
(defun evil () "启用evil"
    (require 'evil)
    (evil-mode 1)
)

(defun evil-init()
  (evil)
)

(provide 'evil-init)
