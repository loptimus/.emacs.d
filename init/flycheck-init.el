;; flycheck
(defun flycheck()
  (require 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
)

(defun flycheck-init ()
  (flycheck)
  (setq flycheck-check-syntax-automatically '(save))
)

(provide 'flycheck-init)
