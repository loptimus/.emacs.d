;; flycheck

(defun flycheck-init ()
  (require 'flycheck)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save))
)

(provide 'flycheck-init)
