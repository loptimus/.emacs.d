;; flycheck

(defun flycheck-init ()
  (require 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-check-syntax-automatically '(save))
)

(provide 'flycheck-init)
