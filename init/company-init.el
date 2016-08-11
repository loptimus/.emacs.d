
;;; company-mode
(defun company () "Load company"
    (require 'company)
    (setq company-idle-delay nil)
    (add-hook 'after-init-hook 'global-company-mode)
)

(defun company-init ()
  (company)
)

(provide 'company-init)
