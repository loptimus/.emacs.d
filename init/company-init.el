
;;; company-mode
(defun company () "Load company"
    (require 'company)
    (setq company-idle-delay nil)
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0)
    (setq company-minimun-prefix-length 2)
)

(defun company-init ()
  (company)
)

(provide 'company-init)
