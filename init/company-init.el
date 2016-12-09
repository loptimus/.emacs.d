
;; 快捷键初始化
(defun company-keymap-init () 
  ""
  (if (fboundp 'company-keymap)
    (company-keymap)
    )
)

;;; company-mode
(defun company-init ()
  "Load company"
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay nil)
  (setq company-minimun-prefix-length 2)
  )

(provide 'company-init)
