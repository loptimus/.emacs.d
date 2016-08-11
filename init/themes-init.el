;;; Color Theme

; 启用font-lock-mode，在color-theme中会用到
;(global-font-lock-mode t)

;(add-to-list 'load-path "~/.emacs.d/baseConf/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)

;下列任选一种
;(color-theme-oswald)
;(color-theme-charcoal-black)
;(color-theme-deep-blue)
;(color-theme-ahei)
;(color-theme-molokai)

(defun themes-init ()
  (load-theme 'monokai t)
)

(provide 'themes-init)
