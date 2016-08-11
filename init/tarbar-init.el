;; tabbar

;; set tabbar
(defun set-tarbar ()
  (set-face-attribute 'tabbar-default-face nil
                      :family "DejaVu Sans Mono"
                      :background "gray80"
                      :foreground "gray30"
                      :height 1.0
                      )
  (set-face-attribute 'tabbar-button-face nil
                      :inherit 'tabbar-default
                      :box '(:line-width 1 :color "yellow")
					)
  (set-face-attribute 'tabbar-selected-face nil
                      :inherit 'tabbar-default
                      :foreground "DarkGreen"
                      :background "LightGoldenrod"
                      :box '(:line-width 2 :color "Darkgoldenrod")
                      :overline "black"
                      :underline "black"
                      :weight 'bold)
  (set-face-attribute 'tabbar-unselected-face nil :inherit 'tabbar-default :box '(:line-width 2 :color "#00B2BF"))
)

;; 初始化
(defun tarbar-init ()
  (require 'tabbar)
  (tabbar-mode t)
)

(provide 'tarbar-init)
