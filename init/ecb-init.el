;;; =========================== ecb =========================================
(defun ecb () "Load ECB"
  ;; Load ECB
  (add-to-list 'load-path ecbPath)
  (require 'ecb)
  ;; Auto activate ECB and cancel ECB Daily Tips (ECB 自动启动，取消ECB的每日提示)
  ;;(setq ecb-auto-activate t ecb-tip-of-the-day nil)
  ;; Cancel ECB Daily Tips (取消ECB的每日提示)
  (setq ecb-tip-of-the-day nil
   ecb-tree-indent 4
   ecb-windows-height 0.5
   ecb-windows-width 0.18
   ecb-auto-compatibility-check nil
   ecb-version-check nil
   inhibit-startup-message t
  )
  ;; Support mouse action (ECB 支持鼠标动作)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(display-time-mode t)
   '(ecb-options-version "2.40")
   '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
   '(show-paren-mode t))
  (ecb_keymap)
)

(defun ecb-init ()
  (ecb)
)

(provide 'ecb-init)
