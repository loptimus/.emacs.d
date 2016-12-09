;;; =========================== ecb =========================================

;; 快捷键初始化
(defun ecb-keymap-init () 
  ""
  (if (fboundp 'ecb-keymap)
    (ecb-keymap)
    )
)

(defun ecb-init () "Load ECB"
  (setq ecb-path "~/.emacs.d/lisp/ecb-2.40")
  (add-to-list 'load-path ecb-path)
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
  (ecb-keymap-init)
  ;(ecb-options-version "2.40")
)

(provide 'ecb-init)
