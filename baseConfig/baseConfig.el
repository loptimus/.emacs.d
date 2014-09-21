;;;=================================loptimus' base_config=================================

(provide 'baseConfig)


; Disable Startup display (关闭启动画面)
;(setq inhibit-startup-message t)


; Disable backup file (不要生成临时文件)
(setq-default make-backup-files nil)

; y/n instead of yes/no (用y/n代替yes/no)
(fset 'yes-or-no-p 'y-or-n-p)


; Set windows color (修改EMACS配色方案)
(setq default-frame-alist  '((cursor-color . "blue" )(cursor-type . box)))
(show-paren-mode)  
(global-font-lock-mode t)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ;; '(default ((t (:inherit nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "outline" :family "Consolas")))))
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "outline" :family "Consolas")))))


; Display date and time (显示时间)
(display-time-mode 1)
(setq display-time-24hr-format  t)
(setq display-time-day-and-date  t)
(setq display-time-format  "%a(%V) %m.%d/%H:%M")
(display-time)

; Display line number (显示行号)
(require 'linum) 
(global-linum-mode t)

; Set (设置行距)
;(setq defaule-line-spaceing 4)

; Set (页宽)
;(setq default-fill-column 60)



;; 高亮显示区域
;(transient-mark-mode t)

;; 闪屏警报
;(setq visible-bell t)


;; 取消提示音
(setq ring-bell-function 'ignore)

;; 锁定行高
;(setq resize-mini-windows nil)


;; tabbar
(require 'tabbar)
(tabbar-mode t) 
; set tabbar
;(set-face-attribute 'tabbar-default-face nil
;                    :family "DejaVu Sans Mono"
;                    :background "gray80"
;                    :foreground "gray30"
;                    :height 1.0
;                    )
;(set-face-attribute 'tabbar-button-face nil
;                    :inherit 'tabbar-default
;                    :box '(:line-width 1 :color "yellow")
; 					)
;(set-face-attribute 'tabbar-selected-face nil
;                    :inherit 'tabbar-default
;                    :foreground "DarkGreen"
;                    :background "LightGoldenrod"
;                    :box '(:line-width 2 :color "Darkgoldenrod")
;                    :overline "black"
;                    :underline "black"
;                    :weight 'bold)
;(set-face-attribute 'tabbar-unselected-face nil :inherit 'tabbar-default :box '(:line-width 2 :color "#00B2BF"))



;恢复功能
(add-to-list 'load-path "~/.emacs.d/baseConfig/session/lisp")
(require 'session)
(add-hook 'after-init-book 'session-initialize)



;(require 'glasses)
;;glass
;(global-set-key [(f12)] 'loop-alpha)

;; 半透明设置
(setq alpha-list '((80 55) (100 100)))  
(defun loop-alpha ()  
  (interactive)  
  (let ((h (car alpha-list)))                  
	((lambda (a ab)  
	   (set-frame-parameter (selected-frame) 'alpha (list a ab))  
	   (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))  
	   ) (car h) (car (cdr h)))  
	(setq alpha-list (cdr (append alpha-list (list h))))  
	)  
) 

;;; ==================== Theme =========================
;(add-to-list 'load-path "~/.emacs.d/base_config/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-oswald)
;(color-theme-charcoal-black)
;(color-theme-deep-blue)
;(require 'color-theme-ahei)

