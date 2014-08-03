;;;=================================loptimus' base_config=================================

(provide 'base_config)


;; �ر���ʾ����
;(setq inhibit-startup-message t)


;��Ҫ������ʱ�ļ�
(setq-default make-backup-files nil)

;��y/n����yes/no
(fset 'yes-or-no-p 'y-or-n-p)


;;�޸�EMACS��ɫ����
(setq default-frame-alist  '((cursor-color . "blue" )(cursor-type . box)))
(show-paren-mode)  
(global-font-lock-mode t)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))



;;;; ���ڼ��л�
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)


;; ��ʾʱ��
(display-time-mode 1)
(setq display-time-24hr-format  t)
(setq display-time-day-and-date  t)
(setq display-time-format  "%a(%V) %m.%d/%H:%M")
(display-time)

;;��ʾ�к�
(require 'linum) 
(global-linum-mode t)

;;�����о�
;(setq defaule-line-spaceing 4)

;;ҳ��
;(setq default-fill-column 60)


;;�����﷨����
;(global-font-lock-mode 1)

;;������ʾ����
;(transient-mark-mode t)

;;��������
;(setq visible-bell t)


;; ȡ����ʾ��
(setq ring-bell-function 'ignore)

;;�����и�
;(setq resize-mini-windows nil)


;; tabbar
(require 'tabbar)
(tabbar-mode t) 
(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key [(meta k)] 'tabbar-forward)
;;����ѡ��
(global-set-key [(meta u)] 'tabbar-backward-group)
(global-set-key [(meta i)] 'tabbar-forward-group)

;; set tabbar
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



;�ָ�����
(add-to-list 'load-path "~/.emacs.d/base_config/session/lisp")
(require 'session)
(add-hook 'after-init-book 'session-initialize)



;(require 'glasses)
;;glass
;(global-set-key [(f12)] 'loop-alpha)

;; ��͸������
(eval-when-compile (require 'cl))
(defun toggle-transparency ()
   (interactive)
     (if (/=
		         (cadr (frame-parameter nil 'alpha))
				        100)
	        (set-frame-parameter nil 'alpha '(100 100))
			    (set-frame-parameter nil 'alpha '(95 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;;������ɫ
;(add-to-list 'load-path "~/.emacs.d/base_config/color-theme-6.6.0/")
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-oswald)
;(color-theme-charcoal-black)
;(color-theme-deep-blue)
;(require 'color-theme-ahei)
