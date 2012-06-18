;;;=================================loptimus' cpp_config=================================

(provide 'cpp_config)



;; ======================================ac-complete=================================

;(add-to-list 'load-path "~/.emacs.d/commonIDE/auto-complete-1.3.1/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/commonIDE/auto-complete-1.3.1/ac-dict")
;(ac-config-default)
;; ======================================ac-complete=================================


;; ================================cedet==========================================================

;;cedet
;(require 'cedet)
;(global-ede-mode t)

;;;;  Helper tools.
;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(ecb-options-version "2.40")
; '(semantic-default-submodes (quote (global-semantic-decoration-mode global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
; '(semantic-idle-scheduler-idle-time 3))
;(semantic-mode)

;; smart complitions
;(require 'semantic/ia)
;(setq-mode-local c-mode semanticdb-find-default-throttle
;                 '(project unloaded system recursive))
;(setq-mode-local c++-mode semanticdb-find-default-throttle
;                 '(project unloaded system recursive))

;;ecb
;(require 'semantic/analyze)
;(provide 'semantic-analyze)
;(provide 'semantic-ctxt)
;(provide 'semanticdb)
;(provide 'semanticdb-find)
;(provide 'semanticdb-mode)
;(provide 'semantic-load)

;;=========================cedet==================================================


;;===========================ecb==================================================

;;load ecb
;(add-to-list 'load-path "~/.emacs.d/commonIDE/ecb-2.40")
;(require 'ecb)

;;;; 自动启动ecb，并且不显示每日提示
;(setq ecb-auto-activate t
;      ecb-tip-of-the-day nil)

;;;; 各窗口间切换
;(global-set-key [M-left] 'windmove-left)
;(global-set-key [M-right] 'windmove-right)
;(global-set-key [M-up] 'windmove-up)
;(global-set-key [M-down] 'windmove-down)

 ;;;; 隐藏和显示ecb窗口
;(define-key global-map [(control f4)] 'ecb-hide-ecb-windows)
;(define-key global-map [(control f3)] 'ecb-show-ecb-windows)

 ;;;; 使某一ecb窗口最大化
;(define-key global-map "/C-c1" 'ecb-maximize-window-directories)
;(define-key global-map "/C-c2" 'ecb-maximize-window-sources)
;(define-key global-map "/C-c3" 'ecb-maximize-window-methods)
;(define-key global-map "/C-c4" 'ecb-maximize-window-history)

;;;; 恢复原始窗口布局
;(define-key global-map "/C-c`" 'ecb-restore-default-window-sizes)

;; =======================ecb=====================================================



;; ======================================yasnippet=================================

;;load yasnippet
;(add-to-list 'load-path "~/.emacs.d/commonIDE/yasnippet")
;(require 'yasnippet)
;(setq yas/snippet-dirs '("~/.emacs.d/commonIDE/yasnippet/snippets" 
;	"~/.emacs.d/commonIDE/yasnippet/extras/imported"))
;(yas/global-mode 1)

;; ======================================yasnippet =================================



;; ======================================someone's config=================================

(require 'caole_config)
;(require 'karotte_config)













