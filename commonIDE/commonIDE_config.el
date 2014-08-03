;;;=================================loptimus' commonIDE_config=================================

;;
(provide 'commonIDE_config)



;; ======================================ac-complete=================================

(add-to-list 'load-path "~/.emacs.d/commonIDE/auto-complete-1.3.1/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/commonIDE/auto-complete-1.3.1/ac-dict")
(ac-config-default)

(setq-default ac-sources
		'(ac-source-filename
 		ac-source-functions
 		ac-source-yasnippet
 		ac-source-variables
 		ac-source-symbols
 		ac-source-features
 		ac-source-abbrev
 		ac-source-words-in-same-mode-buffers
 		ac-source-dictionary)
     	
)

;; ======================================ac-complete=================================




;; ====================================flymake=================================
(autoload 'flymake-find-file-hook "flymake" "" t)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level 0)
;; ======================================flymake=================================


;; ================================cedet==========================================================

;;Switch cedet
;;(add-to-list 'load-path "~/.emacs.d/commonIDE/cedet-1.0.1/common")
(add-to-list 'load-path "~/.emacs.d/commonIDE/cedet-1.1/common")

;(load-file "~/.emacs.d/commonIDE/cedet-1.0.1/common/cedet.el")

;Currently CEDET issues a warning “Warning: cedet-called-interactively-p called with 0 arguments, but requires 1”, which can be suppressed by adding (setq byte-compile-warnings nil) in your .emacs file before CEDET is loaded
(setq byte-compile-warnings nil)

(require 'cedet)



;;启动project管理工具ede
(global-ede-mode t)

;;semantic 语法分析工具
;(semantic-load-enable-minimun-features)
(semantic-load-enable-code-helpers)
;(semantic-load-enable-guady-code-helpers)
;(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
;(require 'semantic-ia')


;(global-srecode-minor-mode 1)


;;书签可视化
(enable-visual-studio-bookmarks)

;;代码折叠
(require 'semantic-tag-folding nil 'noerror)
(global-semantic-tag-folding-mode 1)


;;=========================cedet==================================================




;;===========================ecb==================================================

;;load ecb
(add-to-list 'load-path "~/.emacs.d/commonIDE/ecb-2.40")
(require 'ecb)


;;;; ecb自动启动，取消ecb的每日提示
;(setq ecb-auto-activate t
;      ecb-tip-of-the-day nil)
(setq ecb-tip-of-the-day nil)


 ;;;; 隐藏和显示ecb
(define-key global-map [(control f4)] 'ecb-hide-ecb-windows)
(define-key global-map [(control f3)] 'ecb-show-ecb-windows)

 ;;;; 最大化ecb的某一个窗口
;(define-key global-map "/C-c1" 'ecb-maximize-window-directories)
;(define-key global-map "/C-c2" 'ecb-maximize-window-sources)
;(define-key global-map "/C-c3" 'ecb-maximize-window-methods)
;(define-key global-map "/C-c4" 'ecb-maximize-window-history)

;;;; 恢复ecb窗口的默认布局
;(define-key global-map "/C-c`" 'ecb-restore-default-window-sizes)


;;;; ecb 支持鼠标动作

(custom-set-variables
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1)))

;; =======================ecb=====================================================



;; ======================================yasnippet=================================

;;load yasnippet
(add-to-list 'load-path "~/.emacs.d/commonIDE/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs '("~/.emacs.d/commonIDE/yasnippet/snippets" "~/.emacs.d/commonIDE/yasnippet/extras/imported"))
(yas/global-mode 1)

;; ======================================yasnippet =================================





