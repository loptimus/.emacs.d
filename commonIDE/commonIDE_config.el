;;;=================================loptimus' commonIDE_config=================================

; Provide under compoments (提供以下组件)
(provide 'commonIDE_config)



;;; ======================================ac-complete=================================

;(add-to-list 'load-path "~/.emacs.d/commonIDE/auto-complete-1.3.1/")
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/commonIDE/auto-complete-1.3.1/ac-dict")
(add-to-list 'load-path autoCompletePath)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat autoCompletePath "/ac-dict"))
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

;;; ======================================ac-complete=================================




;;; ====================================flymake=================================
(autoload 'flymake-find-file-hook "flymake" "" t)
(add-hook 'find-file-hook 'flymake-find-file-hook)
(setq flymake-gui-warnings-enabled nil)
(setq flymake-log-level 0)
;;; ======================================flymake=================================


;;; ================================cedet==========================================================

; Switch cedet
; (add-to-list 'load-path "~/.emacs.d/commonIDE/cedet-1.0.1/common")
(add-to-list 'load-path (concat cedetPath "/common"))

;(load-file "~/.emacs.d/commonIDE/cedet-1.0.1/common/cedet.el")

;Currently CEDET issues a warning “Warning: cedet-called-interactively-p called with 0 arguments, but requires 1”, which can be suppressed by adding (setq byte-compile-warnings nil) in your .emacs file before CEDET is loaded
(setq byte-compile-warnings nil)

(require 'cedet)



; Enable EDE Mode (启动project管理工具EDE)
(global-ede-mode t)

; semantic (语法分析工具)
;(semantic-load-enable-minimun-features)
(semantic-load-enable-code-helpers)
;(semantic-load-enable-guady-code-helpers)
;(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
;(require 'semantic-ia')
;(global-srecode-minor-mode 1)


; Visual bookmarks (书签可视化)
(enable-visual-studio-bookmarks)

; Code collapse (代码折叠)
(require 'semantic-tag-folding nil 'noerror)
(global-semantic-tag-folding-mode 1)


;;; =========================cedet==================================================




;;; =========================== ecb =========================================

; Load ECB
(add-to-list 'load-path ecbPath)
(require 'ecb)


; Auto activate ECB and cancel ECB Daily Tips (ECB 自动启动，取消ECB的每日提示)
;(setq ecb-auto-activate t
;      ecb-tip-of-the-day nil)
   
; Cancel ECB Daily Tips (取消ECB的每日提示)
(setq ecb-tip-of-the-day nil)

; Support mouse action (ECB 支持鼠标动作)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(display-time-mode t)
  '(ecb-options-version "2.40")
  '(ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
  '(show-paren-mode t))

;;; =======================ecb=====================================================



;;; ======================================yasnippet=================================

; Load yasnippet
(add-to-list 'load-path yasnippetPath)
(require 'yasnippet)
(yas/initialize)
;(setq yas/snippet-dirs ((concat yasnippetPath "/snippets") (concat yasnippetPath "/extras/imported")))
(yas/load-directory (concat yasnippetPath "/snippets"))
;(yas/snippet-dirs (concat yasnippetPath "/snippets"))
(yas/global-mode 1)
;;; ======================================yasnippet =================================

;;; ======================================cscope=================================
(add-to-list 'load-path cscopePath)
(require 'xcscope)
(setq cscope-do-not-update-database t)
;;; ======================================cscope=================================

;;; ======================================undo-tree=================================
(add-to-list 'load-path ".")
(require 'undo-tree)
(global-undo-tree-mode)

