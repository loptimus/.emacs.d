
;;; ================================cedet==========================================================

;; 快捷键初始化
(defun cedet-keymap-init () 
  ""
  (if (fboundp 'cedet-keymap)
    (cedet-keymap)
    )
)

(defun cedet-init ()
  (setq cedet-path "~/.emacs.d/lisp/cedet-1.1")
  (add-to-list 'load-path (concat cedet-path "/common"))
  ;;Currently CEDET issues a warning “Warning: cedet-called-interactively-p called with 0 arguments, but requires 1”, which can be suppressed by adding (setq byte-compile-warnings nil) in your .emacs file before CEDET is loaded
  (setq byte-compile-warnings nil)
  (require 'cedet)
  ;; Enable EDE Mode (启动project管理工具EDE)
  (global-ede-mode t)
  ;; semantic (语法分析工具)
  ;;(semantic-load-enable-minimun-features)
  (semantic-load-enable-code-helpers)
  ;;(semantic-load-enable-guady-code-helpers)
  ;;(semantic-load-enable-excessive-code-helpers)
  (semantic-load-enable-semantic-debugging-helpers)
  ;;(require 'semantic-ia')
  ;;(global-srecode-minor-mode 1)
  ;; Visual bookmarks (书签可视化)
  (enable-visual-studio-bookmarks)
  ;; Code collapse (代码折叠)
  (require 'semantic-tag-folding nil 'noerror)
  (global-semantic-tag-folding-mode 1)
  (cedet-keymap-init)
)

(provide 'cedet-init)
