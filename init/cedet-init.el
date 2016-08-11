
;;; ================================cedet==========================================================
(defun cdt () "Load cedet"
  ;; Switch cedet
  ;; (add-to-list 'load-path "~/.emacs.d/commonIDE/cedet-1.0.1/common")
  (add-to-list 'load-path (concat cedetPath "/common"))
  ;;(load-file "~/.emacs.d/commonIDE/cedet-1.0.1/common/cedet.el")
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
  (cedet_keymap)
)

(defun cedet-init ()
  (cdt)
)

(provide 'cedet-init)
