
;; 快捷键初始化
(defun helm-keymap-init () 
  ""
  (if (fboundp 'helm-keymap)
    (helm-keymap)
    )
)

(defun helm-init ()
  ""
  ;; (require-package 'helm)
  ;;(require 'helm)
  (require 'helm-config)
  ;;(require 'helm-dash)
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;;(setq helm-ff-auto-update-initial-value nil)    ; 禁止自动补全
  (setq helm-split-window-in-side-p           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        ;; 模糊搜索
        helm-M-x-fuzzy-match                  t
        helm-buffers-fuzzy-matching           t
        helm-locate-fuzzy-match               t
        helm-recentf-fuzzy-match              t
        helm-scroll-amount                    8
        helm-ff-file-name-history-use-recentf t)
  (helm-keymap-init)
)

(provide 'helm-init)
