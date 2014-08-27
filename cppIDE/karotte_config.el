;;;=====================在Emacs下用C/C++编程  http://blog.csdn.net/karotte/article/details/6990031========================


(provide 'karotte_config)



;;;gdb-UI设置
;(setq gdb-many-windows t)
;(load-library "multi-gud.el")
;(load-library "multi-gdb-ui.el")

;;折叠和打开整个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C--") 'semantic-tag-folding-fold-all)
;(define-key semantic-tag-folding-mode-map (kbd "C-=") 'semantic-tag-folding-show-all)

;;折叠和打开单个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-block)
;(define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-fold-block)


;;设置semantic搜索范围
(setq semanticdb-project-roots 
      (list
        (expand-file-name "/")))

;;自定义补全命令， 如果单词在中间就补全，否则就tab		
(defun my-indent-or-complete ()
   (interactive)
   (if (looking-at "\\>")
      (hippie-expand nil)
      (indent-for-tab-command))
)

 ;;补全快捷键， ctrl-tab用senator补全，不显示列表
 ;;alt+/补全，显示列表让选择
(global-set-key [(control tab)] 'my-indent-or-complete)
;(define-key c-mode-base-amp [(meta ?/)] 'semantic-ia-complete-symbol-menu)

(autoload 'senator-try-expand-semantic "senator")
(setq hippie-expand-try-functions-list
      '(
        senator-try-expand-semantic
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
)

;;显示帮助命令
;(global-set-key [f1] 'manual-entry)
;(global-set-key [C-f1] 'info)

;;f3为查找字符串，alt+f3关闭当前缓冲区
;(global-set-key [f3] 'grep-find)
;(global-set-key [M-f3] 'kill-this-buffer)

;;speedbar快捷键
;(global-set-key [(f4)] 'speedbar-get-focus)

;;显示/隐藏工具栏，方便调试
;(global-set-key [f5] 'tool-bar-mode)

;;显示/隐藏菜单栏 M-x menu-bar-open
;(global-set-key [C-f5] 'menu-bar-mode)

;;gdb调试
;(global-set-key [f6] 'gdb)

;;设置C-F12快速查看日程安排
;;F12调到函数定义
;(global-set-key [f12] 'semantic-ia-fast-jump)
;(global-set-key [C-f12] 'list-bookmarks)

;;shift-f12调回去
;(global-set-key [S-f12])
;;;=====================在Emacs下用C/C++编程  http://blog.csdn.net/karotte/article/details/6990031========================
