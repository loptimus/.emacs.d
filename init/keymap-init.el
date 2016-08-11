;;;================================= Keymap ======================================
;;; 快捷键配置

;;; Default keymap
(defun default_keymap () "Defaule keymap"
; 查看man手册
(global-set-key (kbd "<f1>") 'manual-entry)

; 显示Emacs info
(global-set-key (kbd "S-<f1>") 'info)

; f3为查找字符串
(global-set-key (kbd "C-6") 'grep-find)

; 撤销
(global-set-key (kbd "C-z")  'undo-tree-undo)
(global-set-key (kbd "C-M-z")  'undo-tree-redo)
;(global-set-key (kbd "C-Z")  'redo)

; 只读开关
(global-set-key (kbd "<f3>")  'read-only-mode)

; Mark set
(global-set-key (kbd "M-SPC")  'set-mark-command)

;; 行复制
(global-set-key (kbd "C-c w") 'copy-lines)

; Etags jump back
(global-set-key (kbd "M-[") 'find-tag)
(global-set-key (kbd "M-]") 'pop-tag-mark)

; 书签列表
(global-set-key (kbd "M-8") 'list-bookmarks)
(global-set-key (kbd "<f2>") 'bookmark-set)
(global-set-key (kbd "S-<f2>") 'bookmark-jump)
(global-set-key (kbd "M-<f2>") 'bookmark-delete)

; gdb调试
(global-set-key (kbd "C-4") 'gdb)

;; company
(global-set-key (kbd "C-<tab>") 'company-complete-common)

; Switch windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

; Tabbar
;(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key (kbd "M-h") 'tabbar-backward)
(global-set-key (kbd "M-l") 'tabbar-forward)
; 分组选择
(global-set-key (kbd "M-u") 'tabbar-backward-group)
(global-set-key (kbd "M-i") 'tabbar-forward-group)
)

(defun utils_keymap () "Utils Keymap"
;; === Toggle transparency  ===
;(global-set-key (kbd "C-c t") 'toggle-transparency)
; (启用透明背景)
(global-set-key (kbd "M-9") 'loop-alpha)
;;显示/隐藏工具栏，方便调试
(global-set-key (kbd "C-9") 'tool-bar-mode)
)

;;; CEDET
(defun cedet_keymap () "Cedet Keymap"
; speedbar快捷键
(global-set-key (kbd "C-0") 'speedbar-get-focus)

; 跳转到函数定义
(global-set-key (kbd "C-.") 'semantic-ia-fast-jump)

;; semantic
;折叠和打开整个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C--") 'semantic-tag-folding-fold-all)
;(define-key semantic-tag-folding-mode-map (kbd "C-=") 'semantic-tag-folding-show-all)

;折叠和打开单个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-block)
;(define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-fold-block)
)

;; ECB
(defun ecb_keymap () "Ecb Keymap"
; Show/Hide ECB (显示/隐藏 ECB)
(define-key global-map (kbd "S-<f4>") 'ecb-hide-ecb-windows)
(define-key global-map (kbd "S-<f3>") 'ecb-show-ecb-windows)

; Maximize a ECB windows (最大化一个ECB窗口)
;(define-key global-map "/C-c1" 'ecb-maximize-window-directories)
;(define-key global-map "/C-c2" 'ecb-maximize-window-sources)
;(define-key global-map "/C-c3" 'ecb-maximize-window-methods)
;(define-key global-map "/C-c4" 'ecb-maximize-window-history)

; Restore ECB dafault windows layout (恢复ECB窗口默认布局)
;(define-key global-map "/C-c`" 'ecb-restore-default-window-sizes)
)

;; Cscope
(defun cscope_keymap () "Cscope Keymap"
;cscope
(global-set-key (kbd "M-;") 'cscope-find-this-symbol)
(global-set-key (kbd "M-'")  'cscope-find-global-definition)
(global-set-key (kbd "M-\\")  'cscope-pop-mark)
;(global-set-key (kbd "M-;") 'cscope-prev-symbol)
;(global-set-key (kbd "M-'") 'cscope-next-symbol)
(global-set-key (kbd "S-<f3>")  'cscope-set-initial-directory)
)

(provide 'keymap-init)
