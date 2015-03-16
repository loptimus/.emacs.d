;;;=================================loptimus' cpp_config=================================

(provide 'cpp_config)



;; CC mode
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)


;; C 
(defun my-c-mode-hook()
  "C 语言配置"
  (setq tab-width 4 indent-tabs-mode nil)
  ;; Code style
  (c-set-style "k&r")
  ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
  (c-toggle-auto-state)
  ;; hungry-delete and auto-newline
  ;; (c-toggle-auto-hungry-state 1)
  ;; 在菜单中加入当前Buffer的函数索引
  (imenu-add-menubar-index)
  ;; 在状态条上显示当前光标在哪个函数体内部
  (which-function-mode)
  ;; 按键定义
  ;(define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(f7)] 'compile)
  ;(define-key c-mode-base-map [(meta \`)] 'c-indent-command)
  ;; (define-key c-mode-base-map [(tab)] 'hippie-expand)
  (define-key c-mode-base-map [(control tab)] 'my-indent-or-complete)
  ;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; 预处理设置
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
)
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; 我的C++语言编辑策略
(defun my-c++-mode-hook()
  "C++ 配置"
  (setq tab-width 4 indent-tabs-mode nil)
  (c-set-style "stroustrup")
    ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
  (c-toggle-auto-state)
  ;; hungry-delete and auto-newline
  ;;(c-toggle-auto-hungry-state 1)
  ;; 在菜单中加入当前Buffer的函数索引
  (imenu-add-menubar-index)
  ;; 在状态条上显示当前光标在哪个函数体内部
  (which-function-mode)
  ;; 按键定义
  ;(define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)
  (define-key c-mode-base-map [(return)] 'newline-and-indent)
  (define-key c-mode-base-map [(f7)] 'compile)
  ;(define-key c-mode-base-map [(meta \`)] 'c-indent-command)
  ;; (define-key c-mode-base-map [(tab)] 'hippie-expand)
  (define-key c-mode-base-map [(control tab)] 'my-indent-or-complete)
  ;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
  ;; 预处理设置
  (setq c-macro-shrink-window-flag t)
  (setq c-macro-preprocessor "cpp")
  (setq c-macro-cppflags " ")
  (setq c-macro-prompt-flag t)
  (setq hs-minor-mode t)
  (setq abbrev-mode t)
  ;;  (define-key c++-mode-map [f3] 'replace-regexp)
)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
					;'(compile-command "make")

;; gdb-UI设置
(setq gdb-many-windows t)
(load-library "multi-gud.el")
(load-library "multi-gdb-ui.el")

;;设置semantic搜索范围
(setq semanticdb-project-roots (list (expand-file-name "/")))

;;自定义补全命令， 如果单词在中间就补全，否则就tab
(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (indent-for-tab-command))
  )


;; 补全快捷键， ctrl-tab用senator补全，不显示列表
; (global-set-key [(control tab)] 'my-indent-or-complete)

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

