;;;================================= loptimus' .emacs =================================


; Environment variable
;(setenv "Environment Variable Name" "Environment Variable Value")
;(defvar Variable (getenv "Environment Variable Name"))

; Stack trace on error
(setq stack-trace-on-error t)


; Default directory
(defvar workspace "~/workspace")
(setq default-directory workspace)
(cd workspace)


;;;================================= Emacs base configure =================================
(add-to-list 'load-path "~/.emacs.d/base_config")
(require 'base_config)




;;;================================= Emacs common IDE configure =================================
(add-to-list 'load-path "~/.emacs.d/commonIDE")
(defvar autoCompletePath "~/.emacs.d/commonIDE/auto-complete-1.3.1")
(defvar cedetPath "~/.emacs.d/commonIDE/cedet-1.1")
(defvar ecbPath "~/.emacs.d/commonIDE/ecb-2.40")
(defvar yasnippetPath "~/.emacs.d/commonIDE/yasnippet")
(require 'commonIDE_config)



;;;================================= Erlang configure =================================
;;(add-to-list 'load-path "~/.emacs.d/erlIDE/")

;; Erlang
;;(defvar erlangPath "/usr/local/lib/erlang")
;;(defvar erlangEmacsPath "/usr/local/lib/erlang/lib/tools-2.6.7/emacs")

;; Distel
;;(defvar distelPath "~/.emacs.d/erlIDE/distel-4.03/elisp")

;; Refactorerl
;; (defvar refactorerlPath "~/.emacs.d/erlIDE/refactorerl-0.9.12.05")

;; Wrangler
;;(defvar wranglerPath "/usr/local/share/wrangler")

;;(require 'erlang_config)


;;;================================= C/Cpp configure =================================
(add-to-list 'load-path "~/.emacs.d/cppIDE/")
(require 'cpp_config)


;;;================================= PHP configure ======================================
;(add-to-list 'load-path "~/.emacs.d/phpIDE/")
;(defvar phpPath "~/.emacs.d/phpIDE")
;(require 'php_config)






;;;================================= Keymap ======================================

;; === ECB ===
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

;; === Toggle transparency  ===
;(global-set-key (kbd "C-c t") 'toggle-transparency)

;; === Tabbar  ===
;(global-set-key [(meta j)] 'tabbar-backward)
(global-set-key (kbd "M-j") 'tabbar-backward)
(global-set-key (kbd "M-k") 'tabbar-forward)
; 分组选择
(global-set-key (kbd "M-u") 'tabbar-backward-group)
(global-set-key (kbd "M-i") 'tabbar-forward-group)

;; === Switch windows ===
; (窗口间切换)
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)


;; === CEDET ===
;; semantic
;折叠和打开整个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C--") 'semantic-tag-folding-fold-all)
;(define-key semantic-tag-folding-mode-map (kbd "C-=") 'semantic-tag-folding-show-all)

;折叠和打开单个buffer的所有代码
;(define-key semantic-tag-folding-mode-map (kbd "C-_") 'semantic-tag-folding-fold-block)
;(define-key semantic-tag-folding-mode-map (kbd "C-+") 'semantic-tag-folding-fold-block)




;; 查看man手册
(global-set-key (kbd "<f1>") 'manual-entry)

;; 显示Emacs info
(global-set-key (kbd "S-<f1>") 'info)

;; f3为查找字符串
(global-set-key (kbd "C-6") 'grep-find)

;;speedbar快捷键
(global-set-key (kbd "C-0") 'speedbar-get-focus)

;;显示/隐藏工具栏，方便调试
(global-set-key (kbd "C-9") 'tool-bar-mode)

;;gdb调试
(global-set-key (kbd "C-4") 'gdb)

; 跳转到函数定义
(global-set-key (kbd "C-.") 'semantic-ia-fast-jump)

; 跳转到函数声明
;(global-set-key (kbd "C-,"))

; 跳转到上次位置
; C-x b
;(global-set-key (kbd "C-,") ')

;设置C-F12快速查看日程安排
;(global-set-key (kbd "C-<f12>") 'list-bookmarks)

; (启用透明背景)
(global-set-key (kbd "s-<f8>") 'loop-alpha)
