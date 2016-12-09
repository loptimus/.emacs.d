;;;=================================loptimus' baseConfig=================================

; Disable Startup display (关闭启动画面)
(setq inhibit-startup-message t)

;; 最近访问过的文件和最近 修改过的文件
;(add-hook 'after-init-hook 'session-initialize)

; y/n instead of yes/no (用y/n代替yes/no)
(fset 'yes-or-no-p 'y-or-n-p)

;;; 编码
;(set-default buffer-file-coding-system 'utf-8-unix)
;(set-default-coding-systems 'utf-8-unix)
;;设置默认读入文件编码
(prefer-coding-system 'utf-8-unix)
;;设置写入文件编码
(setq default-buffer-file-coding-system 'utf-8-unix)

; Display date and time (显示时间)
(display-time-mode t)
(setq display-time-24hr-format  t)
(setq display-time-day-and-date  t)
(setq display-time-format  "%a(%V) %m.%d/%H:%M")
(display-time)

;; 保存上次打开的文件记录
;(load "desktop")
;(desktop-load-default)
;(desktop-read)

; (使用4个空格代替Tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq indent-line-function 'insert-tab)

;; Disable backup file (不要生成临时文件)
(setq-default make-backup-files nil)

; Display line number (显示行号)
(require 'linum)
(global-linum-mode t)

; Display line number (使用web-line显示行号)
;(set-scroll-bar-mode nil)
;(require 'wb-line-number)
;(wb-line-number-toggle)

; Set (设置行距)
;(setq defaule-line-spaceing 4)

; Set (页宽)
(setq default-fill-column 80)

;括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线
(mouse-avoidance-mode 'animate)

;; 高亮显示区域
;(transient-mark-mode t)

;; 闪屏警报
;(setq visible-bell t)

;; 取消提示音
(setq ring-bell-function 'ignore)

;; 直接打开和显示图片
(auto-image-file-mode)

;; 隐藏滚动条
(scroll-bar-mode nil)

;; 锁定行高
;(setq resize-mini-windows nil)

;恢复功能
;(add-to-list 'load-path "~/.emacs.d/baseConf/session/lisp")
;(require 'session)
;(add-hook 'after-init-book 'session-initialize)

;; 保存时，删除行尾空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'base-init)
