;;;=================================loptimus' baseConfig=================================

(provide 'base_conf)

; Disable Startup display (关闭启动画面)
(setq inhibit-startup-message t)

;; 最近访问过的文件和最近 修改过的文件
(add-hook 'after-init-hook 'session-initialize)

;; 保存上次打开的文件记录
(load "desktop")
(desktop-load-default)
(desktop-read)

; (使用4个空格代替Tab)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;(setq indent-line-function 'insert-tab)

; Disable backup file (不要生成临时文件)
(setq-default make-backup-files nil)

; y/n instead of yes/no (用y/n代替yes/no)
(fset 'yes-or-no-p 'y-or-n-p)

; Display date and time (显示时间)
(display-time-mode 1)
(setq display-time-24hr-format  t)
(setq display-time-day-and-date  t)
(setq display-time-format  "%a(%V) %m.%d/%H:%M")
(display-time)

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
(setq default-fill-column 60)

;括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号
(show-paren-mode)
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

;; 锁定行高
;(setq resize-mini-windows nil)

;; 判断某个字体是否在系统中是否安装
(defun qiang-font-existsp (font)
 (if(null(x-list-fonts font))
  nil t))

;(defun qiang-font-existsp (font)
; (if(null(find-font (font-spec :name font)))
;  nil t))

;; 用来产生带上font size信息的font描述文本
(defun qiang-make-font-string (font-name font-size)
 (if(and(stringp font-size)
     (equal":"(string (elt font-size 0))))
  (format "%s%s" font-name font-size)
  (format "%s %s" font-name font-size)))

;; 自动设置字体函数
(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
 "english-font-size could be set to \":pixelsize=18\" or a integer.If set/leave chinese-font-size to nil, it will follow english-font-size"
 (require 'cl) ;; for find if
 (let((en-font (qiang-make-font-string
                (find-if #'qiang-font-existsp english-fonts)
                english-font-size))
      (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                :size chinese-font-size)))
  ;; Set the default English font
  ;; The following 2 method cannot make the font settig work in new frames.
  ;; (set-default-font "Consolas:pixelsize=18")
  ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
  ;; We have to use set-face-attribute
  (message "Set English Font to %s" en-font)
  (set-face-attribute
   'default nil :font en-font)

  ;; Set Chinese font
  ;; Do not use 'unicode charset, it will cause the english font setting invalid
  (message "Set Chinese Font to %s" zh-font)
  (dolist(charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font)
    charset
    zh-font)
  ))
  ;(set-face-attribute
  ; 'default nil :font zh-font)
  ;)
)

;; 字体设置
(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") 14 '("Microsoft Yahei" "Kaiti SC" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

;; tabbar
(require 'tabbar)
(tabbar-mode t)
; set tabbar
;(set-face-attribute 'tabbar-default-face nil
;                    :family "DejaVu Sans Mono"
;                    :background "gray80"
;                    :foreground "gray30"
;                    :height 1.0
;                    )
;(set-face-attribute 'tabbar-button-face nil
;                    :inherit 'tabbar-default
;                    :box '(:line-width 1 :color "yellow")
; 					)
;(set-face-attribute 'tabbar-selected-face nil
;                    :inherit 'tabbar-default
;                    :foreground "DarkGreen"
;                    :background "LightGoldenrod"
;                    :box '(:line-width 2 :color "Darkgoldenrod")
;                    :overline "black"
;                    :underline "black"
;                    :weight 'bold)
;(set-face-attribute 'tabbar-unselected-face nil :inherit 'tabbar-default :box '(:line-width 2 :color "#00B2BF"))



;恢复功能
(add-to-list 'load-path "~/.emacs.d/baseConf/session/lisp")
(require 'session)
(add-hook 'after-init-book 'session-initialize)



;(require 'glasses)
;;glass
;(global-set-key [(f12)] 'loop-alpha)

;; 半透明设置
(setq alpha-list '((75 55) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
	((lambda (a ab)
	   (set-frame-parameter (selected-frame) 'alpha (list a ab))
	   (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
	   ) (car h) (car (cdr h)))
	(setq alpha-list (cdr (append alpha-list (list h))))
	)
)

;;; ==================== Color Theme =========================

; 启用font-lock-mode，在color-theme中会用到
(global-font-lock-mode t)

(add-to-list 'load-path "~/.emacs.d/baseConf/color-theme-6.6.0/")
(require 'color-theme)
(color-theme-initialize)

;下列任选一种
;(color-theme-oswald)
;(color-theme-charcoal-black)
;(color-theme-deep-blue)
;(color-theme-ahei)
(color-theme-molokai)

;; 保存时，删除行尾空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(when (equal system-type 'windows-nt)
  ;; 设置Cygwin路径
  (setenv "PATH" (concat "D:/cygwin/bin;" (getenv "PATH")))
  (setq exec-path (cons "D:/cygwin/bin" exec-path))
  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; 使用指定的shell
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)
  (setq explicit-shell-file-name "zsh.exe")
  ;; For subprocesses invoked via the shell
  ;; (e.g., “shell -c command”)
  (setq shell-file-name explicit-shell-file-name)

;;;;;;;;;;;;;;;;;;;;;;
;;CygForTheWin
;;*cygwin
;; (when (equal system-type 'windows-nt)
;; (message "Setting up Cygwin...")
;; (let* ((cygwin-root "c:")
;;        (cygwin-bin (concat cygwin-root "/bin"))
;;        (gambit-bin "/usr/local/Gambit-C/4.0b22/bin/")
;;        (snow-bin "/usr/local/snow/current/bin")
;;        (mysql-bin "/wamp/bin/mysql/mysql5.0.51a/bin/"))
;;    (setenv "PATH" (concat cygwin-bin ";" ;
;;                           snow-bin ";"
;;                           gambit-bin ";"
;;                           mysql-bin ";"
;;                           "c:/usr/local/jdk1.60_03/bin/"
;;                           ".;")
;;            (getenv "PATH"))
;;    (setq exec-path (cons cygwin-bin exec-path)))

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; (setq shell-file-name "bash")
;; (setq explicit-shell-file-name "bash")

;; (defun jonnay-cygwin-shell ()
;;   "Wrapper around cygwin-shell so that it doesn't throw an error"
;;   (interactive)
;;   (condition-case e
;;    (cygwin-shell)
;;    (message "There was an error trying to launch the shell: %s" e)))

;; (message "Setting up Cygwin...Done")


;; ;; found from the manual, check, use and make go?
;;  (defun my-shell-setup ()
;;    "For Cygwin bash under Emacs 20"
;;    (setq comint-scroll-show-maximum-output 'this)
;;    (setq comint-completion-addsuffix t)
;;    (setq comint-eol-on-send t)
;;    (setq w32-quote-process-args ?\")
;;    (make-variable-buffer-local 'comint-completion-addsuffix))

;; (setq shell-mode-hook 'my-shell-setup)
;; (add-hook 'emacs-startup-hook 'jonnay-cygwin-shell)
;; )
)
