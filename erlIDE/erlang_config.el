;;;=================================loptimus' erlang_config=================================

;;
(provide 'erlang_config)



;; ===============================Erlang mode====================================

(setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.7/emacs" load-path))
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(setq erlang-man-root-dir "/usr/local/lib/erlang/man/")
(require 'erlang-start)

;; ===============================Erlang mode====================================



;; ==============================Distel===========================================

;(add-to-list 'load-path "~/.emacs.d/erlIDE/distel-4.03/elisp")
;(require 'distel)
;(distel-setup)

(let ((distel-dir "~/.emacs.d/erlIDE/distel-4.03/elisp"))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)

;; ==============================Distel===========================================



;; ===================Some Erlang customizations=========================================
(add-hook 'erlang-mode-hook
  (lambda ()
  ;; when starting an Erlang shell in Emacs, default in the node name
    (setq inferior-erlang-machine-options '("-sname" "emacs"))
    ;; add Erlang functions to an imenu menu
    (imenu-add-to-menubar "imenu")))


;; prevent annoying hang-on-compile
;(defvar inferior-erlang-prompt-timeout t)

;; tell distel to default to that node
;(setq erl-nodename-cache
;      (make-symbol
;       (concat
;        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
;        (car (split-string (shell-command-to-string "hostname"))))))




	
;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete) 
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind) 
    ("\M-*"      erl-find-source-unwind) 
    )
  "Additional keys to bind when in Erlang shell.")
  
(add-hook 'erlang-shell-mode-hook
   (lambda ()
     ;; add some Distel bindings to the Erlang shell
     (dolist (spec distel-shell-keys)
       (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
;; ==================Some Erlang customizations=================================================






;;===========================================FlymakeErlang语法检查=========================

;; 官方自带的，在erlang安装目录下/lib/tools<版本号>/emacs下
;(require 'erlang-flymake)


;; http://www.emacswiki.org/emacs/FlymakeErlang
(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name temp-file
		(file-name-directory buffer-file-name))))
    (list "~/.emacs.d/erlIDE/eflymake/eflymake_linux" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

;;That's all, and you can either enable flymake globally, with the file open hook
(add-hook 'find-file-hook 'flymake-find-file-hook)

;;or you can enable flymake mode only for some modes with corresponding hooks:
;(defun my-erlang-mode-hook ()
;       (flymake-mode 1))
;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
;;=========================================FlymakeErlang=========================================




;; =====================================esense 配置=================================================
;(add-to-list 'load-path "~/.emacs.d/erlIDE/esense-1.12")
;(require 'esense-start)
;(setq esense-indexer-program "~/.emacs.d/erlIDE/esense-1.12/esense.sh")
;; =====================================esense=====================================================








;; ======================================ac-complete=================================

;(add-to-list 'load-path "~/.emacs.d/commonIDE/auto-complete-1.3.1/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/commonIDE/auto-complete-1.3.1/ac-dict")
;(ac-config-default)
;; ======================================ac-complete=================================




;; ================================cedet==========================================================

;;cedet
;(require 'cedet)
;(global-ede-mode t)

;;;;  Helper tools.
;(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
; '(ecb-options-version "2.40")
; '(semantic-default-submodes (quote (global-semantic-decoration-mode global-semantic-idle-completions-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode global-semantic-mru-bookmark-mode)))
; '(semantic-idle-scheduler-idle-time 3))
;(semantic-mode)

;; smart complitions
;(require 'semantic/ia)
;(setq-mode-local c-mode semanticdb-find-default-throttle
;                 '(project unloaded system recursive))
;(setq-mode-local c++-mode semanticdb-find-default-throttle
;                 '(project unloaded system recursive))

;;ecb
;(require 'semantic/analyze)
;(provide 'semantic-analyze)
;(provide 'semantic-ctxt)
;(provide 'semanticdb)
;(provide 'semanticdb-find)
;(provide 'semanticdb-mode)
;(provide 'semantic-load)
;;=========================cedet==================================================




;;===========================ecb==================================================

;;load ecb
;(add-to-list 'load-path "~/.emacs.d/commonIDE/ecb-2.40")
;(require 'ecb)

;;;; 自动启动ecb，并且不显示每日提示
;(setq ecb-auto-activate t
;      ecb-tip-of-the-day nil)

;;;; 各窗口间切换
;(global-set-key [M-left] 'windmove-left)
;(global-set-key [M-right] 'windmove-right)
;(global-set-key [M-up] 'windmove-up)
;(global-set-key [M-down] 'windmove-down)

 ;;;; 隐藏和显示ecb窗口
;(define-key global-map [(control f4)] 'ecb-hide-ecb-windows)
;(define-key global-map [(control f3)] 'ecb-show-ecb-windows)

 ;;;; 使某一ecb窗口最大化
;(define-key global-map "/C-c1" 'ecb-maximize-window-directories)
;(define-key global-map "/C-c2" 'ecb-maximize-window-sources)
;(define-key global-map "/C-c3" 'ecb-maximize-window-methods)
;(define-key global-map "/C-c4" 'ecb-maximize-window-history)

;;;; 恢复原始窗口布局
;(define-key global-map "/C-c`" 'ecb-restore-default-window-sizes)

;; =======================ecb=====================================================



;; ======================================yasnippet=================================

;;load yasnippet
;(add-to-list 'load-path "~/.emacs.d/commonIDE/yasnippet")
;(require 'yasnippet)
;(setq yas/snippet-dirs '("~/.emacs.d/commonIDE/yasnippet/snippets" 
;	"~/.emacs.d/commonIDE/yasnippet/extras/imported"))
;(yas/global-mode 1)

;; ======================================yasnippet =================================




;; =====================================refactorerl ============================
(add-to-list 'load-path "~/.emacs.d/erlIDE/refactorerl-0.9.12.05/lib/referl_ui/emacs")
(require 'refactorerl)

(custom-set-variables
 '(refactorerl-base-path "~/.emacs.d/erlIDE/refactorerl-0.9.12.05/"))


;(add-hook 'erlang-mode-hook 'refactorerl-mode)
;; ======================================refactorerl ============================
