;; ===============================Erlang mode====================================

(defun set-erlang()
(setq load-path (cons erlangEmacsPath load-path))
(setq erlang-root-dir erlangPath)
(setq exec-path (cons (concat erlangPath "/bin") exec-path))
(setq erlang-man-root-dir (concat erlangPath "/man"))
(require 'erlang-start)
(set-distel)
)

;; ==============================Distel===========================================

;(add-to-list 'load-path "~/.emacs.d/erlIDE/distel-4.03/elisp")
;(require 'distel)
;(distel-setup)

;; Erlang节点名
(defun erl-set-nodename (name)
  "设置Erlang节点名"
  (interactive "s请输入节点名：")
  (setq inferior-erlang-machine-options (list "-sname" name))
)

(defun set-distel()
(let ((distel-dir distelPath))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))
(require 'distel)
(distel-setup)
)

;; ===================Erlang customizations=========================================
(add-hook 'erlang-mode-hook
  (lambda ()
    ;; when starting an Erlang shell in Emacs, default in the node name
    ; (if (nil (boundp 'inferior-erlang-machine-options))
    ;    (message "true")
    ;  ((message (buffer-file-name)))
    ;  )
    ;;(setq inferior-erlang-machine-options '("-sname" "emacs"))
    ;; add Erlang functions to an imenu menu
    (imenu-add-to-menubar "imenu")))

;(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))
;; prevent annoying hang-on-compile
;(defvar inferior-erlang-prompt-timeout t)

;; tell distel to default to that node
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")
;
(add-hook 'erlang-shell-mode-hook
   (lambda ()
     ;; add some Distel bindings to the Erlang shell
     (dolist (spec distel-shell-keys)
       (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;;========================= FlymakeErlang语法检查 =========================

;; 官方自带的，在erlang安装目录下/lib/tools<版本号>/emacs下
(defun  my-erlang-include-dirs ()
 (list
    "inc"
    "../inc"
    "../../inc"
    "../../../inc"
    "include"
    "../include"
    "../../include"
    "../../../include"
 )
)
(defun  my-erlang-code-dirs ()
 (list
    "../ebin"
    "../../ebin"
    "../../../ebin"
 )
)

;; 另一种方式：http://www.emacswiki.org/emacs/FlymakeErlang
;(require 'flymake)
;(defun flymake-erlang-init ()
;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;		     'flymake-create-temp-inplace))
;	 (local-file (file-relative-name temp-file
;		(file-name-directory buffer-file-name))))
;    (list "~/.emacs.d/erlIDE/eflymake/eflymake_linux" (list local-file))))
;(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))

;;That's all, and you can either enable flymake globally, with the file open hook
;(add-hook 'find-file-hook 'flymake-find-file-hook)

;;or you can enable flymake mode only for some modes with corresponding hooks:
;(defun my-erlang-mode-hook () (flymake-mode 1))
;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; =====================================esense 配置=================================================
;(add-to-list 'load-path esensePath)
;(require 'esense-start)
;(message esensePath)
;(setq esense-indexer-program (concat esensePath "/esense.sh"))

;; =====================================refactorerl ============================
;(add-to-list 'load-path (concat refactorerlPath "/lib/referl_ui/emacs"))
;(custom-set-variables '(refactorerl-base-path refactorerlPath))
;(require 'refactorerl)
;(add-hook 'erlang-mode-hook 'refactorerl-mode)

;; ====================================== wrangler ============================
;(add-to-list 'load-path (concat wranglerPath "/elisp"))
;(setq exec-path (cons (concat wranglerPath "/bin") exec-path))
;(require 'wrangler)

(defun erlang-init ()
  ;; Erlang
  (defvar erlangPath "/usr/local/Cellar/erlang/17.5")
  (defvar erlangEmacsPath "~/.emacs.d/lisp/erlang/emacs")
  ;; Distel
  (defvar distelPath "~/.emacs.d/lisp/erlang/distel-4.03/elisp")
  ;; Esense
  ;;(defvar esensePath "~/.emacs.d/erlangConf/esense-1.12")
  ;; Refactorerl
  ;; (defvar refactorerlPath "~/.emacs.d/erlangConf/refactorerl-0.9.12.05")
  ;; Wrangler
  ;; (defvar wranglerPath "~/.emacs.d/erlangConf/Wrangler")
  (set-erlang)
  (require 'erlang-flymake)
  (setq erlang-flymake-get-include-dirs-function 'my-erlang-include-dirs)
  (setq erlang-flymake-get-code-path-dirs-function 'my-erlang-code-dirs)
  (erlang-flymake-only-on-save)
)

(provide 'erlang-init)
