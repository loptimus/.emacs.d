;;;=================================loptimus' erlang_config=================================

;;
(provide 'erlang_config)

;(print erlangEmacsPath)
;(print distelPath)
;(print erlangPath)

;; ===============================Erlang mode====================================

(setq load-path (cons erlangEmacsPath load-path))
(setq erlang-root-dir erlangPath)
(setq exec-path (cons (concat erlangPath "/bin") exec-path))
(setq erlang-man-root-dir (concat erlangPath "/man"))
(require 'erlang-start)

;; ===============================Erlang mode====================================



;; ==============================Distel===========================================

;(add-to-list 'load-path "~/.emacs.d/erlIDE/distel-4.03/elisp")
;(require 'distel)
;(distel-setup)

(let ((distel-dir distelPath))
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
(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs@"
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
        (car (split-string (shell-command-to-string "hostname"))))))




	
;; A number of the erlang-extended-mode key bindings are useful in the shell too
;(defconst distel-shell-keys
;  '(("\C-\M-i"   erl-complete)
;    ("\M-?"      erl-complete) 
;    ("\M-."      erl-find-source-under-point)
;    ("\M-,"      erl-find-source-unwind) 
;    ("\M-*"      erl-find-source-unwind) 
;    )
;  "Additional keys to bind when in Erlang shell.")
;  
(add-hook 'erlang-shell-mode-hook
   (lambda ()
     ;; add some Distel bindings to the Erlang shell
     (dolist (spec distel-shell-keys)
       (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
;; ==================Some Erlang customizations=================================================






;;===========================================FlymakeErlang语法检查=========================

;; 官方自带的，在erlang安装目录下/lib/tools<版本号>/emacs下
(require 'erlang-flymake)


;(defun get-erlang-app-dir ()
;   (let* ((src-path (file-name-directory (buffer-file-name)))
;               (pos (string-match "/src/" src-path)))
;        (if pos
;             (substring src-path 0 (+ 1 pos))
;                   src-path)))
;(setq erlang-flymake-get-code-path-dirs-function
;       (lambda ()
;            (concat (get-erlang-app-dir) "ebin")))
;(defun erlang-flymake-self-get-include-dirs ()
;  (list (concat (erlang-flymake-get-app-dir) "inc")
;        (concat (erlang-flymake-get-app-dir) "../inc")
;        (concat (erlang-flymake-get-app-dir) "../../inc")
;  )
;)
;(setq erlang-flymake-get-include-dirs-function
;      'erlang-flymake-self-get-include-dirs)

;(setq erlang-flymake-get-code-include-dirs-function
;      (lambda ()
;        (concat (get-erlang-app-dir) "inc")
;        (concat (get-erlang-app-dir) "../inc")
;        (concat (get-erlang-app-dir) "../../inc")
;        ))
;(require 'erlang-flymake)


;; http://www.emacswiki.org/emacs/FlymakeErlang
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
;(defun my-erlang-mode-hook ()
;       (flymake-mode 1))
;(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
;;=========================================FlymakeErlang=========================================




;; =====================================esense 配置=================================================
;(add-to-list 'load-path "~/.emacs.d/erlIDE/esense-1.12")
;(require 'esense-start)
;(setq esense-indexer-program "~/.emacs.d/erlIDE/esense-1.12/esense.sh")
;; =====================================esense=====================================================


;; =====================================refactorerl ============================
;(add-to-list 'load-path (concat refactorerlPath "/lib/referl_ui/emacs"))
;(require 'refactorerl)

;(custom-set-variables '(refactorerl-base-path refactorerlPath))


;(add-hook 'erlang-mode-hook 'refactorerl-mode)
;; ======================================refactorerl ============================


;; ====================================== wrangler ============================
;(add-to-list 'load-path (concat wranglerPath "/elisp")) 
;(require 'wrangler)
