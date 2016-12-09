;;; package --- Emacs Config
;;; Commentary:
;;; Emacs Config

;;; Code:
;;;================================= Info =================================

; Stack trace on error
(setq stack-trace-on-error t)

(defvar init-path (expand-file-name "init" user-emacs-directory))
(defvar lisp-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path init-path)
(add-to-list 'load-path lisp-path)
(defvar theme-path (expand-file-name "themes/monokai-emacs" user-emacs-directory))
(add-to-list 'custom-theme-load-path theme-path)
(require 'benchmarking-init)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-nt*  (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;;================================= Emacs base configure =================================
;;; Custom
(setq keymap-init-file (expand-file-name "keymap-init.el" user-emacs-directory))
(require 'base-init)
(when (file-exists-p keymap-init-file)
  (load keymap-init-file))
(require 'utils-init)  ; 自定义函数
(require 'site-lisp-init) ;; Must come before elpa, as it may provide package.el
(require 'elpa-init)      ;;

(sanityinc/add-subdirs-to-load-path lisp-path)
(require 'themes-init)
(require 'tabbar-init)
(require 'fonts-init)
(require 'powerline-init)
(require 'undo-tree-init)
(require 'auto-complete-init)
(require 'company-init)
(require 'yasnippet-init)
(require 'cscope-init)
(require 'cedet-init)
(require 'ecb-init)
(require 'indent-init)
(require 'ido-init)
(require 'helm-init)
(require 'async-init)
(require 'flymake-init)
(require 'flycheck-init)
(require 'evil-init)

;; Program
(require 'c-init)
(require 'erlang-init)
(require 'php-init)
(require 'lua-init)
(require 'golang-init)
(require 'web-init)

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'local-init nil t)

(after-load 'keymap-init
	(default-keymap) 
)

(provide 'init)
;;; init.el ends here
