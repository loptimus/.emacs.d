;;; package --- Emacs Config
;;; Commentary:
;;; Emacs Config

;;; Code:
;;;================================= Info =================================
(setq user-full-name "username")
(setq user-mail-address "email")

; Stack trace on error
(setq stack-trace-on-error t)

(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes/monokai-emacs" user-emacs-directory))
(require 'benchmarking-init)

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-nt*  (eq system-type 'windows-nt))

;;----------------------------------------------------------------------------
;; Temporarily reduce garbage collection during startup
;;----------------------------------------------------------------------------
(defconst sanityinc/initial-gc-cons-threshold gc-cons-threshold
  "Initial value of `gc-cons-threshold' at start-up time.")
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold sanityinc/initial-gc-cons-threshold)))

;;;================================= Emacs base configure =================================
;;; Custom
(require 'utils-init)  ; 自定义函数
(require 'base-init)
(require 'keymap-init)  ; Keymap 快捷键配置
(require 'site-lisp-init) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'elpa-init)      ;;


(sanityinc/add-subdirs-to-load-path
 (expand-file-name "lisp/" user-emacs-directory))
(require 'themes-init)
(require 'tarbar-init)
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

(themes-init)
(fonts-init)
(tarbar-init)
(powerline-init)
(undo-tree-init)
(ido-init)
(async-init)
(helm-init)
;(evil-init)
;(auto-complete-init)
(company-init)
(flymake-init)
;(flycheck-init)
(erlang-init)
(lua-init)
(php-init)

;; 启用自定义快捷键
(default_keymap)
(utils_keymap)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)
;;; init.el ends here
