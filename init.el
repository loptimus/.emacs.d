;;;================================= loptimus' .emacs =================================
; Environment variable
;(setenv "Environment Variable Name" "Environment Variable Value")
;(defvar Variable (getenv "Environment Variable Name"))

; Stack trace on error
(setq stack-trace-on-error t)

; Default directory
;(defvar workspace "D:/workspace")
;(setq default-directory workspace)
;(cd workspace)

;(setq exec-path (cons "/usr/local/bin" exec-path)) 
;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-nt*  (eq system-type 'windows-nt))

;;;================================= Info =================================
(setq user-full-name "liwl")
(setq user-mail-address "liwl@syg.com")

;(setenv "PATH" (concat (getenv "PATH") ":~/.emacs.d/commonIDE/cscope/bin"))
;(setq tags-file-name "/root/etags/ERL_LIB_TAGS")

;;;================================= Emacs base configure =================================
;;; Custom
(load-library "~/.emacs.d/utils_conf.el")
(load-library "~/.emacs.d/keymap_conf.el")
(require 'utils_conf)  ; 自定义函数
(require 'keymap_conf)  ; Keymap 快捷键配置
(add-to-list 'load-path "~/.emacs.d/baseConf")
(require 'base_conf)

;;;================================= Emacs common IDE configure =================================
(defvar commonConfPath "~/.emacs.d/commonConf")
(add-to-list 'load-path commonConfPath)
(defvar autoCompletePath (concat commonConfPath "/auto-complete-1.3.1"))
(defvar companyPath "~/.emacs.d/commonConf/companyMode")
(defvar cedetPath (concat commonConfPath "/cedet-1.1"))
(defvar ecbPath (concat commonConfPath "/ecb-2.40"))
(defvar yasnippetPath (concat commonConfPath "/yasnippet"))
(defvar cscopePath "~/.emacs.d/commonConf/cscope")
(defvar indentPath "~/.emacs.d/commonConf/Indent")
(defvar helmPath "~/.emacs.d/commonConf/helm")
(defvar powerlinePath "~/.emacs.d/commonConf/powerline")
(require 'common_conf)

;;;================================= orgMode configure =================================
(defun org () "Load org-mode"
  (defvar orgModePath "~/.emacs.d/orgMode")
  (add-to-list 'load-path orgModePath)
  (require 'org-install)
)

;;;================================= Erlang configure =================================
(add-to-list 'load-path "~/.emacs.d/erlangConf/")
;; Erlang
(defvar erlangPath "D:/Program Files/erl6.0")
(defvar erlangEmacsPath "~/.emacs.d/erlangConf/emacs")
;; Distel
(defvar distelPath "~/.emacs.d/erlangConf/distel-4.03/elisp")
;; Esense
;;(defvar esensePath "~/.emacs.d/erlangConf/esense-1.12")
;; Refactorerl
;(defvar refactorerlPath "~/.emacs.d/erlangConf/refactorerl-0.9.12.05")
;; Wrangler
(defvar wranglerPath "~/.emacs.d/erlangConf/Wrangler")
(require 'erlang_conf)

;;;================================= C/Cpp configure =================================
(add-to-list 'load-path "~/.emacs.d/cppConf/")
(require 'cpp_conf)

;;;================================= PHP configure ======================================
(defvar phpPath "~/.emacs.d/phpConf")
(add-to-list 'load-path phpPath)
(require 'php_conf)

;;;================================= Lua configure ======================================
(defvar luaPath "~/.emacs.d/luaConf")
(add-to-list 'load-path luaPath)
(require 'lua_conf)

;;;================================= webMode configure ======================================
(defvar webDevConfPath "~/.emacs.d/webDevConf")
(add-to-list 'load-path webDevConfPath)
(require 'web_conf)


 ;; Erlang节点名
(defun erl-set-nodename (name)
      "设置Erlang节点名"
      (interactive "s请输入节点名：")
      (setq inferior-erlang-machine-options (list "-sname" name))
)

 ;; 加载插件
(defun load-plugin (plugin)
      "手动加载指定插件 M-x load-plugin"
      (interactive "s请输入要加载的插件：")
      (cond
       ((string-equal plugin "ac") (ac))
       ((string-equal plugin "company") (company))
       ((string-equal plugin "cedet") (cdt))
       ((string-equal plugin "ecb") (ecb))
       ((string-equal plugin "org") (org))
       ((string-equal plugin "php") (php))
       ((string-equal plugin "cscope") (cscope))
       ((string-equal plugin "yas") (yas))
       ((string-equal plugin "undo-tree") (undo-tree))
       (t (message "没有找到插件：%s" plugin))
      )
)

;; 自动加载插件
(defun auto-load-plugin () "启动时自动加载的插件"
  (ac)
  ;(company)
  ;(cdt)
  ;(ecb)
  (undo-tree)
)

;; 调用自动加载函数
(auto-load-plugin)

;; 启用自定义快捷键
(default_keymap)
(utils_keymap)
(set-face-background 'highlight-indentation-face "#e3e3d3")
