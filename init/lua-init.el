;;; package --- lua
;;; Commentary:

;;; Code:
(defun lua()
  ;; (add-to-list 'load-path (concat luaPath "/luaMode"))
  (sanityinc/add-subdirs-to-load-path
   (expand-file-name "lisp/lua" user-emacs-directory))
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (require 'lua-block)
  (lua-block-mode t)

  (require 'flymake-lua)
  (add-hook 'lua-mode-hook 'flymake-lua-load)
)

(defun lua-init()
  (lua)
)

(provide 'lua-init)
;;; lua-init.el ends here
