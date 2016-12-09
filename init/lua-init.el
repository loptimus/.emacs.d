;;; package --- lua
;;; Commentary:

;;; Code:
(sanityinc/add-subdirs-to-load-path
   (expand-file-name "lisp/lua" user-emacs-directory))
(require 'lua-mode)
;(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
;(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

(defun my-lua-hook () 
  ""
  (require 'lua-block)
  (lua-block-mode t)
  (when (featurep 'flymake)
    (require 'flymake-lua)
    (flymake-lua-load)
  )
)

(add-hook 'lua-mode-hook 'my-lua-hook)

(provide 'lua-init)
;;; lua-init.el ends here
