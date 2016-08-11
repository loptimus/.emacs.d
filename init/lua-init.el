;; Lua

(defun lua()
  (add-to-list 'load-path (concat luaPath "/luaMode"))
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
