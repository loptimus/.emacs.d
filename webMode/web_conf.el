;; Web config
(provide 'web_conf)

;; js2
(add-to-list 'load-path (concat webModePath "/js2"))
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
