
(provide 'web_conf)

(add-to-list 'load-path (concat webModePath "/js2"))
(require 'js2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
