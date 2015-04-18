;; Web config
(provide 'web_conf)

;; js2
(add-to-list 'load-path (concat webDevConfPath "/js2"))
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; web Mode
(add-to-list 'load-path (concat webDevConfPath "/webMode"))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
