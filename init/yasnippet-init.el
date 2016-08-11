;;; ======================================yasnippet=================================
(defun yas () "Load yasnippet"
  ;; Load yasnippet
  (add-to-list 'load-path yasnippetPath)
  (require 'yasnippet)
  (yas/initialize)
  ;;(setq yas/snippet-dirs ((concat yasnippetPath "/snippets") (concat yasnippetPath "/extras/imported")))
  (yas/load-directory (concat yasnippetPath "/snippets"))
  ;;(yas/snippet-dirs (concat yasnippetPath "/snippets"))
  (yas/global-mode 1)
)

(defun yasnippet-init()
  (yas)
)

(provide 'yasnippet-init)
