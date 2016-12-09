;;; ======================================ac-complete=================================

(defun auto-complete-init ()
  ""
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "dict")
  (ac-config-default)

  (setq-default ac-sources
                '(ac-source-filename
                  ac-source-functions
                  ac-source-yasnippet
                  ac-source-variables
                  ac-source-symbols
                  ac-source-features
                  ac-source-abbrev
                  ac-source-words-in-same-mode-buffers
                  ac-source-dictionary)
                )
)

(provide 'auto-complete-init)
