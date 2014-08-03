;;;=================================loptimus' .emacs=================================

;(add-to-list 'load-path (expand-file-name "~/.emacs.d"))


;;;=================================Emacs base configure=================================
(add-to-list 'load-path "~/.emacs.d/base_config/")
(require 'base_config)




;;;=================================Emacs common IDE configure=================================
(add-to-list 'load-path "~/.emacs.d/commonIDE")
(require 'commonIDE_config)



;;;=================================Erlang configure=================================
(add-to-list 'load-path "~/.emacs.d/erlIDE/")
(require 'erlang_config)


;;;=================================Cpp configure=================================
(add-to-list 'load-path "~/.emacs.d/cppIDE/")
(require 'cpp_config)


;;;=================================PHP configure======================================
;(add-to-list 'load-path "~/.emacs.d/phpIDE/")
;(require 'php_config)





;;;=================================Stack trace on error======================================
(setq stack-trace-on-error t)


