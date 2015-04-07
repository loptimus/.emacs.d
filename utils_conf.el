;;;================================= Custom ======================================
;;; 自定义函数
(provide 'utils_conf)

;;; 编码
;(set-default buffer-file-coding-system 'utf-8-unix)
;(set-default-coding-systems 'utf-8-unix)
;;设置默认读入文件编码
(prefer-coding-system 'utf-8-unix)
;;设置写入文件编码
(setq default-buffer-file-coding-system 'utf-8-unix)

;;; 行复制
(defun copy-lines(&optional arg) 
  (interactive "p") 
  (save-excursion 
    (beginning-of-line) 
    (set-mark (point)) 
    (if arg 
        (next-line (- arg 1))) 
    (end-of-line) 
    (kill-ring-save (mark) (point)) 
    ) 
  )

;;; Default only Read （默认指定为只读模式）
(defun make-some-files-read-only ()
  "when file opened is of a certain mode, make it read only"
  (when (memq major-mode '(c-mode c++-mode erlang-mode php-mode python-mode shell-script-mode emacs-lisp-mode))
    (toggle-read-only 1)))
(add-hook 'find-file-hooks 'make-some-files-read-only)

