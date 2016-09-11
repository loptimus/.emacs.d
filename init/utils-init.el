;;; utils

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


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
  (when (memq major-mode '(c-mode c++-mode erlang-mode php-mode python-mode shell-script-mode))
    (toggle-read-only 1)))
(add-hook 'find-file-hooks 'make-some-files-read-only)

 ;; 加载插件
(defun load-plugin (plugin)
      "手动加载指定插件 M-x load-plugin"
      (interactive "s请输入要加载的插件：")
      (cond
       ((string-equal plugin "ac") (ac))
       ((string-equal plugin "company") (company))
       ((string-equal plugin "pl") (powerline))
       ((string-equal plugin "cedet") (cdt))
       ((string-equal plugin "ecb") (ecb))
       ((string-equal plugin "org") (org))
       ((string-equal plugin "php") (php))
       ((string-equal plugin "cscope") (cscope))
       ((string-equal plugin "yas") (yas))
       ((string-equal plugin "pl") (powerline))
       ((string-equal plugin "undo-tree") (undo-tree))
       ((string-equal plugin "evil") (evil))
       (t (message "没有找到插件：%s" plugin))
      )
)

(provide 'utils-init)
