;;; utils

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

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
  (when (memq major-mode '(c-mode c++-mode erlang-mode php-mode python-mode lua-mode))
    (toggle-read-only 1)))
(add-hook 'find-file-hooks 'make-some-files-read-only)

;; 加载插件
(defun load-require (feature)
      "手动加载指定插件 M-x load-require"
      (interactive "s请输入要加载的插件：")
      (if (featurep 'feature)
        (message "%s:require done" feature)
       (require 'feature)
      )
)

;; 半透明设置
(setq alpha-list '((75 55) (100 100)))
(defun loop-alpha ()
  (interactive)
  (let ((h (car alpha-list)))
	((lambda (a ab)
	   (set-frame-parameter (selected-frame) 'alpha (list a ab))
	   (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
	   ) (car h) (car (cdr h)))
	(setq alpha-list (cdr (append alpha-list (list h))))
	)
)

(provide 'utils-init)
