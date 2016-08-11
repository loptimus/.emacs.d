;; Fonts

;; 判断某个字体是否在系统中是否安装
(defun qiang-font-existsp (font)
  (if(null(x-list-fonts font))
      nil t))

;;(defun qiang-font-existsp (font)
;; (if(null(find-font (font-spec :name font)))
;;  nil t))

;; 用来产生带上font size信息的font描述文本
(defun qiang-make-font-string (font-name font-size)
  (if(and(stringp font-size)
         (equal":"(string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

;; 自动设置字体函数
(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl) ;; for find if
  (let((en-font (qiang-make-font-string
                 (find-if #'qiang-font-existsp english-fonts)
                 english-font-size))
       (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                           :size chinese-font-size)))
    ;; Set the default English font
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist(charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font)
      ))
  ;;(set-face-attribute
  ;; 'default nil :font zh-font)
  ;;)
  )

;; 字体设置
(defun fonts-init
    (qiang-set-font
     '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") 14 '("Microsoft Yahei" "Kaiti SC" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体")
     )
)

(provide 'fonts-init)
