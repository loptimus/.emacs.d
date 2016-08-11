;; Cygwin


(defun set-cygwin ()
  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; 使用指定的shell
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt nil t)
  (setq explicit-shell-file-name "zsh.exe")
  ;; For subprocesses invoked via the shell
  ;; (e.g., “shell -c command”)
  (setq shell-file-name explicit-shell-file-name)

;;;;;;;;;;;;;;;;;;;;;;
;;CygForTheWin
;;*cygwin
;; (when (equal system-type 'windows-nt)
;; (message "Setting up Cygwin...")
;; (let* ((cygwin-root "c:")
;;        (cygwin-bin (concat cygwin-root "/bin"))
;;        (gambit-bin "/usr/local/Gambit-C/4.0b22/bin/")
;;        (snow-bin "/usr/local/snow/current/bin")
;;        (mysql-bin "/wamp/bin/mysql/mysql5.0.51a/bin/"))
;;    (setenv "PATH" (concat cygwin-bin ";" ;
;;                           snow-bin ";"
;;                           gambit-bin ";"
;;                           mysql-bin ";"
;;                           "c:/usr/local/jdk1.60_03/bin/"
;;                           ".;")
;;            (getenv "PATH"))
;;    (setq exec-path (cons cygwin-bin exec-path)))

;; (require 'cygwin-mount)
;; (cygwin-mount-activate)

;; (setq shell-file-name "bash")
;; (setq explicit-shell-file-name "bash")

;; (defun jonnay-cygwin-shell ()
;;   "Wrapper around cygwin-shell so that it doesn't throw an error"
;;   (interactive)
;;   (condition-case e
;;    (cygwin-shell)
;;    (message "There was an error trying to launch the shell: %s" e)))

;; (message "Setting up Cygwin...Done")


;; ;; found from the manual, check, use and make go?
;;  (defun my-shell-setup ()
;;    "For Cygwin bash under Emacs 20"
;;    (setq comint-scroll-show-maximum-output 'this)
;;    (setq comint-completion-addsuffix t)
;;    (setq comint-eol-on-send t)
;;    (setq w32-quote-process-args ?\")
  ;;    (make-variable-buffer-local 'comint-completion-addsuffix))

;; (setq shell-mode-hook 'my-shell-setup)
  ;; (add-hook 'emacs-startup-hook 'jonnay-cygwin-shell)
  ;; )
)

(defun cygwin-init ()
  (when *is-a-nt*
    ;; 设置Cygwin路径
    (setenv "PATH" (concat "D:/cygwin/bin;" (getenv "PATH")))
    (setq exec-path (cons "D:/cygwin/bin" exec-path))
    (set-cygwin)
  )
)

(provide 'cygwin-init)
