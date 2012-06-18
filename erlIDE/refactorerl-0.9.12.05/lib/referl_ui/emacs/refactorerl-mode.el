;; -*- coding: utf-8 -*-

;; The  contents of this  file are  subject to  the Erlang  Public License,
;; Version  1.1, (the  "License");  you may  not  use this  file except  in
;; compliance  with the License.  You should  have received  a copy  of the
;; Erlang  Public License  along  with this  software.  If not,  it can  be
;; retrieved at http://plc.inf.elte.hu/erlang/
;;
;; Software  distributed under  the License  is distributed  on an  "AS IS"
;; basis, WITHOUT  WARRANTY OF ANY  KIND, either expressed or  implied. See
;; the License  for the specific language governing  rights and limitations
;; under the License.
;;
;; The Original Code is RefactorErl.
;;
;; The Initial Developer of the  Original Code is Eötvös Loránd University.
;; Portions created  by Eötvös  Loránd University are  Copyright 2008-2009,
;; Eötvös Loránd University. All Rights Reserved.

(provide 'refactorerl-mode)
(require 'refactorerl-operations)
(require 'refactorerl-query)
; (require 'refactorerl-metrics)
(require 'refactorerl-flasher)
(eval-when-compile
  (require 'cl))

(require 'refactorerl-custom-undo-mode)

;; Minor mode definition

(setf refactorerl-mode-keybindings
      (append refactorerl-mode-keybindings
              '(("Q"    refactorerl-quit)
                ("S"    refactorerl-restart)
                ("a"    refactorerl-add-file)
                ("d"    refactorerl-drop-file)
                ("D"    refactorerl-debug-shell)

;                ("m1"    refactorerl-start-metrics)
;                ("m2"    refactorerl-stop-metrics)

                ("G"    refactorerl-draw-graph)
                ("L"    refactorerl-load-dir)
                ("C"    refactorerl-list-files)
                ("\C-u" refactorerl-update-status)
                ("ct"   refactorerl-cluster-agglom)
                ("cg"   refactorerl-cluster-genetic)
                ("U"    refactorerl-buffer-undo)
                ("R"    refactorerl-buffer-redo)
                ("TT"   refactorerl-toggle-test))))

(flet ((define-keybindings (bindings prefix)
         (loop for (suffix fun) in bindings
               do (let ((key (if (listp suffix)
                                 (car suffix)
                               (concat prefix suffix))))
                    (define-key refactorerl-mode-map key fun)))))

  (define-keybindings refactorerl-mode-keybindings "\C-c\C-r"))

(defun* menu-from-group (group-name &key enable)
  (let ((menu-group (cdr (assq group-name refactorerl-menu-groups))))
    (unless menu-group
      (error "Menu group %s not defined" group-name))

    (cons `(,(menu-group-label menu-group) :enable ,enable)
          (menu-group-children menu-group))))

(setf refactorerl-mode-menu
      `((("Server")
         ("Start server" refactorerl-restart :enable (lambda () (not (refac-server-is-running))))
         ("Stop server" refactorerl-quit :enable :server)
         "--"
         ("Start metrics" refactorerl-start-metrics :enable :server)
         ("Stop metrics" refactorerl-stop-metrics :enable :server)
         "--"
         ("Configure" refactorerl-configure)
         ("Select control buffer" refactorerl-control-buffer :enable :server))
        (("Files")
         ("Load directory" refactorerl-load-dir :enable :server)
         "--"
         ("Database contents" refactorerl-list-files :enable :server)
;; TODO         ("Show file details" refactorerl-server-show-files :enable :server)
         ("Show parse errors" refactorerl-show-errors :enable :server)
         ("Draw graph" refactorerl-draw-graph :enable :server)
         "--"
         ("Reset database"  refactorerl-server-reset-db :enable :server)
         )
        "--"
        ("Add file" refactorerl-add-file :enable :buffer-file)
        ("Drop file" refactorerl-drop-file :enable :buffer-file)
        ("Update status" refactorerl-update-status :enable :buffer-file)
		("Undo (one step only)" refactorerl-undo :help "Steps back on the database" :enable (lambda () (not refactorerl-custom-undo-mode)))
        (("Undo")
		 ("last change" refactorerl-buffer-undo :enable 
		 (lambda () refac-undo-available))
		 "--"
		)
		(("Redo")
		 ("last change" refactorerl-buffer-redo :enable 
		 (lambda () refac-redo-available))
		 "--"
		)
        "--"
        (("Module clustering")
         ("Agglomerative" refactorerl-cluster-agglom :enable :server)
         ("Genetic" refactorerl-cluster-genetic :enable :server))
        ("Function clustering" refactorerl-cluster-function :enable :server)
        "--"
        ,(menu-from-group 'query)
;        ,(menu-from-group 'metrics)
        "--"
        ,(menu-from-group 'func)
        ,(menu-from-group 'introelim)
        ,(menu-from-group 'move)
        ,(menu-from-group 'rename)
        ,(menu-from-group 'upgrade)))

(defun refactorerl-expand-enabler (enabler)
  (case enabler
    ((:buffer-state)
     `(and (refac-server-is-running) (eq refac-buffer-state :ok)))
    ((:buffer-file)
     `buffer-file-name)
    ((:server)
     `(refac-server-is-running))
    ((:wrangler)
     `(and (not (eq refactorerl-wrangler-path 'disabled))))
    (otherwise
     (list enabler))))

(defun* refactorerl-easy-menu (name items &key enable)
  `(,name
    ,@(when (and (not (featurep 'xemacs)) enable) `(:active ,(refactorerl-expand-enabler enable)))
    ,@(loop for menu-item in items
            collect (if (consp menu-item)
                        (if (stringp (car menu-item))
                            (destructuring-bind (label callback &key enable help) menu-item
                              `[,label ,callback :active ,(if enable
                                                              (refactorerl-expand-enabler enable)
                                                            't)])
                          (destructuring-bind ((label &key enable help) &body items) menu-item
                            (refactorerl-easy-menu label items :enable enable)))
                      menu-item))))

(easy-menu-define refactorerl-mode-easymenu refactorerl-mode-map "Refactor menu"
  (refactorerl-easy-menu "Refactor" refactorerl-mode-menu))

(defvar refac-buffer-state nil
  "Status of the file:
 - `:off': not part of the active refactoring set
 - `:err': there is an error in the file
 - `:ok': ready for refactoring
 - nil: unknown state (e.g. during parsing)")

;(defvar refac-buffer-opened nil)
;(defvar refac-undo-available nil)
;(defvar refac-redo-available nil)

(defvar refac-buffer-state-is-off nil)
(defvar refac-buffer-state-is-err nil)
(defvar refac-buffer-state-is-ok nil)
(defvar refac-buffer-state-is-nil nil)
(defvar refac-buffer-state-metrics nil)

(defun refac-set-buffer-state (new-state)
  "refac-buffer-state needs to be set with this function so that
refac-buffer-state-is-* can be kept in sync. This is needed
because in XEmacs, there is no dynamic modeline lighter, and
thus, we have to make do with the very simple conditional
modeline facility it offers (see also the :lighter of
refactorerl-mode)"
  (setf refac-buffer-state new-state)
  (setf refac-buffer-state-is-off (eq new-state :off))
  (setf refac-buffer-state-is-err (eq new-state :err))
  (setf refac-buffer-state-is-ok (eq new-state :ok))
  (setf refac-buffer-state-is-nil (not new-state)))

(defmacro* define-minor-mode* (name doc &key lighter map menu on off)
  (let ((manual-menu-add (featurep 'xemacs)))
    `(define-minor-mode ,name ,doc
       :lighter ,lighter
       :map map
       (if ,name
           (progn
             ,(when manual-menu-add
                    `(easy-menu-add ,menu))
             ,on)
           (progn
             ,(when manual-menu-add
                    `(easy-menu-remove ,menu))
             ,off)))))

(define-minor-mode* refactorerl-mode
  "Minor mode providing access to Erlang Refactorer operations.
\\<refactorerl-mode-map>
The first time this mode is activated the refactorer server is started as a
subprocess. It should be stopped before leaving Emacs (`\\[refactorerl-quit]').

Before using this minor mode, you must customize `refactorerl-base-path'.

Key bindings:
\\{refactorerl-mode-map}
"
  :lighter (" Refact"
            (refac-buffer-state-is-err ":error" "")
            (refac-buffer-state-is-off ":off" "")
            (refac-buffer-state-is-nil ":???" "")
            (refac-buffer-state-metrics " Metrics" ""))
  :map refactorerl-mode-map
  :menu refactorerl-mode-easymenu
  :on (progn
        (make-local-variable 'refac-buffer-state)
        ;; (make-local-variable 'refac-ephemeral-overlays)
        (loop for var in '(refac-buffer-state-is-off
                           refac-buffer-state-is-ok
                           refac-buffer-state-is-err
                           refac-buffer-state-is-nil
                           refac-buffer-state-metrics
						   ;refac-undo-available 
						   ;refac-redo-available 
						   ;refac-buffer-opened
						   )
              do (make-local-variable var))
        (refactorerl-start)
        (refactorerl-update-status)

        (refactorerl-flasher t)

        ;(add-hook 'after-change-functions 'refac-bufsrv-load t t)
        ;(add-hook 'kill-buffer-hook 'after-kill-buffer t t)
        (add-hook 'after-save-hook 'refac-file-saved t t)
        (add-hook 'post-command-hook 'refac-remove-overlays nil t))
  :off (progn
         (refac-remove-stacked-overlays)
         ;(remove-hook 'after-change-functions 'refac-bufsrv-load)
         ;(remove-hook 'kill-buffer-hook 'after-kill-buffer)
		 (when refactorerl-custom-undo-mode
		  (refactorerl-custom-undo-mode))
         (refactorerl-flasher nil)
         (remove-hook 'after-save-hook 'refac-file-saved t)))
