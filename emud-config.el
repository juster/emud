;; MUD CONFIG BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'facemenu)

(defun emud-config (host-name)
  (interactive
   (list
    (if (boundp 'mud-host-name) mud-host-name
      (read-from-minibuffer "MUD hostname: "))))
  
  (let ( emud-config-buffer )
    (when (get-buffer "*EMUD Config*")
      (kill-buffer "*EMUD Config*"))
    (setq emud-config-buffer (get-buffer-create "*EMUD Config*"))
    (pop-to-buffer emud-config-buffer t)
    (set (make-local-variable 'mud-config-host-name) host-name)
    (set (make-local-variable 'mud-config-settings)
         (get-settings-for-mud mud-config-host-name))
    (let (( default-major-mode 'emud-config-mode ))
      (set-buffer-major-mode emud-config-buffer))))

(defun emud-config-mode ()
  "Major mode editing EMUD settings like triggers and aliases."
  (interactive)
;;  (unless (eq major-mode 'emud-config-mode) (kill-all-local-variables))
;;  (use-local-map emud-config-mode-keymap)
  (setq mode-name "EMUD Config")
  (setq major-mode 'emud-config-mode)

  (emud-config-draw)
  (use-local-map widget-keymap))

(define-widget 'emud-trigger-widget 'group
  "A widget for viewing or editing trigger match text and actions."
  :args '((text)
          ('emud-trigger-action-view)
          (push-button :value "Edit")
          (push-button :format "%[%v%]\n" :value "Delete")))
          
(define-widget 'emud-trigger-actions 'checklist

  "A checklist for listing all the actions triggered by a match.
The standard 'checklist widget does strange things for enabling
buttons so we override the :value-create function, and use what
other parts we can."

  :tag "Actions"
  :format "%t: %v"
  :indent 9
  :greedy nil

  :value-create 'emud-trigger-actions-value-create
  :value-to-internal 'emud-trigger-actions-value-to-internal
  :value-to-external 'emud-trigger-actions-value-to-external
  :value-get 'emud-trigger-actions-value-get

  :args '((emud-trigger-color-pair :tag "Font" :indent 25)
          (emud-trigger-code)
          (item :tag "Ignore"
                :inline t
                :match
                (lambda (widget val)
                  (message "DEBUG ignore:match - val = %s" val)
                  (not (null val))))
          (text :tag "Response" :indent 15 :format "%t %v"
                :inline t
                :match (lambda (widget val)
                         (message "DEBUG response:match - val = %s"
                                  val)
                         (and val (stringp val))))))

(defun emud-trigger-actions-value-create (widget)

  "Sort of like `widget-checklist-value-create', but with our own
simpler match function.  I couldn't understand how things were
matched in the above function or why things were overly complex.

"

  ;; I don't quite understand the difference between match and
  ;; match-inline.

  (let (( values  (widget-get widget :value) )
        ( args    (widget-get widget :args)  )
        ( nextval nil )
        ( nextarg nil ))
    (unless (and (listp values) (listp args)
                 (= (length args) (length values)))
      (error "emud-trigger-actions must be have the number of values as args"))

    (while args
      (setq nextarg (car args))
      (setq nextval (car values))

      (widget-checklist-add-item widget nextarg
                                 (if nextval
                                     (cons nil nextval)
                                   nil))

      (setq args   (cdr args))
      (setq values (cdr values))))

  ;; `widget-checklist-value-create' does this so we might as well, too..?
  (widget-put widget :children (nreverse (widget-get widget :children))))

(defun emud-trigger-actions-value-get (widget)
  (mapcar (lambda (child) (widget-value child))
          (widget-get widget :children)))

(defun emud-trigger-actions-value-to-internal (widget actions)
  (list (plist-get actions :font)
        (plist-get actions :code)
        (plist-get actions :ignore)
        (plist-get actions :respond)))

(defun emud-trigger-actions-value-to-external (widget values)
  (list :font (car values) :code (cadr values) :ignore (caddr values)
        :respond (cadddr values)))

(define-widget 'emud-trigger-code 'push-button
  "A button that edits code if there is any"
  :tag "Code"
  :format "%t %[Edit%]\n"
  :match-inline (lambda (widget new-val)
                  (message "DEBUG code:match-inline - val = %s" new-val)
                  t)
  :match (lambda (widget new-val)
           (message "DEBUG code:match - val = %s" new-val)
           (functionp new-val))

;;            (and (listp new-val)
;;                 (/= 0 (length new-val))
;;                 (symbolp (car new-val))))
  :notify 'emud-trigger-code-button-press)
  
(defun emud-trigger-code-button-press (widget widget-child event)
  (message "DEBUG: emud-trigger-code-button-press"))

(define-widget 'emud-trigger-color-pair 'group
  "A pair of colors representing foreground and background"
  :tag "Font"
  :format "%{%t%} (%{sample%}) %v"
  :inline t
  :value-to-internal 'emud-trigger-color-pair-value-to-internal
  :value-to-external 'emud-trigger-color-pair-value-to-external
  :sample-face-get   'emud-trigger-color-pair-sample-face-get
  :notify            'emud-trigger-color-pair-notify
  :match             (lambda (widget new-val)
                       (and (listp new-val) (/= 0 (length new-val))))
  :indent 15
  :args '((editable-field :size 20 :tag "Foreground" :format "%t: %v\n")
          (editable-field :size 20 :tag "Background" :format "%t: %v\n")))

(defun emud-trigger-color-pair-notify (widget child &optional event)
  "Update the widget's value and update the sample face."
  (widget-put widget :value (emud-trigger-color-pair-value-get widget))
  (overlay-put (widget-get widget :sample-overlay)
	       'face (widget-apply widget :sample-face-get))
  (widget-default-notify widget child event))

(defun emud-trigger-color-pair-value-get (widget)
  (let (( result
          (mapcar (lambda (child)
                    (and (widget-get child :active)
                         (widget-value child)))
                  (widget-get widget :children))))
    result))

(defun emud-trigger-color-pair-value-to-internal (widget value)
  (let (result)
    (setq result (list (plist-get value :foreground)
                       (plist-get value :background)))
    result))

(defun emud-trigger-color-pair-value-to-external (widget value)
;;   (message "DEBUG emud-trigger-color-pair-value-to-external")
;;   (message "value = %s" value)
  (append (if (and (car value) (color-defined-p (car value)))
              (list :foreground (car value)))
          (if (and (cadr value) (color-defined-p (cadr value)))
              (list :background (cadr value)))))

(defun emud-trigger-color-pair-sample-face-get (widget)
;;   (message "DEBUG emud-trigger-color-pair-sample-face-get")
  (let* ((value (widget-get widget :value)) ; use :value because it is
                                            ; initialized sooner
         (fg (car value))
         (bg (cadr value)))
;;     (message "value = %s" value)
    (let ((result (append (if (and fg (color-defined-p fg))
                              (list :foreground fg) '())
                          (if (and bg (color-defined-p bg))
                              (list :background bg) '()))))
;;       (message "sample face = %s" result )
      result)))

(defun emud-config-draw ()
  (require 'widget)
  (setq buffer-read-only nil)
  (let (( inhibit-read-only t))
    (erase-buffer))

  (message "VALUE = %s"
           (widget-value (widget-create 'emud-trigger-actions
                               :value '(:font (:foreground "green")
                                              :code (lambda (x) (ignore))
                                              :ignore nil
                                              :respond "Reponse"))))
  (widget-setup))

(provide 'emud)

; EOF
