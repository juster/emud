(require 'emud)

(defvar mud-local-prompt-components
  '( (BASE . "") ))

(defvar mud-local-prompt-suffix
  "> ")

(make-variable-buffer-local 'mud-local-prompt-components)
(make-variable-buffer-local 'mud-local-prompt-suffix)

(defun emud-prompt-register-component (key-symbol)
  (if (assq key-symbol mud-local-prompt-components)
      (message "%s is already a registered prompt component" key-symbol)
    (setq mud-local-prompt-components
          (cons (cons key-symbol "")
                mud-local-prompt-components))))

(defun emud-prompt-unregister-component (key-symbol)
  (unless (assq key-symbol mud-local-prompt-components)
      (error "%s is not a registered prompt component" key-symbol))
  (when (eq key-symbol 'BASE)
    (error "You cannot unregister the BASE path component"))
  (setq mud-local-prompt-components
        (assq-delete-all key-symbol mud-local-prompt-components))
  (emud-prompt-refresh))

(defun emud-prompt-set-component (key text)
  (let (( found-pair (assq key mud-local-prompt-components) ))
    (if found-pair
        (setcdr found-pair text)
      (error "There is no registered prompt component named %s" key)))
  (emud-prompt-refresh))

(defadvice mud-set-prompt (before emud-fancy-prompt-set)
  (emud-prompt-set-component 'BASE (ad-get-arg 0))
  (ad-set-arg 0 (emud-prompt-generate)))

(ad-activate 'mud-set-prompt)

(defun emud-prompt-generate ()
  (concat (mapconcat 'identity
                     (delete "" (mapcar (lambda (comp) (cdr comp))
                                        (reverse mud-local-prompt-components)))
                     " ")
          mud-local-prompt-suffix))

(defun emud-prompt-refresh ()
  (save-match-data
    (ad-disable-advice 'mud-set-prompt 'before 'emud-fancy-prompt-set)
    (ad-activate 'mud-set-prompt)
    (mud-set-prompt (emud-prompt-generate))
    (ad-enable-advice 'mud-set-prompt 'before 'emud-fancy-prompt-set)
    (ad-activate 'mud-set-prompt)))

(defmacro %PROMPT-REGISTER (&rest components)
  (let ( result )
    (dolist (component components)
      (push `(emud-prompt-register-component ',component) result))
    `(progn
       ,@result)))

(defmacro %PROMPT-SET (component text)
  `(emud-prompt-set-component ',component ,text))

(defun emud-prompt-color-gauge (number-value gauge-meters)
  (let ( range-max color-name )
    (while gauge-meters
      (setq range-max  (cadr gauge-meters)
            color-name (car gauge-meters))
      (if (and range-max (<= number-value range-max))
          (setq gauge-meters nil)
        (setq gauge-meters (cddr gauge-meters))))
    (emud-color color-name (number-to-string number-value))))

(defmacro emud-prompt-color-number (number-value number-max &optional how)
  ;; TODO: logarithmic gauges
  (unless how (setq how 'linear))
  (let ( one-third range-list )
    (setq one-third (/ number-max 3)
          range-list (list "red" one-third
                           "yellow" (* 2 one-third)
                           "green"))
    `(emud-prompt-color-gauge ,number-value ',range-list)))

(defmacro %COLOR-GAUGE (number-value number-max)
  `(emud-prompt-color-number ,number-value ,number-max))

(provide 'emud-prompt)
