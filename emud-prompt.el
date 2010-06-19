;; emud-prompt.el
;; by Justin Davis < jrcd83 ATAT gmail >
;;
;; A more sophisticated prompt interface.  Scripts can register
;; components of the prompt they wish to set, along with a format
;; string.  Later they can set the prompt component and have the
;; prompt automatically updated.
;;
;; Overrides the `mud-set-prompt' function using advice.

(require 'emud)

(defvar mud-local-prompt-values
  '( ( BASE "%s" "" ) )
  )

(defvar mud-local-prompt-suffix
  "> ")

(make-variable-buffer-local 'mud-local-prompt-components)
(make-variable-buffer-local 'mud-local-prompt-suffix)

(defun emud-prompt-register-component (key-symbol &optional format-string)
  (unless format-string
    (setq format-string "%s"))
   (if (assq key-symbol mud-local-prompt-components)
       (message "%s is already a registered prompt component" key-symbol)
    (setq mud-local-prompt-components
          (cons (list key-symbol format-string "")
                mud-local-prompt-components))))

(defun emud-prompt-unregister-component (key-symbol)
  (unless (assq key-symbol mud-local-prompt-components)
      (error "%s is not a registered prompt component" key-symbol))
  (when (eq key-symbol 'BASE)
    (error "You cannot unregister the BASE path component"))
  (setq mud-local-prompt-components
        (assq-delete-all key-symbol mud-local-prompt-components))
  (emud-prompt-refresh))

(defun emud-prompt-set-component (key new-value)
  (let (( found-list (assq key mud-local-prompt-components) ))
    (if found-list
        (setcdr found-list (list (cadr found-list) new-value))
      (error "There is no registered prompt component named %s" key)))
  (emud-prompt-refresh))

(defadvice mud-set-prompt (before emud-fancy-prompt-set)
  (emud-prompt-set-component 'BASE (ad-get-arg 0))
  (ad-set-arg 0 (emud-prompt-generate)))

(ad-activate 'mud-set-prompt)

(defun emud-prompt-generate ()
  (concat (mapconcat 'identity
                     (delete "" (mapcar (lambda (comp)
                                          (emud-prompt-format
                                           (cadr comp)
                                           (caddr comp)))
                                        (reverse mud-local-prompt-components)))
                     " ")
          mud-local-prompt-suffix))

(defun emud-prompt-color-gauge-replacer (seq)
  "Helper function used by `emud-prompt-format'.  Uses the dynamic
variables format-string and format-value from that function.

Replaces any %<max>D format sequences with the color gauged number."
  (let (( max-param (match-string 1 seq) ))
    (unless max-param
      (error
       "%<max>D prompt format sequence must have a max specified as a number"))
    (setq max-param (string-to-number max-param))
    (emud-prompt-color-number format-value max-param)))

(defun emud-prompt-format (format-string format-value)
  "Custom formatting function which replaces color gauged parameters"
  (save-match-data
    (setq format-string
          (replace-regexp-in-string "%\\([0-9]+\\)?D"
                                    'emud-prompt-color-gauge-replacer
                                    format-string
                                    t))
    (format format-string format-value)))

(defun emud-prompt-refresh ()
  (save-match-data
    (ad-disable-advice 'mud-set-prompt 'before 'emud-fancy-prompt-set)
    (ad-activate 'mud-set-prompt)
    (mud-set-prompt (emud-prompt-generate))
    (ad-enable-advice 'mud-set-prompt 'before 'emud-fancy-prompt-set)
    (ad-activate 'mud-set-prompt)))

(defmacro %PROMPT-REGISTER (key-symbol &optional format)
  `(emud-prompt-register-component ',key-symbol ,format))

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

(defun emud-prompt-color-number (number-value number-max &optional how)
  ;; TODO: logarithmic gauges
  (unless how (setq how 'linear))
  (let ( one-third range-list )
    (setq one-third (/ number-max 3)
          range-list (list "red" one-third
                           "yellow" (* 2 one-third)
                           "green"))
    (emud-prompt-color-gauge number-value range-list)))

(defmacro %COLOR-GAUGE (number-value number-max)
  `(emud-prompt-color-number ,number-value ,number-max))

(provide 'emud-prompt)
