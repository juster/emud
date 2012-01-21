(defvar emud-scripts
  '())

(defun head-is-symbol-p (expected-name body)
  (and (symbolp (car body))
       (eq t (compare-strings expected-name 0 nil
                              (symbol-name (car body)) 0 nil t))))

(defun get-head-symbol-string (body)
  (upcase (symbol-name (car body))))

(defmacro %SCRIPT (script-name &rest body)
  (unless (stringp script-name)
    (error "First argument to %SCRIPT must be a name (string)"))
  (let (script-host-name script-variables macros)
    (when (head-is-symbol-p "for" body)
      (unless (stringp (setq script-host-name (cadr body)))
        (error
         "Must provide a mud name as a string after %SCRIPT's FOR option"))
      (setq body (cddr body)))
    (let ((script-variables '()))
      (setq body (extract-escript-variables body 'script-variables))
      (setq body (mapcar (lambda (macro) (macroexpand macro)) body))
      (message "DEBUG: body = %S" body)
      `(let ((script-variables ',script-variables))
         ,(if script-host-name `(with-host-buffers ,script-host-name ,@body)
            `(with-mud-buffers ,@body))))))

(defun extract-escript-variables (macro-args variables-symbol)
  "Extracts any LET options given to a macro as arguments.
ARGS is the argument list.  VARIABLES-SYMBOL is the symbol which
contains the alist of variable names/values.

Returns all the MACRO-ARGS that were not LET or LET's arguments."
  (let (pruned-args new-var-alist)
    (while macro-args
      (if (head-is-symbol-p "LET" macro-args)
          (let ((let-args  (cadr macro-args))
                (let-plist '()))
            (unless (and (not (null let-args)) (listp let-args))
              (error
               "Arguments to %%WHERE's VARS option must be a non-empty list of symbols"))
            ;; Convert vars to a property list and append to
            ;; the dynamic variables (i.e. `script-variables')
            (dolist (let-arg let-args)
               (cond ((listp let-arg)
                      (setq let-plist
                            (plist-put let-plist
                                       (escript-var-symbol-to-string
                                        (car let-arg))
                                       (cadr let-arg))))
                     ((symbolp)
                      (setq let-plist
                            (plist-put let-plist
                                       (escript-var-symbol-to-string
                                        let-arg)
                                       nil)))
                     (t
                      (error "Arguments to LET option must be a list of symbols or symbol-value pairs"))))
            (message "let-plist = %S" let-plist)
            (set variables-symbol
                 (append let-plist
                         (symbol-value variables-symbol)))
            (setq macro-args (cddr macro-args))
            (message "macro-args = %S" macro-args))
        (progn
          (setq pruned-args (cons (car macro-args) pruned-args))
          (setq macro-args (cdr macro-args)))))
    (nreverse pruned-args)))

(defun escript-var-symbol-to-string (var-symbol)
  "Converts VAR-SYMBOL to a string, removes leading $ signs."
  (unless (symbolp var-symbol)
    (error "%S is not a valid symbol object" var-symbol))
  (let ((var-name-string (symbol-name var-symbol)))
    (while (char-equal ?$ (string-to-char var-name-string))
      (setq var-name-string (substring var-name-string 1)))
    var-name-string))

(defun escript-get-var (var-name &optional inside-trigger)
  "Macro makes getting the value of script or trigger variables easier.

If INSIDE-TRIGGER is false, expands to retrieve the variables
from the dynamic functions `script-variables' or
`trigger-variables'.  These are located in the `%SCRIPT' and
`%WHEN' macros, respectively.

If INSIDE-TRIGGER is true, then expands to retrieve the variables
from the `trigger' parameter's symbol plist at runtime."
  (let ((plist-symbol
         (cond ((lax-plist-get trigger-variables var-name)
                (if inside-trigger :trigger-variables 'trigger-variables))
               ((lax-plist-get script-variables var-name)
                (if inside-trigger :script-variables 'script-variables))
               (t 
                (message "DEBUG: script-variables = %S var-name = %S"
                         script-variables
                         var-name)
                (error "Variable named '%s' is not a defined trigger or script variable" var-name)))))
    (if inside-trigger
        `(lax-plist-get (get trigger ,plist-symbol) ,var-name)
      (lax-plist-get (symbol-value plist-symbol) var-name))))

(defun replace-escript-var-string (var-string)
  (let (chunked var-name)
    (while (string-match "\\$\\([0-9]\\|[a-zA-Z_-]+\\)\\b" var-string)
      (setq var-name (match-string 1 var-string))
      (setq chunked
            (append chunked
                    (list (substring var-string 0 (match-beginning 0))
                          (if (and (= (length var-name) 1)
                                   (>= (string-to-char var-name) ?0)
                                   (<= (string-to-char var-name) ?9))
                              ;; This is a regexp match variable (ie $0, etc)
                              `(match-string ,(- (elt var-name 0) ?0) match)
                            ;; This is a script variable, use the trigger's
                            ;; variables or the script's variables.
                            (escript-get-var var-name t)))))
      (setq var-string (substring var-string (match-end 0)))
      (message "length var-string = %S" (length var-string))
      )
    (when (> (length var-string) 0)
      (setq chunked (append chunked (list var-string))))
    (setq chunked (delete "" chunked))
    (if chunked
        (if (> (length chunked) 1)
            `(concat ,@chunked)
          (car chunked))
      var-string)))

(defun find-and-replace-escript-vars ( code-element )
  ;; Filter out any occurrances of inline variables.
  ;; They begin with $, like in perl.
  (cond ((stringp code-element)
         (replace-escript-var-string code-element))
;;         ((and (symbolp code-element)
;;               (char-equal ?$ (string-to-char (symbol-name code-element))))
;;          (let* (( var-name  (escript-var-symbol-to-string code-element) ))
;;            `(string-to-number ,(escript-get-var var-name t))))
        ((listp code-element)
         (mapcar 'find-and-replace-escript-vars code-element))
        (t code-element)))

(defun escript-expand-code (code)
  "Expand CODE given for triggers, whether it is one statement or
a list of statements."
  (message "DEBUG: code = %S" code)
  ;; A single statement is either a string or a macro/function call
  (if (or (and (listp code) (symbolp (car code)))
          (stringp code))
    (setq code (list code)))            ;convert it to a list of statements

  ;; we replace strings (at the top level only) with sends.
  (setq code (mapcar (lambda (statement)
                       (cond ((stringp statement)
                              `(emud-send-line ,statement))
                             (t
                              (macroexpand-all statement))))
                     code))

  ;; we replace all variables that we can find
  (setq code (mapcar 'find-and-replace-escript-vars code))

  `(progn ,@code))

(defun escript-when-foreach (regexp foreach-body)
  (let ((pos-symbol (make-symbol "foreach-position")))
    `(let ((,pos-symbol 0)
           (iter-count  0))
       (save-match-data
         (while (and (< ,pos-symbol (1- (length match)))
                     (< iter-count 100)
                     (string-match ,regexp match ,pos-symbol))
           ,(escript-expand-code foreach-body)
           (setq ,pos-symbol (match-end 0))
           (when (= ,pos-symbol 0)
             (setq ,pos-symbol (length match)))
           (setq iter-count (1+ iter-count)))
         (when (= iter-count 100)
           (message "Warning: FOREACH loop reached maxed iterations" ))))))

(defun parse-escript-when-args (body)
  (let (result element trigger-return-val)
    (while body
      (setq element (car body) body (cdr body))
      (if (symbolp element)
        (let ((option (upcase (symbol-name element))))
          (cond ((string= option "FOREACH")
                 (push (escript-when-foreach (car body) (cdr body)) result)
                 (setq body (cddr body)))
                ((string= option "HIDE")
                 (setq trigger-return-val
                       '(propertize match 'invisible t)))
                (t
                 (error "Unknown %%WHEN option: '%s'" option))))
        ;; Everything other than symbols should be code
        (push (escript-expand-code element) result))
      )
    (setq result (nreverse result))
    `(lambda (trigger match)            ; Return generated lambda.
       ,@result
       ,trigger-return-val)))

(defun escript-var-symbol (prefixed-variable-symbol)
  (make-symbol (escript-var-symbol-to-string prefixed-variable-symbol)))

(defun escript-make-trigger (code &rest trigger-plist)
  (let ((trigger-symbol (make-symbol "trigger")))
    (fset trigger-symbol code)
    (setplist trigger-symbol trigger-plist)
    trigger-symbol))

(defmacro %WHEN (regexp &rest body)
  (let (trigger-variables)
    (setq body (extract-escript-variables body 'trigger-variables))
    `(emud-set-trigger
      ,regexp
      (escript-make-trigger
       ,(parse-escript-when-args body)
       :script            ,(if (boundp 'script-name) script-name nil)
       :script-variables  ,(if (boundp 'script-variables) script-variables nil)
       :trigger-variables ,trigger-variables))))

(defmacro %SET (var-symbol new-value)
  "A macro to be used inside a trigger definition (%WHERE macro).
Assigns either a script or trigger scoped variable named VAR-NAME
with the new value NEW-VALUE."
  (let* ((var-name (escript-var-symbol-to-string var-symbol))
         (plist-symbol
          (cond ((lax-plist-get trigger-variables var-name)
                 :trigger-variables)
                ((lax-plist-get script-variables var-name)
                 :script-variables)
                (t
                 (error "Variable named '%s' is not a defined trigger or script variable" var-name)))))
    `(lax-plist-put (get trigger ,plist-symbol) ,var-name ,new-value)))

(defmacro %GET (var-symbol)
  (let* ((var-name (escript-var-symbol-to-string var-symbol))
         (plist-symbol
          (cond ((lax-plist-get trigger-variables var-name)
                 :trigger-variables)
                ((lax-plist-get script-variables var-name)
                 :script-variables)
                (t
                 (error "Variable named '%s' is not a defined trigger or script variable" var-name)))))
    `(lax-plist-get (get trigger ,plist-symbol) ,var-name)))

(defmacro %COLOR (color-format string)
  `(emud-color ,color-format ,string))

(defmacro %INC (var-symbol)
  `(%SET ,var-symbol (1+ (%GET ,var-symbol))))

(defmacro %DEC (var-symbol)
  `(%SET ,var-symbol (1- (%GET ,var-symbol))))

(provide 'emud-script)