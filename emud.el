;; Emacs MUD Client
;; by Justin Davis < jrcd83 ATAT gmail >
;;
;;
;; This source file is seperated into the following sections:
;;
;; * CUSTOMIZE
;; Customizations for things like font faces and simple settings that
;; effect all emud sessions
;;
;; * VARIABLES
;; Global variables used by every emud session
;;
;; * FUNCTIONS
;; Simple functions that don't belong anywhere else, like emud-mode for
;; example
;;
;; * COMMANDS
;; Commands that are user-accessible and mostly considered the public
;; interface.  Commands start with "emud-".
;;
;; * FILTERS
;; Filters are internally used by emud to extract stateful protocol
;; information like VT100 color codes and Telnet protocol codes.  The
;; codes are also removed from the mud server's output.
;;
;; * TRIGGERS
;; Triggers, like with most mud clients.  Triggers respond to a matching
;; regexp with some sort of action, like coloring, sending the mud server
;; some input, or a lambda function.
;;
;; * INPUT AREA
;; The input area is where the user types in their input before sending
;; it to the mud server.  The input area keeps mud output from mixing
;; with the user's input before they are ready to send it.
;;
;; ** STICKY INPUT
;; A common feature with mud clients, sticky input keeps the last line of
;; input around in case the user wants to send it again.  Then they only
;; have to press ENTER if they do.
;;
;; * MUD SETTINGS
;; This is a vague name that means settings that are mud-specific.
;; Things like triggers, aliases, variables, maps, etc.  Because these
;; are not global across all emud sessions, they are stored in a seperate
;; file.  This file is ~/.emudrc by default but can be customized.
;;
;; ** MUD CONFIG BUFFER
;; The mud config buffer aims to be like customizing in Emacs.  Using
;; widgets you should be able to easily add, edit, and delete settings
;; like triggers, aliases, etc.
;;


;; CUSTOMIZE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgroup emud nil
  "The Emacs MUD Client"
  :group 'applications)

(defcustom mud-sticky-input t
  "Sticky input saves the previous user input so you can easily
resend it by pressing ENTER.  This is useful for travelling and
other repetitive commands in MUDs."
  :type  'boolean
  :group 'emud)

(defcustom mud-history-max 25
  "How many lines of user input to remember in history"
  :type  'integer
  :group 'emud)

(defface mud-input-area
  '( ( ((class color))
       (:foreground "green")
       ) )
  "The font face of the active input area"
  :group 'emud)

(defface mud-client-message
  '( ( ((class color))
       (:foreground "yellow")
       ) )
  "The font face for messages from the mud client"
  :group 'emud)

(defcustom mud-config-base "~/.emacs.d/emud"
  "Directory which contains all of EMUD's settings and sessions
triggers, aliases, etc."
  :type 'file
  :group 'emud)


;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mud-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r"       'mud-input-submit)
    (define-key map "\C-c\C-h" 'emud-send-input-history)
    (define-key map "\d"       'emud-sticky-backspace)
    map))

; the sticky properties below don't have to do with sticky-input
(defvar mud-default-output-props
  '(read-only t rear-nonsticky t front-sticky t mud-color-intensity 1))

; ANSI color palette
(defvar mud-color-palette
  '(("black"       "grey10"  "gray20"  )   ; 0
    ("red5"        "red2"    "red1"    )
    ("green4"      "green2"  "green1"  )
    ("yellow3"     "yellow2" "yellow1" )
    ("blue4"       "blue2"   "blue1"   )
    ("DarkMagenta" "magenta" "HotPink" )
    ("cyan4"       "cyan3"   "cyan1"   )
    ("grey50"      "white"   "white"   ))) ; 7
    ; dim          normal    bright

(defvar mud-ansi-color-codes
  `(("0"  . (lambda ()                  ; reset attributes
              (copy-tree mud-default-output-props)))
    ("1"  . (lambda ()                 ; bright
              (plist-put mud-local-output-text-props 'mud-color-intensity 1)))
    ("2"  . (lambda ()                 ; dim
              (plist-put mud-local-output-text-props 'mud-color-intensity 0)))
    ("4"  . (:underline t t))           ; underscore
    ("5"  . nil)                        ; blink
    ("7"  . nil)                        ; reverse
    ("8"  . (:invisible t t))           ; hidden

    ;( code (font key ((dim color  normal color bright color))))
    ("30" . (:foreground ,(nth 0 mud-color-palette)))
    ("31" . (:foreground ,(nth 1 mud-color-palette)))
    ("32" . (:foreground ,(nth 2 mud-color-palette)))
    ("33" . (:foreground ,(nth 3 mud-color-palette)))
    ("34" . (:foreground ,(nth 4 mud-color-palette)))
    ("35" . (:foreground ,(nth 5 mud-color-palette)))
    ("36" . (:foreground ,(nth 6 mud-color-palette)))
    ("37" . (:foreground ,(nth 7 mud-color-palette)))

    ("40" . (:background ,(nth 0 mud-color-palette)))
    ("41" . (:background ,(nth 1 mud-color-palette)))
    ("42" . (:background ,(nth 2 mud-color-palette)))
    ("43" . (:background ,(nth 3 mud-color-palette)))
    ("44" . (:background ,(nth 4 mud-color-palette)))
    ("45" . (:background ,(nth 5 mud-color-palette)))
    ("46" . (:background ,(nth 6 mud-color-palette)))
    ("47" . (:background ,(nth 7 mud-color-palette)))))

(defvar mud-builtin-triggers
  '(( "https?://[A-Za-z0-9./?=&;~_%-]+" .
      (:code mud-trigger-url) ))
  "Builtin triggers for all sessions.  These cannot be
edited interactively.")

(defvar mud-server-filters
  '(("\033\\(?:\\[\\(?:\\([0-9;]*\\)\\([^0-9;]\\)?\\)?\\)?" . mud-color-filter)
    ("\xFF\\([\xFB-\xFE].\\)?" . mud-telnet-filter)
    ("^.*\x0D\x00" . mud-erase-line-filter))

  "A list of filters.  Each filter is a cons cell with a regular
expression and a function to call if the expression matches.
Filters match special characters, remove them from the server output,
and change text properties setting `mud-local-output-text-props'.

No arguments are passed, instead the filter modifies the
`recv-data' variable from `mud-filter' in place.

Server filters search for regular expressions in the output
received from the MUD server and remove the text in place.  After
the filter runs the output is passed to the next filter.  After
all filters are checked, the output is written to the buffer.

Filters can throw a 'filter-resume symbol to abort filtering and
have the server's next output sent straight to the throwing
filter.  This is useful if the data the filter is parsing is
split across two socket receives.")

(defvar mud-input-history-temp nil
  "A global variable to hold our buffer local input history so
the minibuffer has access to it")

(defmacro read-from-minibuffer-default (prompt default)
  (if (boundp default)
       `(let ( minibuffer-input )
          (setq minibuffer-input
                (read-from-minibuffer
                 (concat ,prompt
                         (format " (default %s): "
                                 (symbol-value ,default)))))
          (if (= 0 (length minibuffer-input))
              (symbol-value ,default) minibuffer-input))
    `(read-from-minibuffer (concat ,prompt ": "))))

(defvar mud-active-buffers '()
  "A list of currently active MUD sessions/buffers")

(defvar emud-connect-hook '()
  "Hooks that are run after connecting to any MUD server.")

(defvar emud-disconnect-hook '()
  "Hooks that are run after disconnecting from any MUD server.")


;; LOCAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mud-local-host-name nil
  "Hostname the buffer is connected to.")
(make-variable-buffer-local 'mud-local-host-name)

(defvar mud-local-net-process nil
  "Process object that connects the server and client.")
(make-variable-buffer-local 'mud-local-net-process)

(defvar mud-local-sticky-input-flag nil
  "Whether the input area is currently 'sticky'.")
(make-variable-buffer-local 'mud-local-sticky-input-flag)

(defvar mud-local-filter-continuation nil
  "Left-over, incomplete data from the last filter.")
(make-variable-buffer-local 'mud-local-filter-continuation)

(defvar mud-local-color-intensity 1
  "Intensity of the VT100 color codes.  0 = Dim, 1 = Normal, 2 = Bright")
(make-variable-buffer-local 'mud-local-color-intensity)

(defvar mud-local-echo t
  "If local echo is enabled, show input as it is typed and sent")
(make-variable-buffer-local 'mud-local-echo)

(defvar mud-local-cached-triggers nil
  "Triggers used for the current session")
(make-variable-buffer-local 'mud-local-cached-triggers)

(defvar mud-local-cached-aliases nil
  "Aliases used for the current session")
(make-variable-buffer-local 'mud-local-cached-aliases)

(defvar mud-local-input-history nil
  "Input history")
(make-variable-buffer-local 'mud-local-input-history)

(defvar mud-local-input-overlay nil
  "Overlay which contains the user input area")
(make-variable-buffer-local 'mud-local-input-overlay)


;; FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-mode ()
  "Major mode for playing MUDs"
  (interactive)
  (unless (eq major-mode 'mud-mode) (kill-all-local-variables))
  (use-local-map mud-mode-keymap)
  (setq mode-name "MUD")
  (setq major-mode 'mud-mode))

;; (defun mud-vt100-find-end (recv-string &optional x)
;;   (when (null x) (setq x 0))
;;   (if (>= x (length recv-string)) nil
;;     (if (char-equal ?m (aref recv-string x)) x
;;       (mud-vt100-find-end recv-string (+ x 1)))))


(defun mud-sentinel (mud-process event)
  (when (buffer-name (process-buffer mud-process))
    (with-current-buffer (process-buffer mud-process)
      ;; If we are disconnected, remove buffer from active buffers list.
      ;; Also clear the prompt because it looks weird.
      (when (string= event "connection broken by remote peer\n")
        (mud-clear-prompt)
        (setq mud-active-buffers
              (delq (process-buffer mud-process) mud-active-buffers)))
      (mud-client-message (replace-regexp-in-string "\n+$" "" event)))))

(defun mud-client-message (message)
  "Send a message to the user from the MUD client."
  (save-excursion
    (goto-char (process-mark mud-local-net-process))
    (insert-before-markers
     (let (( prop-list mud-local-output-text-props ))
       (plist-put prop-list 'face "mud-client-message")
;;        (plist-put prop-list 'rear-sticky nil)
;;        (plist-put prop-list 'front-sticky nil)
       (apply 'propertize (format "*EMUD* says: \"%s\"\n" message)
              prop-list)))))


(defun mud-append-server-output (output)
  "Append text from the server before the input area."
  (save-excursion
    (goto-char (process-mark mud-local-net-process))
    (insert-before-markers
     (apply 'propertize output mud-local-output-text-props))))

(defun mud-grep-list (test-function grep-list)
  "Acts like perl's grep builtin.  Passes each element in
GREP-LIST to TEST-FUNCTION.  Returns a list of every element
which returned t.  Preserves the original order as well."
  (let (result-list element)
    (while grep-list
      (setq element (car grep-list)
            grep-list (cdr grep-list))
      (when (funcall test-function element)
          (setq result-list (cons element result-list)))))
  (nreverse result-list))

(defun emud-set-trigger (regexp plist-or-symbol)
"Sets a trigger to be executed when REGEXP is seen in output from the
server to the MUD client.

PLIST-OR-SYMBOL can be a property list or a symbol. The property
lists are documented below. Symbols are used for scripting and
I forget how they work at the moment ... :P

:ACTION-TYPE can be :font :respond :code or :ignore.  Only one
of each type can be present.  In this way actions are like
property lists, but they are presorted by `cache-mud-settings' so
they are executed in the proper order.

:font must have a face attribute list as ACTION.

:respond must have a string as ACTION

:code must have a list beginning with 'lambda or a symbol to a
function.  Anything `funcall' can use.  The function must return
text to replace the matching text with.

:ignore has its ACTION ignored, ironically.  The matching text is
simply removed."
  (interactive "sRegexp: \nXTrigger (ex: symbol or '(:type action)): \n")
  ;; TODO: check the value of the trigger?!
  (let ((trig (assoc-string regexp mud-local-triggers)))
    (if trig (setcdr trig plist-or-symbol)
      (setq mud-local-triggers
            (cons (cons regexp plist-or-symbol) mud-local-triggers)))))

(defmacro with-mud-buffers (&rest body)
  `(dolist (active-buffer mud-active-buffers)
     (with-current-buffer active-buffer
       ,@body)))

(defmacro with-host-buffers (host-name &rest body)
  `(with-mud-buffers
    (when (string-equal mud-local-host-name ,host-name)
      ,@body)))

(defun emud-send-line (string-to-send)
  "Sends STRING-TO-SEND to the mud server for the current buffer.
Appends a newline to the end if the string doesnt end with newline."
  (process-send-string mud-local-net-process
                       (if (string-match "\n$" string-to-send)
                           string-to-send
                         (concat string-to-send "\n"))))

(defun emud-color (color-format string)
  (let (color-names color-props)
    (save-match-data
      (setq color-names (split-string color-format ":")))
    (unless (> (length color-names) 0)
      (error
       "emud-color must be provided at least one color as second argument"))
    (setq color-props (list :foreground (car color-names)))
    (when (cadr color-names)
      (setq color-props (append color-props
                                (list :background (cadr color-names)))))
    (apply 'propertize string 'face (list color-props))))

(defun mud-find-recv-data-matches (pos regexp-alist)
  (save-match-data
    (let (lowest-pos next-match matched match-begin)
      (while regexp-alist
        (setq next-match   (car regexp-alist)
              regexp-alist (cdr regexp-alist)
              match-begin  (string-match
                            (car next-match) recv-data pos))
        (cond ((null match-begin)
               nil)
              ((or (null lowest-pos) (< match-begin lowest-pos))
               (setq matched (list (cons (cdr next-match) (match-data)))
                     lowest-pos match-begin))
              ((= match-begin lowest-pos)
               (setq matched (cons (cons (cdr next-match) (match-data))
                                   matched)))))
      matched)))

;; COMMANDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun emud-connect (hostname port)
  (interactive "sHostname: \nnPort: ")
  (let (( mud-host-and-port (format "%s:%d" hostname port) ))
    (with-temp-message (format "Connecting to %s ..." mud-host-and-port)
      (let* (( mud-buffer  (generate-new-buffer mud-host-and-port) )
             ( mud-process (make-network-process
                            :name             mud-host-and-port
                            :host             hostname
                            :service          port
                            :coding           'emacs-mule-dos
                            :buffer           mud-buffer
                            :filter           'mud-filter
                            :filter-multibyte t
                            :sentinel         'mud-sentinel) ))
        (set-buffer mud-buffer)
        (let ( (default-major-mode 'mud-mode) )
          (kill-all-local-variables)
          (set-buffer-major-mode mud-buffer))

        (setq mud-local-host-name hostname)
        (setq mud-local-net-process mud-process)
        (setq mud-local-output-text-props
              (copy-tree mud-default-output-props))

        ;; Overlay for user input area -------------------------
        (let (( end-of-buffer (point-max) ))
          (setq mud-local-input-overlay
                (make-overlay end-of-buffer end-of-buffer
                              (current-buffer) nil t))
          (overlay-put mud-local-input-overlay 'field 'mud-input)
          (overlay-put mud-local-input-overlay 'non-rearsticky t)
          (overlay-put mud-local-input-overlay 'face "mud-input-area")
          (overlay-put mud-local-input-overlay 'invisible nil))
        ;; -----------------------------------------------------

        ;; Override external variables
        (setq tab-width 8)
        (set (make-local-variable 'debug-on-error) 1)
                                        ; set to a high number
        (set (make-local-variable 'scroll-conservatively) 1000)
        (buffer-disable-undo)

        (load-mud-settings hostname)
        (run-hooks 'emud-connect-hook)
        (add-hook 'kill-buffer-hook
                  (lambda ()
;;                     (message
;;                      "DEBUG removing killed buffer from mud-active-buffers\n")
                    (setq mud-active-buffers
                          (delq (current-buffer) mud-active-buffers)))
                  nil t)                ; buffer-local hook
        (setq mud-active-buffers (cons mud-buffer mud-active-buffers))

        (set-window-buffer (selected-window) mud-buffer)))))

(defun mud-input-history-exit ()
  (setq mud-input-history-temp nil)
  (remove-hook 'minibuffer-exit-hook 'mud-local-input-history-exit))

(defun emud-send-input-history (send-history)
  (interactive
   (if mud-input-history-temp
       (error "Cannot browse input history in two frames at once")
     (progn
       (setq mud-input-history-temp mud-local-input-history)
;;       (add-hook 'minibuffer-setup-hook 'mud-local-input-history-setup)
       (add-hook 'minibuffer-exit-hook 'mud-input-history-exit)
       (list
        (completing-read "History: "
                         mud-local-input-history-temp
                         nil nil nil
                         '(mud-local-input-history-temp . 1)
                         (mud-get-input-area))))))
  (mud-set-input-area send-history)
  (mud-input-submit))

;; FILTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mud-telnet-codes
  '((?\xFF . :IAC)
    (?\xFE . :SE)
    (?\xFB . :WILL)
    (?\xFC . :WONT)
    (?\xFD . :DO)
    (?\xFE . :DONT)))

(defun mud-telnet-code (symbol)
  (car (rassq symbol mud-telnet-codes)))

(defvar mud-telnet-code-iac  "\xFF")
(defvar mud-telnet-code-se   "\xFE")
(defvar mud-telnet-code-will "\xFB")
(defvar mud-telnet-code-wont "\xFC")
(defvar mud-telnet-code-do   "\xFD")
(defvar mud-telnet-code-dont "\xFE")

(defun mud-telnet-confirm (accept-flag opt-code)
  "Sends a response about the requested option.  If `accept-flag'
is t, sends we WILL do the option.  If nil sneds we WONT do the option.
The option's code in the telnet specification is given with `opt-code', a
character/byte.  Uses the variable `process' from `mud-filter'."
  (process-send-string process
                       (concat
                        mud-telnet-code-iac
                        (if accept-flag
                            mud-telnet-code-will
                          mud-telnet-code-wont)
                        (char-to-string opt-code)
                        mud-telnet-code-se)))

(defun mud-telnet-extract-codes (telnet-code-string &optional pos)
  (unless pos (setq pos 0))
  (if (>= pos (length telnet-code-string))
      nil
    (let* (( code       (aref telnet-code-string pos) )
           ( code-assoc (assoc code mud-telnet-codes) ))
      (cons (if code-assoc
                (cdr code-assoc)
              code)
            (mud-telnet-extract-codes telnet-code-string (1+ pos))))))

(defun mud-telnet-filter ()
  (let* ((telnet-codes (match-string 1 recv-data))
         (code-symbols (mud-telnet-extract-codes telnet-codes)))

;;    (message "DEBUG: parsed symbols: %s" code-symbols)
    (cond ((= (elt code-symbols 1) 1)
;;           (message "DEBUG: received echo code")
           (let ((echo (not (eq (car code-symbols) :WILL))))
;;             (message "DEBUG: echo = %s" echo)
             (mud-input-echo echo)
             (setq mud-local-echo echo))))))

(defun mud-store-color-codes (color-codes)
  "Stores the mud color codes as the current color to use.
COLOR-CODES is the string of VT100/ANSI color codes, with ; as a
delimiter just as they are received.

The new colors are stored in `mud-local-output-text-props'."
  (let (( ansi-codes (split-string color-codes ";" t) ))
    (dolist (code ansi-codes)
      (let* ((code-entry (assoc-string code mud-ansi-color-codes))
             (code-action (if code-entry (cdr code-entry) nil)))

        ;; ansi color code keys can be either a font property or a
        ;; function
        (cond ((null code-action)       ; no match was found
               nil)

              ((functionp code-action)  ; action is a lambda
               (setq mud-local-output-text-props (funcall code-action)))

              (t                        ; action is a property list

                                        ; Choose a diff color if we
                                        ; are dim, bright, or
                                        ; normal...
               (let* ((color-intensity (plist-get mud-local-output-text-props
                                                  'mud-color-intensity))
                      (face-prop (list (car code-action)
                                       (nth color-intensity
                                            (cadr code-action)))))
                 ;; Preserve existing text properties
                 (if (plist-get 'face mud-local-output-text-props)
                     (plist-put (plist-get 'font mud-local-output-text-props)
                                (car face-prop) (cadr face-prop))
                   (plist-put mud-local-output-text-props 'face face-prop))
                 )))))))


;; Uses the recv-data variable in mud-filter directly
(defun mud-color-filter ()
  (let ((color-codes  (match-string 1 recv-data))
        (end-code     (match-string 2 recv-data)))
    
    (if end-code
        ;; ignores codes other than color codes
        (when (string= end-code "m")              
          (mud-store-color-codes color-codes))
      ;; save code for later if it doesn't end
      ;; (it was split between two sends/recvs)
      (throw 'mud-filter-continue t))))

(defun mud-erase-line-filter ()
;;  (message "DEBUG: erase-line-filter on: %s" recv-data)
  (let (( inhibit-read-only t ))
    (save-excursion
      (goto-char (process-mark process))
      (unless (bolp)
;;        (message "DEBUG: should erase line")
        (forward-line 0)
        (delete-region (point) (process-mark process))))))

(defun mud-filter-match-sort (left right)
;;  (message "DEBUG sorting, left = %s -- right = %s" left right)
  (< (cadr left) (cadr right)))

(defun mud-filter-matches (filter-list)
  "Create a list of matched regexps, if any.  Checks each regexp.

FILTER-LIST is a list of filter pairs: ( REGEXP . FUNCTION ).
The idea being, if REGEXP matches, FUNCTION is called.

A list of matches is returned in a special format.  The filter
is appended with the match data, as from `match-data'.

The result is a sorted associated list:
\( ( FUNCTION-SYMBOL . ( MATCH-DATA ) ), ... )

This is actually the same as a flat list with the function symbol
as the first element, followed by the match position data:
\( ( FUNCTION-SYMBOL, MATCH-DATA ), ... )
"
  (if filter-list
      (let (( filter (car filter-list) ))
;;        (message "DEBUG: filter regexp = %s" (cdr filter))
        (if (string-match (cdr filter) recv-data 0)
            ;; Construct an associated list like the one in description.
            (cons (cons (car filter)
                        (match-data))
                  (mud-filter-matches (cdr filter-list)))
          (mud-filter-matches (cdr filter-list))))
    nil))

(defun mud-filter-helper (pos filter)
  (let (( match-regexp      (cdr (assq (car filter) mud-server-filters)))
        ( match-positions   (cdr filter)))
    ;; set preceding text to the old text properties
    (when (< pos (car match-positions))
      (set-text-properties pos (car match-positions)
                           mud-local-output-text-props recv-data))
    (set-match-data match-positions)
    (when (catch 'mud-filter-continue
            (funcall (car filter))
            nil)
      ;; If the filter requested a continuation, store the "old" data.
      (setq mud-local-filter-continuation (substring recv-data pos))
      (setq recv-data (substring recv-data 0 pos))
      (throw 'mud-filter-continue t))
    (set-match-data match-positions)
    (setq recv-data (replace-match "" nil t recv-data))
    (car match-positions)))

(defun mud-filter (process recv-data)
  "The master mud server output filter for the MUD connection/process.

Filters through the data by checking every filter in
`mud-server-filters' and filtering out unprintables with them.

Checks all mud server output for any trigger matches after that."
  (with-current-buffer (process-buffer process)

    ;; If a filter is continuing from before, prepend the old data.
    (when mud-local-filter-continuation
      (setq recv-data (concat mud-local-filter-continuation recv-data))
      (setq mud-local-filter-continuation nil))

    (catch 'mud-filter-continue
      (let (( pos        0   )
            ( matches    nil )
            ( next-match nil ))
        (while (setq next-match
                     (car (mud-find-recv-data-matches pos mud-server-filters)))
          (setq pos (mud-filter-helper pos next-match)))

        ;; Set the text properties of leftover text after all filters.
        (when (< pos (1- (length recv-data)))
          (set-text-properties
           pos (length recv-data)
           mud-local-output-text-props recv-data))))

    (mud-triggers)

    (let (( inhibit-read-only t ))
      (save-excursion
        (goto-char (process-mark process))
        (insert-before-markers recv-data)))))

;; TRIGGERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mud-triggers ()
  "Check the mud server output for text matching triggers.

This function takes no parameters, but uses the dynamically-scoped recv-data
parameter from `mud-filter', where it is called from.

Each trigger is a cons cell with a REGEXP and corresponding actions:
\(REGEXP . (:ACTION-TYPE ACTION [ :ACTION-TYPE ACTION ... ]))"

  (let (( pos 0 ))
    ;; TODO: replace this with assoc-default, or should I?
    ;; CREATE a better algorithm for searching, like the mud filters use.
    (dolist (trigger mud-local-triggers)
      (while (and (< pos (length recv-data))
                  (string-match (car trigger) recv-data pos))
        (let* ((trigger-action  (cdr trigger))
               (matched-text    (match-string 0 recv-data))
               (matched-offsets (match-data))
               (match-start     (car matched-offsets))
               (trigger-result  nil))

          (message "DEBUG: trigger regexp matched: %S" (car trigger))
          (set-match-data  (mapcar
                            (lambda (pos)
                              (- pos match-start))
                            (match-data)))

          ;; Triggers made by scripts are symbols
          (if (symbolp trigger-action)
              (progn
;;                (setq pos (cadr matched-offsets))
                (message "DEBUG: trigger-action = %S"
                         (symbol-function trigger-action))
                (setq trigger-result
                      (funcall (symbol-function trigger-action)
                               trigger-action
                               (substring recv-data match-start)))
                (message "DEBUG: trigger-result = %S" trigger-result)
                (when (stringp trigger-result)
                  (setq matched-text trigger-result)))

            ;; Triggers made using `emud-config' are property lists
            ;; Execute a trigger's code and replace the original
            ;; match with the code's return value, if there is one.
            (when (plist-get trigger-action :code)
;;              (message "DEBUG: calling :code trigger")
              (setq trigger-result
                    (funcall (plist-get trigger-action :code) matched-text))
;;              (message "DEBUG: trigger-result = %S" trigger-result)
              (if (stringp trigger-result)
                  (setq matched-text trigger-result)))

            ;; Send a string in response.
            (when (plist-get trigger-action :respond)
              (process-send-string
               mud-local-net-process
               (concat (plist-get trigger-action :respond) "\n")))
            ;; XXX: we might want to abort here if the code replaced
            ;; the text with an empty string

            ;; Ignore the matched text, replace it with empty string.
            (when (plist-get trigger-action :ignore)
              (setq matched-text ""))
          
            ;; Apply text properties to the matched text
            ;; (or what's left of it)
            (when (plist-get trigger-action :font)
              (let (( inhibit-read-only t ))
                (setq matched-text
                      (propertize
                       matched-text
                       'face (plist-get trigger-action :font))))))
          
          (setq recv-data
                (concat
                 (when (> (car matched-offsets) 0)
                   (substring recv-data 0 (car matched-offsets)))
                 matched-text
                 (when (< (cadr matched-offsets) (length recv-data))
                   (substring recv-data (cadr matched-offsets)))))
          (setq pos (+ (car matched-offsets) (length matched-text))))))))


;; BUILTIN TRIGGERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-trigger-url (url)
  (let (( map (make-sparse-keymap) ))
    (define-key map [mouse-1] 'mud-open-url-command)
    (propertize url 'mouse-face 'highlight 'keymap map 'url url 'help-echo
                                 "mouse-1: Open this URL in a browser")))

(defun mud-open-url-command (event)
  "Open the URL that was clicked on, either in Emacs or another
window (firefox)."
  (interactive "e")
  (let (window pos url)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos    (posn-point  (event-end event)))
      (if (not (windowp window))
          (error "Unknown URL link clicked"))
      (set-buffer (window-buffer window))
      ;;(goto-char pos)
      (setq url (get-text-property (1- pos) 'url )))
    (browse-url url)))


;; PROMPT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun emud-set-prompt (new-prompt-string)
  (interactive "sPrompt: \n")
  (overlay-put mud-local-input-overlay 'before-string new-prompt-string))

(defun emud-get-prompt ()
  (overlay-get mud-local-input-overlay 'before-string))

(defun emud-clear-prompt ()
  (interactive)
  (overlay-put mud-local-input-overlay 'before-string nil))


;; INPUT AREA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-input-echo (on)
  (overlay-put mud-local-input-overlay 'invisible (not on)))

(defun mud-input-submit ()
  (interactive)
  (when (and
         (not (string= (process-status mud-local-net-process) "closed"))
         (mud-inside-input-area-p))

    (let* ((inhibit-modification-hooks t) ; disables internal sticky hooks
           (user-input (mud-record-input-area)))
      (process-send-string mud-local-net-process (concat user-input "\n"))

      ;; Turn on sticky input hooks if local echo and sticky input is enabled.
      (when (and mud-local-echo mud-sticky-input)
        (mud-set-input-area user-input)
        (mud-input-sticky t)))))

(defun mud-record-input-area ()
  "Store the user's input area text into the connected buffer.

The user input that was previously in the input area becomes just
like the regular server output."
  (let* ( (user-input-beg    (overlay-start mud-local-input-overlay))
          (user-input-end    (overlay-end   mud-local-input-overlay))
          (user-input        (buffer-substring user-input-beg user-input-end))
          (user-inside-input (mud-inside-input-area-p)) )

;;    (message "DEBUG: mud-local-echo = %s" mud-local-echo)
    (if mud-local-echo
        (save-excursion
          (goto-char user-input-end)
          ;; insert-and-inherit?
          (insert-before-markers "\n")
          (add-text-properties user-input-beg
                               (1+ user-input-end)
                               '(read-only t rear-nonsticky t front-sticky t))
          ;; Input history stuff
          (setq mud-local-input-history
                (add-to-history 'mud-local-input-history
                                user-input mud-history-max)))
      (mud-clear-input-area))

    ;; Adjust our input area positions and mud output marker to go
    ;; past our new input...
    (let (( end-of-buffer (point-max) ))
      (set-marker (process-mark (get-buffer-process (current-buffer)))
                  end-of-buffer)
      (move-overlay mud-local-input-overlay end-of-buffer end-of-buffer
                    (current-buffer))
      (when user-inside-input (goto-char end-of-buffer)))

    user-input))

(defun mud-clear-input-area ()
  (delete-region (overlay-start mud-local-input-overlay)
                 (overlay-end mud-local-input-overlay)))

(defun mud-set-input-area (new-input)
  (let ((move-to-end (mud-inside-input-area-p)))
    (mud-clear-input-area)
    (save-excursion
      (goto-char (process-mark mud-local-net-process))
      (insert new-input))
    (when move-to-end
      (goto-char (point-max)))))

(defun mud-get-input-area ()
  (buffer-substring-no-properties
   (overlay-start mud-local-input-overlay)
   (overlay-end mud-local-input-overlay)))

(defun mud-inside-input-area-p ()
  (and (>= (point) (overlay-start mud-local-input-overlay))
       (<= (point) (overlay-end mud-local-input-overlay))))


;; STICKY INPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-input-sticky-edit-hook (overlay after start end &optional length)
  (unless after
    (mud-input-sticky nil)))

(defun mud-input-sticky-append-hook (overlay after start end &optional length)
  (unless after
    (let ((inhibit-modification-hooks t))
      (mud-input-sticky nil)
      (mud-clear-input-area))))

(defun mud-input-sticky (enable)
  (let ((mod-hook   (and enable '(mud-input-sticky-edit-hook)))
        (input-hook (and enable '(mud-input-sticky-append-hook))))
    (overlay-put mud-local-input-overlay 'modification-hooks mod-hook)
    (overlay-put mud-local-input-overlay 'insert-behind-hooks input-hook)
    (setq mud-local-sticky-input-flag nil)))

(defun emud-sticky-backspace (arg &optional killp)
  (interactive "*p\nP")
  (if mud-local-sticky-input-flag
      (progn
        (mud-set-input-area "")
        (mud-input-sticky nil))
    (backward-delete-char-untabify arg killp)))


;; MUD SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-load-file (directory filename)
  (unless (file-directory-p directory)
    (make-directory directory))
  (setq filename (replace-regexp-in-string "\\." "_" filename))
  (let (( path (concat (file-name-as-directory directory) filename ".el") ))
;;     (message "DEBUG: path = %s" path)
    (when (file-exists-p path)
      (unless (file-readable-p path)
        (error "EMUD config file %s exists, but does not have read permission"
               path))
      (load-file path))))

(defun clear-mud-settings ()
  (setq mud-local-triggers (copy-tree mud-builtin-triggers)))

(defun load-mud-settings (&optional hostname &optional username)
  (unless hostname
    (or (setq hostname mud-local-host-name)
        (error "No hostname is set, are we in an emud session buffer?")))
  (clear-mud-settings)
  (let ((host-dir (expand-file-name hostname mud-config-base)))
    (mud-load-file mud-config-base "global")
    (when hostname
      (mud-load-file mud-config-base hostname))
    (when username
      (mud-load-file host-dir username))))

(defun mud-hostname-to-filename (hostname)
  (replace-regexp-in-string "\\." "_" hostname))
  
(provide 'emud)
