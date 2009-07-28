;; Emacs MUD Client
;; by Justin Davis < jrcd83 ATAT gmail >
;;
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
;; The mud config buffer aims to be like configuring variables.  Using
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

(defcustom mud-settings-file "~/.emudrc"
  "File to evaluate, which sets the global and mud-specific
settings like triggers, aliases, etc."
  :type 'file
  :group 'emud)


;; VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mud-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r"       'mud-input-submit)
    (define-key map "\C-c\C-h" 'emud-send-input-history)
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
              (plist-put mud-output-text-props 'mud-color-intensity 1)))
    ("2"  . (lambda ()                 ; dim
              (plist-put mud-output-text-props 'mud-color-intensity 0)))
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

(defvar mud-settings
  '(( :GLOBAL . (( :triggers . (( "https?://[A-Za-z0-9./?=&;_]+" .
                                  mud-trigger-url )) )
                 ( :aliases  . () ))
              ))
  "Custom mud settings (i.e. triggers, aliases).  The list is
organized as an alist.  Global settings have the :GLOBAL key,
mud-specific settings have the mud hostname as the key.")

(defvar mud-server-filters
  '((mud-color-filter
     . "\033\\(?:\\[\\(?:\\([0-9;]*\\)\\([^0-9;]\\)?\\)?\\)?")
    (mud-telnet-filter . "\xFF\\([\xFB-\xFE].\\)"))

  "A list of filters.  Each filter is a cons cell with a regular
expression and a function to call if the expression matches.
Filters match special characters, remove them from the server output,
and change text properties setting `mud-output-text-props'.

No arguments are passed, instead the filter modifies the
recv-data variable in place.

Server filters search for regular expressions in the output
received from the MUD server and remove the text in place.  After
the filter runs the output is passed to the next filter.  After
all filters are checked, the output is written to the buffer.

Filters can throw a 'filter-resume symbol to abort filtering and
have the server's next output sent straight to the throwing
filter.  This is useful if the data the filter is parsing is
split across two socket receives.  For this reason filters must
return a nil value.")

(defvar mud-input-history-active nil
  "A global variable to hold our buffer local input history so
the minibuffer has access to it")

(defmacro read-from-minibuffer-default (prompt default)
  `(if (boundp ,default)
       (let ( minibuffer-input )
         (setq minibuffer-input
               (read-from-minibuffer (concat ,prompt
                                             (format " (default %s): "
                                                     (symbol-value ,default)))))
         (if (= 0 (length minibuffer-input))
             (symbol-value ,default) minibuffer-input))
     (read-from-minibuffer (concat ,prompt ": "))))
                                     

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
      (mud-client-message (replace-regexp-in-string "\n+$" "" event)))))

(defun mud-make-local-variables (hostname)
  (set (make-local-variable 'mud-host-name) hostname)
  (set (make-local-variable 'debug-on-error) 1)
  (set (make-local-variable 'mud-net-process)
       (get-buffer-process (current-buffer)))
  (set (make-local-variable 'mud-sticky-input-flag) nil)
  (set (make-local-variable 'mud-filter-continuation) nil)
  (set (make-local-variable 'mud-color-intensity) 1)
  (set (make-local-variable 'mud-local-echo) t)
  (make-local-variable 'mud-cached-triggers)
  (make-local-variable 'mud-cached-aliases)

;;  (message "DEBUG: mud-active-triggers = %s" mud-active-triggers)

  ;; Input History ---------------------------------------
  (set (make-local-variable 'mud-input-history) '())
  ;; -----------------------------------------------------
    
  ;; Text properties -------------------------------------
  (set (make-local-variable 'mud-output-text-props)
       (copy-tree mud-default-output-props))

  (set (make-local-variable 'tab-width) 8)
  (set (make-local-variable 'default-tab-width) 8)
  ;; -----------------------------------------------------


  ;; Override external variables
  (set (make-local-variable 'scroll-conservatively) 1000)
                                        ; set to a high number
  (buffer-disable-undo))                ; undo don't work good


(defun mud-client-message (message)
  "Send a message to the user from the MUD client."
  (save-excursion
    (goto-char (process-mark mud-net-process))
    (insert-before-markers
     (let (( prop-list mud-output-text-props ))
       (plist-put prop-list 'face "mud-client-message")
;;        (plist-put prop-list 'rear-sticky nil)
;;        (plist-put prop-list 'front-sticky nil)
       (apply 'propertize (format "*EMUD* says: \"%s\"\n" message)
              prop-list)))))


(defun mud-append-server-output (output)
  "Append text from the server before the input area."
  (save-excursion
    (goto-char (process-mark mud-net-process))
    (insert-before-markers
     (apply 'propertize output mud-output-text-props))))


;; COMMANDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun emud-connect (hostname port)
  (interactive "sHostname: \nnPort: ")

  (let* (( mud-name    (format "%s:%d" hostname port) )
         ( mud-buffer  (generate-new-buffer mud-name) )
         ( mud-process (make-network-process
                        :name             mud-name
                        :host             hostname
                        :service          port
                        :coding           'raw-text
                        :buffer           mud-buffer
                        :filter           'mud-filter
                        :filter-multibyte t
                        :sentinel         'mud-sentinel)) )

    (set-buffer mud-buffer)
    (let ( (default-major-mode 'mud-mode) )
      (set-buffer-major-mode mud-buffer))
    (mud-make-local-variables hostname)
    (cache-mud-settings hostname)

    ;; Overlay for user input area
    (set (make-local-variable 'mud-input-overlay)
         (make-overlay (point) (point) (current-buffer) nil t))
    (overlay-put mud-input-overlay 'field 'mud-input)
    (overlay-put mud-input-overlay 'non-rearsticky t)
    (overlay-put mud-input-overlay 'face "mud-input-area")
    (overlay-put mud-input-overlay 'invisible nil)

    (set-window-buffer (selected-window) mud-buffer)))

(defun mud-input-history-exit ()
  (setq mud-input-history-active nil)
  (remove-hook 'minibuffer-exit-hook 'mud-input-history-exit))

(defun emud-send-input-history (send-history)
  (interactive
   (if mud-input-history-active
       (error "Cannot browse input history in two frames at once")
     (progn
       (setq mud-input-history-active mud-input-history)
       (add-hook 'minibuffer-exit-hook 'mud-input-history-exit)
       (list
        (read-from-minibuffer "Resend: " (car mud-input-history-active) nil nil
                              '(mud-input-history-active . 1))))))
  (mud-set-input-area send-history)
  (mud-input-submit))

(defun emud-add-trigger (hostname regexp color)
  (interactive
   (list
    (read-from-minibuffer-default "MUD hostname" 'mud-host-name)
    (read-from-minibuffer "Trigger Pattern: ")
    (read-from-minibuffer "Trigger Color: ")))

  (unless (> (length hostname) 0)
    (error "You must specify a hostname to apply the filter to"))
  (unless (> (length regexp) 0)
    (error "You must specify a pattern the trigger should respond to"))
  (unless (> (length color) 0)
    (error "You must specify a valid color"))

  (let (mud-host-settings host-triggers)
    (setq mud-host-settings (get-settings-for-mud hostname))
    (setq host-triggers (cdr (assoc :triggers mud-host-settings)))
    (message "DEBUG: host-triggers = %s" host-triggers)

    (if (assoc regexp host-triggers)
        (progn
          (message "DEBUG: replacing existing trigger!")
          (setcdr (assoc regexp host-triggers) (list :foreground color)))
      (setcdr (assoc :triggers mud-host-settings)
              (cons (cons regexp (list :foreground color))
                    host-triggers))))

  (when (string= mud-host-name hostname)
    (cache-mud-settings hostname :triggers))
  (save-mud-settings))


;; FILTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar mud-telnet-codes
  '((?\xFF . :IAC)
    (?\xFE . :SE)
    (?\xFB . :WILL)
    (?\xFC . :WONT)
    (?\xFD . :DO)
    (?\xFE . :DONT)))

(defmacro mud-telnet-code (symbol)
  `(car (or () (rassq ,symbol mud-telnet-codes))))

(defvar mud-telnet-code-iac  "\xFF")
(defvar mud-telnet-code-se   "\xFE")
(defvar mud-telnet-code-will "\xFB")
(defvar mud-telnet-code-wont "\xFC")
(defvar mud-telnet-code-do   "\xFD")
(defvar mud-telnet-code-dont "\xFE")

(defun mud-telnet-confirm (accept-flag opt-code)
  "Uses process from `mud-filter'."
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
  (let* (( telnet-codes (match-string 1 recv-data) )
         ( code-symbols (mud-telnet-extract-codes telnet-codes) ))

;;    (message "DEBUG: parsed symbols: %s" code-symbols)
    (cond ((= (elt code-symbols 1) 1)
;;           (message "DEBUG: received echo code")
           (let (( echo (not (eq (car code-symbols) :WILL)) ))
;;             (message "DEBUG: echo = %s" echo)
             (mud-input-echo echo)
             (setq mud-local-echo echo)))))
  nil)

(defun mud-store-color-codes (color-codes)
  "Stores the mud color codes as the current color to use.
COLOR-CODES is the string of VT100/ANSI color codes, with ; as a
delimiter just as they are received.

The new colors are stored in `mud-output-text-props'."
  (let (( ansi-codes (split-string color-codes ";" t) ))
    (dolist (code ansi-codes)
      (let* ((code-entry (assoc-string code mud-ansi-color-codes))
             (code-action (if code-entry (cdr code-entry) nil)))

        ;; ansi color code keys can be either a font property or a
        ;; function
        (cond ((null code-action)       ; no match was found
               nil)

              ((functionp code-action)  ; action is a lambda
               (setq mud-output-text-props (funcall code-action)))

              (t                        ; action is a property list

                                        ; Choose a diff color if we
                                        ; are dim, bright, or
                                        ; normal...
               (let* ((color-intensity (plist-get mud-output-text-props
                                                  'mud-color-intensity))
                      (face-prop (list (car code-action)
                                       (nth color-intensity
                                            (cadr code-action)))))
                 ;; Preserve existing text properties
                 (if (plist-get 'face mud-output-text-props)
                     (plist-put (plist-get 'font mud-output-text-props)
                                (car face-prop) (cadr face-prop))
                   (plist-put mud-output-text-props 'face face-prop))
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

(defun mud-filter-result-sort (left right)
;;  (message "DEBUG sorting, left = %s -- right = %s" left right)
  (< (cadr left) (cadr right)))

(defun mud-filter-matches (filter-list)
  "Create a list of matched regexps, if any.  Checks each regexp.

FILTER-LIST is a list of filter pairs: ( REGEXP . FUNCTION ).
The idea being, if RJuanKa leaves west.
JuanKa arrives.
Dayman leaves east.
Dayman arrives.
EGEXP matches, FUNCTION is called.

A list of matches is returned in a special format.  The filter
is appended with the match data, as from `match-data'.

The result is a sorted associated list:
\( ( FUNCTION-SYMBOL . ( MATCH-DATA ) ), ... )
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
  (let (( match-regexp (cdr (assq (car filter) mud-server-filters)))
        ( match-list   (cdr filter)))
    ;; set preceding text to the old text properties
    (when (< pos (car match-list))
      (set-text-properties pos (car match-list)
                           mud-output-text-props recv-data))
    (set-match-data match-list)
    (when (catch 'mud-filter-continue (funcall (car filter)))
        ;; If the filter requested a continuation, store its the data.
      (setq mud-filter-continuation
            (substring recv-data pos (length recv-data)))
;;      (message "DEBUG: continuation: %s" mud-filter-continuation)
      (setq recv-data (substring recv-data 0 pos))
      (throw 'mud-filter-continue t))
    (set-match-data match-list)
    (setq recv-data (replace-match "" nil t recv-data))
    (car match-list)))

(defun mud-filter (process recv-data)
  "The master mud server output filter for the MUD connection/process.

Filters through the data by checking every filter in
`mud-server-filters' and filtering out unprintables with them.

Checks all mud server output for any trigger matches after that."
  (with-current-buffer (process-buffer process)

    ;; If a filter is continuing from before, prepend the old data.
    (when mud-filter-continuation
      (setq recv-data (concat mud-filter-continuation recv-data))
      (setq mud-filter-continuation nil))

    (catch 'mud-filter-continue
      (let (( pos 0 )
            ( matches (sort (mud-filter-matches mud-server-filters)
                            'mud-filter-result-sort))
            ( next-filter nil )
            ( match-regexp nil ))

        (while (> (length matches) 0)
;;          (message "DEBUG: matches = %s" matches)
          (setq next-filter (pop matches))
          (setq pos (mud-filter-helper pos next-filter))
          (setq match-regexp (cdr (assq (car next-filter) mud-server-filters)))

          ;; If this same filter matches again, make sure to put
          ;; it back in the results list, resorted.
          (when (string-match match-regexp recv-data pos)
            (setq matches
                  (sort (cons (cons (car next-filter) (match-data)) matches)
                        'mud-filter-result-sort))))

        ;; Set the text properties of leftover text after all filters.
        (when (< pos (1- (length recv-data)))
          (set-text-properties
           pos (length recv-data)
           mud-output-text-props recv-data))))

    (mud-triggers)

    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers recv-data))))


;; TRIGGERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-triggers ()
  "Check all mud server output for text matching triggers.

This function takes no parameters, but uses the recv-data
parameter from `mud-filter', where it is called from.

Each trigger is a cons cell with a REGEXP and corresponding action:
\( REGEXP . ACTION )

If the first element of the action is a symbol or lambda the
ACTION function is evaluated.  Arguments to the ACTION are the matching
text and the list of match results.  The match results are adjusted
to be relative to the matching text, NOT the entire recv-data string.

The function must return the new string to replace the match with.

Otherwise, the action is assumed to be text properties which are
then applied to the entire regexp match.
"
  ;;  (message "DEBUG: mud-cached-triggers = %s" mud-cached-triggers)

  (let (( pos 0 ))
    (dolist (trigger mud-cached-triggers)
      (while (string-match (car trigger) recv-data pos)
        (cond ((symbolp (cdr trigger)) ; trigger is a symbol or lambda
               (let ( trigger-result   ; hopefully a function...
                      orig-match-data
                      trigger-match-data
                      match-start )
                 (setq orig-match-data (match-data)
                       match-start     (car orig-match-data)
                       trigger-match-data ; make relative to matched string
                       (mapcar (lambda (pos) (- pos match-start))
                               trigger-match-data))
                 (setq trigger-result
                       (funcall (cdr trigger)
                                (substring recv-data
                                           (car orig-match-data)
                                           (cadr orig-match-data))
                                trigger-match-data))
                 (when (stringp trigger-result)
                   (setq recv-data
                         (replace-match trigger-result t nil recv-data)))
                 (setq pos (+ (car orig-match-data)
                     (length trigger-result)))))
               
              ((listp (cdr trigger))
               (let (( inhibit-read-only t ))
                 (add-text-properties (match-beginning 0)
                                      (match-end 0)
                                      (list 'face (cdr trigger))
                                      recv-data))
               (setq pos (+ 1 (match-end 0))))

              (t (error "Unknown trigger action type for trigger %s"
                        trigger)))))))

;; BUILTIN TRIGGERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-trigger-url (url match-list)
  (let (( map (make-sparse-keymap) ))
    (define-key map [mouse-1] 'mud-open-url-command)
    (add-text-properties 0 (length url)
                         (list 'mouse-face 'highlight 'keymap map 'url url
                               'help-echo "mouse-1: Open this URL in a browser")
                         url))
  url)

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


;; INPUT AREA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-input-echo (on)
  (overlay-put mud-input-overlay 'invisible (if on nil t)))

(defun mud-input-submit ()
  (interactive)
  (when (and
         (not (string= (process-status mud-net-process) "closed"))
         (mud-inside-input-area-p))

    ;; We don't want to trigger our own sticky hooks
    (let* ((inhibit-modification-hooks t)
           (user-input (mud-record-input-area)))
      (process-send-string mud-net-process (concat user-input "\n"))
      (when mud-sticky-input
        (mud-set-input-area user-input)
        (mud-input-stick))

      ;; Input history stuff
      (setq mud-input-history
            (add-to-history 'mud-input-history user-input mud-history-max)))))

(defun mud-record-input-area ()
  "Store the user's input area text into the connected buffer."
  (let* ( (user-input-beg    (overlay-start mud-input-overlay))
          (user-input-end    (overlay-end   mud-input-overlay))
          (user-input        (buffer-substring user-input-beg user-input-end))
          (user-inside-input (mud-inside-input-area-p)) )
;;    (message "DEBUG: mud-local-echo = %s" mud-local-echo)
    (if mud-local-echo
        (save-excursion
          (goto-char user-input-end)
          (insert-before-markers-and-inherit "\n")
          (add-text-properties user-input-beg (1+ user-input-end)
                               '(read-only t rear-nonsticky t front-sticky t)))
      (delete-region user-input-beg user-input-end))
    (move-overlay mud-input-overlay (point-max) (point-max) (current-buffer))
    (set-marker (process-mark mud-net-process) (point-max))
    (when user-inside-input (goto-char (point-max)))
    user-input))

(defun mud-clear-input-area ()
  (delete-region (overlay-start mud-input-overlay)
                 (overlay-end mud-input-overlay)))

(defun mud-set-input-area (new-input)
  (let ((move-to-end (mud-inside-input-area-p)))
    (mud-clear-input-area)
    (save-excursion
      (goto-char (process-mark mud-net-process))
      (insert new-input))
    (when move-to-end
      (goto-char (point-max)))))

(defun mud-inside-input-area-p ()
  (and (>= (point) (overlay-start mud-input-overlay))
       (<= (point) (overlay-end mud-input-overlay))))

;; STICKY INPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mud-input-sticky-off ()
  (overlay-put mud-input-overlay 'modification-hooks nil)
  (overlay-put mud-input-overlay 'insert-behind-hooks nil)
  (setq mud-sticky-input-flag nil))

(defun mud-input-sticky-edit-hook (overlay after start end &optional length)
  (unless after
    (mud-input-sticky-off)))

(defun mud-input-sticky-append-hook (overlay after start end &optional length)
  (unless after
    (let ((inhibit-modification-hooks t))
      (mud-input-sticky-off)
      (mud-clear-input-area))))

(defun mud-input-stick ()
  (overlay-put mud-input-overlay
               'modification-hooks '(mud-input-sticky-edit-hook))

  (overlay-put mud-input-overlay
               'insert-behind-hooks '(mud-input-sticky-append-hook))
  (setq mud-sticky-input-flag t))


;; MUD SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-settings-for-mud (hostname)
  "Retrieves the settings alist for HOSTNAME.  If the hostname is
not in the `mud-settings' alist, creates an entry for it."
;;  (load-mud-settings)
  (unless (assoc hostname mud-settings)
    (setq mud-settings
          (cons `(,hostname . ((:triggers . ())
                               (:aliases  . ())))
                mud-settings)))

  (cdr (assoc hostname mud-settings)))


(defun cache-mud-settings (mud-hostname &optional setting-type)
  (if (not setting-type)
      (progn
        (cache-mud-settings mud-hostname :triggers)
        (cache-mud-settings mud-hostname :aliases))

    (let (loaded-settings global-settings)
      (setq loaded-settings (cdr (assoc setting-type
                                        (get-settings-for-mud mud-hostname))))
      (setq global-settings (cdr (assoc setting-type
                                        (cdr (assq :GLOBAL mud-settings)))))
;;       (message "DEBUG: loaded-settings = %s\nDEBUG: global-settings = %s"
;;                loaded-settings global-settings)
      (dolist (setting-pair global-settings)
        (unless (assoc (car setting-pair) loaded-settings)
          (setq loaded-settings (cons setting-pair loaded-settings))))

      (cond ((eq setting-type :triggers)
             (setq mud-cached-triggers loaded-settings))
            ((eq setting-type :aliases)
             (setq mud-cached-aliases loaded-settings)))

      loaded-settings)))

(defun load-mud-settings ()
  (when (file-exists-p mud-settings-file)
    (unless (file-readable-p mud-settings-file)
      (error "EMUD settings file %s exists, but does not have read permission"
             mud-settings-file))
    (let* (( settings-buffer (generate-new-buffer "EMUD Settings") ))
      (with-current-buffer settings-buffer
        (insert-file-contents-literally mud-settings-file))
      (setq mud-settings (read settings-buffer))
      (kill-buffer settings-buffer))))

(defun save-mud-settings ()
  (let* (( settings-buffer (generate-new-buffer "EMUD Settings") ))
    (print mud-settings settings-buffer)
    (with-current-buffer settings-buffer
      (write-file mud-settings-file))
    (kill-buffer settings-buffer)))


;; MUD CONFIG BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun emud-config (host-name)
  (interactive
   (list
    (if (boundp 'mud-host-name) mud-host-name
      (read-from-minibuffer "MUD hostname: "))))
  
  (let ( emud-config-buffer )
    (setq emud-config-buffer (get-buffer-create "*EMUD Config*"))
    (pop-to-buffer emud-config-buffer t)
    (set (make-local-variable 'mud-config-host-name) host-name)
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

(defun emud-config-save ()
  )

(defun emud-config-draw ()
  (require 'widget)
  (require 'wid-edit)
  (setq buffer-read-only nil)
  (let (( inhibit-read-only t))
    (erase-buffer))
  (let (mud-cfg triggers)
    (setq mud-cfg (get-settings-for-mud mud-config-host-name))
    (setq triggers (cdr (assq :triggers mud-cfg)))
    (widget-insert "==================================[ TRIGGERS ]================================\n")

    (if (= 0 (length triggers))
        (widget-insert " *EMPTY*\n")
      (dotimes (i (length triggers))
        (let (( trigger (elt triggers i) ))
          (widget-create
           'editable-field
           :tag "Match"
           :size 78
           :value (car trigger))

          (widget-insert "\n")

          (widget-create
           'menu-choice
           :indent 7
           :tag "Action"
           :choice "Color"
           ;;              :choice (cond ((symbolp (cdr trigger)) "Function")
           ;;                            ((listp   (cdr trigger))
           ;;                             (propertize "Color" 'face (cdr trigger)))
           ;;                            (t        "ERROR!"))

           `(checklist
             :indent 23
             :tag "Color"
             :format "%t (%{sample%}) %v"
             :sample-face ,(if (listp (cdr trigger))
                               (cdr trigger)
                             '())
             (editable-field
              :tag "Foreground"
              :format "%t: %v\n"
              :size 10)
             (editable-field
              :tag "Background"
              :format "%t: %v\n"
              :size 10))
           '(editable-field
             :tag "Response"
             :format "%t %v\n"
             :size 61)
           '(choice-item "Function"))))))

  (widget-insert "==================================[ TRIGGERS ]================================\n")
  (widget-setup))

(load-mud-settings)

(provide 'emud)

; EOF
