(require 'timer)
(require 'ring)
(require 'json)
(eval-when-compile (require 'cl))

(eval-when-compile
  (defmacro wlet (binding &rest body)
    "Like `let', but adds a null check. Accepts only one binding."
    (declare (indent defun))
    `(lexical-let (,binding)
       (when ,(car binding) ,@body)))
  (defun unkeywordify (sym)
    "Like `symbol-name' but drops leading colon."
    (let ((str (symbol-name sym)))
      (if (eq (aref str 0) ?:) (substring str 1 (length str)) str)))
  (defmacro hget (table key)
    "A macro for making hashtable access easier for hard-coded keys.
Like `gethash', but expects KEY to be a symbol that it converts
to a string. If KEY is a keyword then the leading colon is
dropped from the string. Does nothing if TABLE is nil."
    (lexical-let ((var (make-symbol "ht")))
      `(wlet (,var ,table) (gethash ,(unkeywordify key) ,var))))
  (defmacro hset (table key val)
    "Like `puthash' but expects KEY to be a symbol as with `hget'."
    `(puthash ,(unkeywordify key) ,val ,table))
  (defmacro dbind (args expr &rest body)
    "Identical to `destructuring-bind'."
    (declare (indent defun))
    `(destructuring-bind ,args ,expr ,@body))
  (defmacro wbind (args expr &rest body)
    "Like `destructuring-bind', except does nothing if EXPR is nil."
    (declare (indent defun))
    (let ((var (make-symbol "expr")))
      `(wlet (,var ,expr)
         (dbind ,args ,var ,@body))))
  (defmacro hbind (args hashtable &rest body)
    "Binds each member of ARGS to the entry in HASHTABLE with the same name."
    (declare (indent defun))
    (let ((hvar (make-symbol "hash")))
      `(let ((,hvar ,hashtable))
         (let ,(loop for arg in args collect `(,arg (hget ,hvar ,arg)))
           ,@body))))
  (defmacro acond (&rest clauses)
    "Anaphoric macro. Like `cond', but binds each condition to IT."
    `(lexical-let ((it nil))
       (cond ,@(loop for (test . forms) in clauses
                     collect (cons (if (eq test t) t `(setf it ,test))
                                   forms)))))
  (defmacro pget (plist prop)
    "Like `plist-get'."
    `(second (memq ,prop ,plist)))
  (defmacro pset (plist prop val)
    "Like `plist-put', but throws an error if the property doesn't exist."
    (let ((var (make-symbol "plist")))
      `(lexical-let ((,var (memq ,prop ,plist)))
         (cond ((null ,var) (error "Property %s doesn't exist" ,prop))
               (t (setf (second ,var) ,val)))))))

(defgroup numen nil
  "Numen REPL"
  :prefix "numen-"
  :group 'tools)

(defface numen-backtrace-face '((t (:inherit font-lock-variable-name-face))) "Face for backtraces in Numen.")
(defface numen-backtrace-bold-face '((t (:inherit numen-backtrace-face :weight bold))) "Bold face for backtraces in Numen.")
(defface numen-boolean-face '((t (:inherit font-lock-builtin-face))) "Face for displaying boolean results in Numen.")
(defface numen-breakpoint-face '((t (:inherit font-lock-warning-face))) "Face for breakpoint icon in fringe.")
(defface numen-breaklist-face '((t (:inherit font-lock-comment-face))) "Face for breakpoints in Numen.")
(defface numen-console-face '((t (:inherit font-lock-doc-face))) "Face for displaying eval process console output in Numen.")
(defface numen-date-face '((t (:inherit font-lock-type-face))) "Face for displaying date values in Numen.")
(defface numen-omitted-face '((t (:inherit numen-null-face))) "Face for printing missing or hidden data in Numen.")
(defface numen-omitted-italic-face '((t (:inherit numen-omitted-face :slant italic))) "Like `numen-omitted-face' but italic.")
(defface numen-error-face '((t (:inherit font-lock-warning-face))) "Face for errors in Numen.")
(defface numen-error-message-face '((t (:inherit escape-glyph))) "Face for error message in Numen.")
(defface numen-frame-arrow-face '((t (:inherit font-lock-function-name-face))) "Face for displaying arrow icon next to a stack frame location in the Numen debugger.")
(defface numen-frame-arrow-face-top '((t (:inherit font-lock-variable-name-face))) "Face for displaying arrow icon next to the location of the top stack frame in the Numen debugger.")
(defface numen-function-face '((t (:inherit font-lock-function-name-face))) "Face for displaying function results in Numen.")
(defface numen-info-face '((t (:inherit font-lock-variable-name-face))) "Face for informational output in Numen.")
(defface numen-null-face '((t (:inherit shadow))) "Face for displaying null in Numen.")
(defface numen-number-face '((t (:inherit font-lock-constant-face))) "Face for displaying numeric results in Numen.")
(defface numen-object-key-face '((t (:inherit font-lock-keyword-face))) "Face for displaying object keys in Numen.")
(defface numen-prompt-face '((t (:inherit font-lock-keyword-face))) "Face for the Numen prompt.")
(defface numen-string-face '((t (:inherit font-lock-string-face))) "Face for displaying string results in Numen.")

(eval-and-compile
  (defvar numen-directory
    (let ((name (or (locate-library "numen") load-file-name)))
      (file-name-directory name))))

(eval-and-compile
  (unless window-system
    (defun define-fringe-bitmap (&rest args))))

(define-fringe-bitmap 'breakpoint "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")

(defun* numen-assign-common-bindings (&optional (map numen-common-keymap))
  (define-key map (kbd "C-c c") 'numen-debugger-continue)
  (define-key map (kbd "C-c t") 'numen-debugger-backtrace)
  (define-key map (kbd "C-c i") 'numen-debugger-step-in)
  (define-key map (kbd "C-c n") 'numen-debugger-step-next)
  (define-key map (kbd "C-c u") 'numen-debugger-step-out)
  (define-key map (kbd "C-c l") 'numen-debugger-locals)
  (define-key map (kbd "C-c b") 'numen-toggle-breakpoint)
  (define-key map (kbd "C-c p") 'numen-list-breakpoints)
  (define-key map (kbd "C-c e") 'numen-toggle-break-on-exception)
  (define-key map (kbd "C-c .") 'numen-cycle-window)
  map)

(defvar numen-common-keymap (numen-assign-common-bindings (make-sparse-keymap)))

(defun* numen-assign-major-mode-bindings (&optional (map numen-mode-map))
  (set-keymap-parent map numen-common-keymap)
  (define-key map (kbd "RET") 'numen-return)
  (define-key map (kbd "M-p") 'numen-previous-input)
  (define-key map (kbd "M-n") 'numen-next-input)
  (define-key map (kbd "C-a") 'numen-move-beginning-of-line)
  (define-key map (kbd "M-m") 'numen-back-to-indentation)
  (define-key map (kbd "C-c C-u") 'numen-kill-input)
  (define-key map (kbd "C-c C-o") 'numen-clear-output)
  (define-key map (kbd "C-c M-o") 'numen-clear-repl-buffer)
  (define-key map (kbd "C-c C-n") 'numen-goto-next-prompt)
  (define-key map (kbd "C-c C-p") 'numen-goto-previous-prompt)
  (define-key map (kbd "C-c C-c") 'numen-restart)
  (define-key map (kbd "C-c C-x") 'numen-exit)
  map)

(defvar numen-mode-map (numen-assign-major-mode-bindings (make-sparse-keymap)))

;;; global vars
(defvar numen-breakpoints nil)
(defvar numen-counter 0 "Integer that is incremented every time Numen needs a unique value.")
(defvar numen-default-host "localhost" "The server that Numen will try to connect to by default when it isn't creating a child process.")
(defvar numen-default-repl-buffer nil)
(defvar numen-default-value-depth 2 "Depth at which Numen will print an ellipsis rather than display the elements of an array or object.")
(defvar numen-input-ring-size 100)
(defvar numen-print-wrap 80 "Maximum number of characters a printed object may require before being broken into multiple lines in the REPL.")
(defvar numen-default-port 7777 "The port that Numen will use to connect to the eval server if the user doesn't specify one.")

(defvar numen-repl-message-hook nil
  "List of functions of one argument MSG that the Numen client
calls with any message from the eval process that it doesn't
recognize. The first one to return non-nil is considered to have
handled the message.")

(defvar numen-startup-hook nil "Hook to call after connecting to the eval process.")

;;; for all buffers
(defvar numen-buffer-id nil "Unique ID assigned to each Numen buffer so that eval process messages can be sent to the right place.")
(defvar numen-preferred-window nil
  "Buffer-local variable. When set, Numen will always choose this
window to display the buffer when it isn't already showing.")

;;; for repl buffer
(defvar numen-call-stacks nil "List of call stacks representing possibly nested debugger breaks.")
(defvar numen-evalproc nil "The process that Numen asks to evaluate expressions.")
(defvar numen-evals nil "Hash table of up to `numen-max-stored-eval-results' evaluation results, keyed by ID received from server.")
(defvar numen-host nil "The server the eval process is running on. NIL when Numen creates the eval process.")
(defvar numen-input-compiler nil "A function through which Numen passes source code for evaluation, sending what it returns to the eval process.")
(defvar numen-input-ring nil)
(defvar numen-input-ring-index nil)
(defvar numen-lumen-p nil "When true, Numen sends code to a Lumen process; when false, to a Node.js process.")
(defvar numen-markers nil)
(defvar numen-max-stored-eval-results nil)
(defvar numen-port nil "Port by which Numen connects to a remote eval process.")
(defvar numen-pre-debugging-state nil "Stuff we want to restore when exiting the debugger, like the prior window configuration.")
(defvar numen-search-prefix nil)
(defvar numen-selected-frame-ids nil "A list of the same length as `numen-call-stacks' containing the id of the currently selected frame from each stack.")
(defvar numen-views nil "Hash table storing view data for the values in `numen-evals'.")

;;; for secondary Numen buffers
(defvar numen-secondary-p nil)
(defvar numen-repl-buffer nil)
(defvar numen-script nil)

;;; for Numen minor mode
(defvar numen-made-readonly nil)

(define-derived-mode numen-mode nil "Numen"
  "Major mode for a Lumen REPL.

\\{numen-mode-map}"
  (set (make-local-variable 'numen-buffer-id) (incf numen-counter))
  (set (make-local-variable 'numen-call-stacks) nil)
  (set (make-local-variable 'numen-evalproc) nil)
  (set (make-local-variable 'numen-evals) (make-hash-table))
  (set (make-local-variable 'numen-input-ring) (make-ring numen-input-ring-size))
  (set (make-local-variable 'numen-input-ring-index) -1)
  (set (make-local-variable 'numen-markers) (make-hash-table))
  (set (make-local-variable 'numen-max-stored-eval-results) 20)
  (set (make-local-variable 'numen-pre-debugging-state) nil)
  (set (make-local-variable 'numen-search-prefix) nil)
  (set (make-local-variable 'numen-selected-frame-ids) nil)
  (set (make-local-variable 'numen-preferred-window) (selected-window))
  (set (make-local-variable 'numen-views) (make-hash-table)))

(defun* numen-assign-minor-mode-bindings (&optional (map numen-minor-mode-map))
  (set-keymap-parent map numen-common-keymap)
  (define-key map (kbd "C-c C-l") 'numen-load-file)
  map)

(defvar numen-minor-mode-map (numen-assign-minor-mode-bindings (make-sparse-keymap)))

(define-minor-mode numen-minor-mode
  "Minor mode to enable Numen for Javascript source files.
Enables such things as toggling breakpoints and displaying
stack frames while debugging.

\\{numen-minor-mode-map}"
  :lighter " Numen")

(add-hook 'numen-minor-mode-hook (lambda nil (if numen-minor-mode (numen-enter-minor-mode) (numen-exit-minor-mode))))

(defun run-numen (&optional hostport compiler lumenp)
  (interactive)
  (let ((invocation-directory default-directory))
    (with-current-buffer (get-buffer-create "*Numen*")
      (add-hook 'kill-buffer-hook 'numen-stop nil t)
      (setq numen-default-repl-buffer (current-buffer))
      (when numen-evalproc
        (numen-stop))
      (numen-enter-mode)
      (set (make-local-variable 'numen-host) nil)
      (set (make-local-variable 'numen-port) nil)
      (when hostport ; tempdg: make sure works with empty prefix arg
        (dbind (host port) (numen-parse-host-and-port hostport)
          (setq numen-host (or host numen-default-host)
                numen-port (or port numen-default-port))))
      (set (make-local-variable 'numen-input-compiler) (or compiler 'identity))
      (set (make-local-variable 'numen-lumen-p) lumenp)
      (numen-insert-prompt)
      (let ((default-directory invocation-directory))
        (numen-start))
      (numen-switch-to-repl))))

(defun numen-enter-mode ()
  (let ((ring numen-input-ring))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (numen-mode)
    (when (ring-p ring)
      (setq numen-input-ring ring))))

(defun numen-start ()
  (cond (numen-host (numen-connect))
        (t (numen-launch))))

(defun numen-restart ()
  (interactive)
  (numen-stop)
  (numen-start))

;;;; process

(defun numen-find-buffer-by-id (id)
  (loop for buffer in (buffer-list)
        when (equal id (buffer-local-value 'numen-buffer-id buffer))
        do (return buffer)))

(defmacro with-numen-buffer-by-id (id &rest body)
  (declare (indent defun))
  (let ((var (make-symbol "buf")))
    `(let ((,var (numen-find-buffer-by-id ,id)))
       (cond ((null ,var) (message "Numen buffer %s not found." ,id) nil)
             (t (with-current-buffer ,var ,@body))))))

(defmacro in-repl-p ()
  'numen-markers)

(eval-and-compile
  (defun numen-find-repl-buffer ()
    (cond ((not (buffer-live-p numen-default-repl-buffer))
           ;; there are no repl buffers
           nil)
          ((in-repl-p)
           ;; you are a repl buffer
           (current-buffer))
          ((buffer-live-p numen-repl-buffer)
           ;; you are associated with a repl buffer (you're a process
           ;; buffer, a script buffer, or an inspector buffer)
           numen-repl-buffer)
          (t numen-default-repl-buffer))))

(defmacro with-repl-buffer (&rest body)
  (let ((repl (make-symbol "replbuf")))
    `(let ((,repl (numen-find-repl-buffer)))
       (cond ((null ,repl) (message "Numen is not running"))
             (t (with-current-buffer ,repl
                  (when (get-buffer-window)
                    (setq numen-preferred-window (get-buffer-window)))
                  ,@body))))))

(defmacro with-repl-buffer-when-debugging (&rest body)
  `(with-repl-buffer
    (cond (numen-call-stacks ,@body)
          (t (message "Not in the debugger")))))

(defmacro in-debugger-p ()
  '(with-repl-buffer numen-call-stacks))

(defun numen-launch ()
  "Launch an eval server as a child process and communicate with
it using stdin and stdout. Called when `numen-host' is NIL."
  (when numen-lumen-p
    (setenv "LUMEN_HOST" "node --expose_debug_as=v8debug"))
  (let* ((process-connection-type nil) ; use a pipe, not a PTY. see Emacs documentation
         (buf (generate-new-buffer " *numen-childproc*")))
    (numen-init-evalproc
     (if numen-lumen-p
         (apply 'start-process "lumen" buf "lumen"
                (list (concat numen-directory "numen.js") "-e" "(launchNumen 'lumen)"))
       (apply 'start-process "node" buf "node"
              (list "--expose_debug_as=v8debug" "-e"
                    (format "require('%snumen.js');launchNumen('js')" numen-directory)))))
    ;; tempdg: since start-process is async, put this elsewhere, like for socket
    (run-hooks 'numen-startup-hook)))

(defun numen-connect ()
  "Connect to an already-running eval process at `numen-host' via
`numen-port'. The connection is stored in `numen-socket'."
  (let* ((ignore (message "Connecting to eval process at %s:%s..." numen-host numen-port))
         (process-connection-type nil) ; use a pipe, not a PTY. see Emacs documentation
         (buf (generate-new-buffer " *numen-socket*")))
    (numen-init-evalproc
     (open-network-stream "numen-socket" buf numen-host numen-port :nowait t))))

(defun numen-init-evalproc (proc)
  (set-process-sentinel proc 'numen-evalproc-sentinel)
  (set-process-filter proc 'numen-evalproc-filter)
  (set-process-query-on-exit-flag proc nil)
  (set-process-coding-system proc 'utf-8 'utf-8)
  (setq numen-evalproc proc)
  (with-current-buffer buf
    (set (make-local-variable 'numen-buffer-id) (incf numen-counter))
    (set (make-local-variable 'numen-repl-buffer) (with-repl-buffer (current-buffer)))))

(defun numen-evalproc-sentinel (proc message)
  (with-repl-buffer
   (cond ((null numen-host)
          (message "Numen child process quit unexpectedly: %s" (numen-strip-newlines message))
          (numen-stop)
          (setq mode-name "Numen:aborted"))
         ((string-match "^open" message)
          (message "Numen is connected.")
          (run-hooks 'numen-startup-hook))
         ((or (string-match "^failed" message)
              (string-match "connection broken" message))
          (with-repl-buffer
           (numen-stop)
           (setq mode-name "Numen:aborted")))
         (t (message "Numen socket: %s" (numen-strip-newlines message))))))

(defun numen-evalproc-filter (proc string)
  "Write a STRING received from the eval process to the eval process
buffer, then call a handler to read and act on any complete messages."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (numen-handle-eval-messages)))

;;;; communication with eval process

(defun numen-send-request (req &optional stuff buffer-id)
  (unless buffer-id (setq buffer-id numen-buffer-id))
  (if (numen-process-running-p numen-evalproc)
      (let ((json (json-encode (append req (list :stuff (append stuff (list :buffer buffer-id)))))))
        (cond ((in-debugger-p) ; debugger calls are synchronous, so don't
               ;; require length encoding
               (process-send-string numen-evalproc json))
              (t (let ((msg (format "%s%s\n" (length json) json))) ; length encoded
                   (process-send-string numen-evalproc msg)))))
    (numen-output "Not connected.\n" 'numen-info-face)))

(defun numen-read-eval-message ()
  "Extract the next complete message, if one exists, from the
eval process buffer. Messages may be either JSON or plain text."
  (let ((start (point-min)))
    (goto-char start)
    (cond ((search-forward "\0" nil t) ; JSON messages begin with this guard character
           (cond ((= (point) (1+ start)) (numen-read-json))
                 (t ;; there's something before the first JSON
                  ;; message. extract it as text and pick the JSON up
                  ;; during the next turn of the read loop
                  (delete-and-extract-region start (1- (point))))))
          ((> (point-max) start)
           ; there is no JSON message. extract as text
           (delete-and-extract-region start (point-max))))))

(defun numen-read-json ()
  (let ((start (point)))
    (when (search-forward "{" nil t)
      (backward-char)
      (let* ((header (buffer-substring-no-properties start (point)))
             (need (string-to-number header)))
        (when (and (> need 0) (>= (- (buffer-size) (point)) need))
          (let* ((json (buffer-substring-no-properties (point) (+ (point) need)))
                 (json-object-type 'hash-table)
                 (value (json-read-from-string json)))
            (goto-char (+ (point) need))
            (assert (= 10 (char-after)) nil "Newline not present after JSON message from server.")
            (delete-region (point-min) (1+ (point)))
            value))))))

;;;; requests

(defun numen-request-evaluation (src break-p)
  (with-repl-buffer
   (numen-send-request (append (list :evaluate src)
                               (acond ((numen-selected-frame-index) (list :frame_index it))
                                      (break-p (list :break_ t)))))))

(defun numen-request-details (id needs spot)
  (let ((buffer-id numen-buffer-id))
    (with-repl-buffer
     (numen-send-request (list :id id :needs (mapcar #'numen-plist-to-hash needs))
                         (list :spot spot) buffer-id))))

(defun numen-request-load-file (script)
  (with-repl-buffer
   (let ((bp-table (numen-breakpoints-as-hashtable)))
     (numen-send-request (append (list :load script) (when bp-table (list :breakpoints bp-table)))))))

(defun numen-request-source (script line column goto)
  (with-repl-buffer
   (numen-send-request (list :source script)
                       (list :script script :line line :column column :goto goto))))

(defun numen-request-breakpoint-state (state script line goto)
  (with-repl-buffer
   (numen-send-request (list :breakpoint (list :script script :line line :state state))
                       (when goto (list :goto goto)))))

(defun numen-request-breakpoints ()
  (with-repl-buffer
   (numen-send-request (list :breakpoints (numen-breakpoints-as-hashtable)))))

(defun numen-request-step-action (action)
  (with-repl-buffer-when-debugging
   (numen-send-request (list :run action))))

(defun numen-debugger-continue ()
  (interactive)
  (numen-request-step-action :continue)
  (numen-end-debugging-session))

(defun numen-debugger-step-next () (interactive) (numen-request-step-action "StepNext"))
(defun numen-debugger-step-in () (interactive) (numen-request-step-action "StepIn"))
(defun numen-debugger-step-out () (interactive) (numen-request-step-action "StepOut"))

(defun numen-debugger-locals ()
  (interactive)
  (with-repl-buffer-when-debugging
   (acond ((numen-selected-frame-index) (numen-send-request (list :locals t :frame_index it)))
          (t (message "No selected frame")))))

;;;; responses

(defun numen-handle-eval-messages ()
  "Read and process messages sent by the eval process."
  (let ((msg nil))
    (while (setq msg (numen-read-eval-message))
      (acond ((stringp msg) (numen-output msg 'numen-console-face))
             ((loop for handler in numen-repl-message-hook do
                    (when (funcall handler msg) (return t)))
              nil)
             ((hget msg :evaluation) (numen-report-evaluation it))
             ((hget msg :supplement)
              (hbind (spot buffer) (hget msg :stuff)
                (numen-handle-supplement (hget msg :id) it (numen-listify spot) buffer)))
             ((hget msg :loaded) (message "Loaded %s" it))
             ((hget msg :source)
              (hbind (script line column goto) (hget msg :stuff)
                (numen-handle-source it script line column goto)))
             ((hget msg :breakpoints)
              (hbind (status failure) msg
                (numen-handle-breakpoints it status failure (hget (hget msg :stuff) :at))))
             ((hget msg :break) (numen-handle-break it (hget msg :e)))
             ((hget msg :resuming) (numen-handle-resuming))
             ((hget msg :in-place) (numen-handle-in-place-output it (hget msg :preserve-p)))
             ((hget msg :emacs-eval) (numen-handle-emacs-eval-request it (hget msg :callback-id)))
             ((hget msg :status-message) (message it))
             (t (let ((str (format "Unrecognized repl message: %s\n" (json-encode msg))))
                  (numen-output str 'numen-error-face)))))))

(defmacro vkey (val) "Printed key representing the path to VAL." `(gethash "vkey" ,val))
(defmacro id (scope) "ID number of the evaluation that SCOPE is part of." `(car (last ,scope)))

(defun numen-report-evaluation (evaluation)
  "Extract the data and metadata from an EVALUATION sent by the
eval process, save it for the user to inspect later, and display
it appropriately in the Numen buffer."
  (with-repl-buffer
   (hbind (value id breaking) evaluation
     (hbind (locals truelen) value
       (cond ((and locals (= 0 (or truelen 0)))
              (message "No local variables"))
             (t (numen-store-val value id)
                (with-output-before-prompt
                 (when breaking
                   (numen-insert "Throwing "))
                 (let ((view (setf (gethash id numen-views) (numen-make-view))))
                   (numen-insert-value id view)))))))))

(defun numen-handle-supplement (id supplement spot buffer-id)
  (assert buffer-id)
  (with-numen-buffer-by-id buffer-id
    (assert numen-evals)
    (cond ((equal supplement "deleted")
           (message "Eval process no longer has value %s" id))
          (t (loop for rec across supplement do
                   (hbind (scope details from fold-level) rec
                     (wlet (val (numen-find-val-or-report-deletion (numen-listify scope)))
                       (numen-splice-details val details from)
                       (when fold-level
                         (numen-fold id val fold-level)))))
             (numen-reinsert-value id)
             (apply #'numen-goto-spot spot)))))

(defun numen-handle-source (source script line column goto)
  (with-repl-buffer
   (message "Loaded %s" script)
   (numen-update-secondary-source-buffer script source)
   (when line
     (numen-jump-to-script script line column goto))))

(defun numen-handle-breakpoints (server-breakpoints status failure goto)
  (with-repl-buffer
   (cond ((equal status "now-on") (message "Set breakpoint"))
         ((equal status "now-off") (message "Cleared breakpoint"))
         ((equal status "already-on") (message "Breakpoint was already set in V8"))
         ((equal status "already-off") (message "Breakpoint did not exist in V8"))
         (t (let ((str (or failure "Unrecognized response to breakpoint request")))
              (numen-output (concat str "\n") 'numen-error-face))))
   (numen-bind-breakpoints server-breakpoints)
   (when goto (goto-char goto))))

(defun numen-handle-break (server-stack e)
  (with-repl-buffer
   (unless numen-pre-debugging-state
     (setq numen-pre-debugging-state (list :window-config (current-window-configuration))))
   (numen-update-mode-line t)
   (when e
     (numen-report-evaluation e))
   (when (> (length server-stack) 0)
     (numen-push-call-stack (numen-listify server-stack))
     (let ((f (caar numen-call-stacks)))
       (numen-jump-to-script (hget f :script) (hget f :line) (hget f :column) (point))))))

(defun numen-handle-resuming ()
  (with-repl-buffer
   (numen-exit-debugger)
   (numen-switch-to-repl)))

(defun numen-handle-in-place-output (text preserve-p)
  (with-repl-buffer
   (numen-in-place-output text 'numen-console-face preserve-p)))

(defun numen-handle-emacs-eval-request (elisp-text callback-id)
  (with-repl-buffer
   (let* ((errmsg nil)
          (result (condition-case e
                      (eval (read elisp-text))
                    (error (setq errmsg (format "Emacs error: %s" e))))))
     (unless (stringp result)
       (setq result (format "%s" result)))
     (numen-send-request (list :client_callback callback-id :errmsg errmsg :output result)))))

;;;; commands

(defun numen-switch-to-repl (&optional pos)
  (interactive)
  (with-repl-buffer
   (numen-display-buffer-in-preferred-window)
   (goto-char (cond ((and pos (>= pos (point-min)) (<= pos (point-max))) pos)
                    (t (point-max))))))

(defun numen-load-file ()
  (interactive)
  (acond ((numen-buffer-file) (numen-request-load-file it))
         (t (message "%s is not a file" (buffer-name)))))

(defun numen-exit ()
  (interactive)
  (with-repl-buffer
   (numen-stop)
   (kill-buffer)))

(defun numen-debugger-backtrace ()
  (interactive)
  (with-repl-buffer-when-debugging
   (numen-switch-to-repl)
   (numen-insert-backtrace)))

;;;; debugger session

(defun numen-push-call-stack (stack)
  (loop for frame in stack do (hset frame :id (incf numen-counter)))
  (push stack numen-call-stacks)
  (push (hget (car stack) :id) numen-selected-frame-ids)
  (numen-update-debugger-ui)
  (numen-walk-minor-buffers 'numen-attach-debugger)) ; tempdg: duplication with prev call

(defun numen-pop-call-stack ()
  (when numen-call-stacks
    (pop numen-call-stacks)
    (pop numen-selected-frame-ids)
    (numen-update-debugger-ui))
  (numen-walk-minor-buffers 'numen-detach-debugger))

(defun numen-find-frame-index (id stack)
  (loop for frame in stack for n from 0
        when (equal (hget frame :id) id) do (return n)))

(defun numen-selected-frame-index ()
  (numen-find-frame-index (car numen-selected-frame-ids) (car numen-call-stacks)))

(defun numen-update-debugger-ui ()
  (numen-update-prompt)
  (numen-update-backtrace (car numen-selected-frame-ids))
  (numen-walk-minor-buffers 'numen-update-fringe-overlays))

(defun numen-maybe-select-frame (id)
  (loop for f in (car numen-call-stacks) when (equal id (hget f :id)) do
        (setf (car numen-selected-frame-ids) id)
        (numen-update-debugger-ui)
        (return)))

(defun numen-end-debugging-session ()
  (with-repl-buffer
   (when numen-pre-debugging-state
     (let ((config (pget numen-pre-debugging-state :window-config)))
       (when (window-configuration-p config)
         (set-window-configuration config)))
     (setq numen-pre-debugging-state nil))))

(defun numen-stop ()
  (let ((inhibit-read-only t))
    (when (in-debugger-p)
      (numen-exit-debugger)
      (numen-end-debugging-session))
    (numen-kill-all-secondary-buffers)
    (numen-kill-evalproc)))

(defun numen-exit-debugger ()
  (with-repl-buffer
   (numen-pop-call-stack)
   (numen-update-mode-line nil)))

(defun numen-kill-evalproc ()
  (when numen-evalproc
    (set-process-sentinel numen-evalproc nil)
    (kill-buffer (process-buffer numen-evalproc))
    (delete-process numen-evalproc)
    (setq numen-evalproc nil)))

(defun numen-kill-all-secondary-buffers ()
  "Kill all the Numen secondary buffers (such as inspectors and
temporary scripts) that are bound to the current buffer, which is
assumed to be a Numen REPL that is shutting down. Also kill any
such buffers whose REPL buffer no longer exists."
  (loop for buffer in (buffer-list) when (numen-secondary-p buffer) do
        (let ((repl (buffer-local-value 'numen-repl-buffer buffer)))
          (assert repl)
          (when (or (eq repl (current-buffer)) (not (buffer-live-p repl)))
            (kill-buffer buffer)))))

(defun numen-update-mode-line (debugging-p)
  (assert (string= major-mode "numen-mode"))
  (let ((name (if debugging-p "Numen *Debugging*" "Numen")))
    (setq mode-name name)
    (assert (assq 'numen-minor-mode minor-mode-alist))
    (setf (second (assq 'numen-minor-mode minor-mode-alist)) (concat " " name))))

;;;; minor mode

(defun numen-enter-minor-mode ()
  (unless numen-buffer-id ; do this once
    (make-local-variable 'numen-made-readonly)
    (set (make-local-variable 'numen-buffer-id) (incf numen-counter))
    (let ((repl-buffer (with-repl-buffer (current-buffer))))
      (set (make-local-variable 'numen-repl-buffer) repl-buffer)
      (let ((repl-window (with-repl-buffer numen-preferred-window))
            (w (next-window)))
        (set (make-local-variable 'numen-preferred-window)
             (if (eq w repl-window) (next-window w) w))))
    (add-hook 'after-revert-hook 'numen-after-revert)
    (numen-attach-debugger)))

(defun numen-exit-minor-mode ()
  (when numen-buffer-id ; do this once
    (numen-detach-debugger t)
    (remove-hook 'after-revert-hook 'numen-after-revert)
    (kill-local-variable 'numen-made-readonly)
    (kill-local-variable 'numen-buffer-id)
    (kill-local-variable 'numen-repl-buffer)
    (kill-local-variable 'numen-preferred-window)))

(defun numen-after-revert ()
  (when (numen-find-repl-buffer) ;; make sure Numen is running
    (numen-update-fringe-overlays)))

(defun numen-attach-debugger ()
  (numen-update-fringe-overlays)
  (when (numen-script-is-being-debugged-p (numen-buffer-script))
    (unless buffer-read-only
      (setq numen-made-readonly t)
      (setq buffer-read-only t))))

(defun numen-detach-debugger (&optional exiting-minor-mode-p)
  (when numen-made-readonly
    (setq buffer-read-only nil)
    (setq numen-made-readonly nil))
  (cond (exiting-minor-mode-p (remove-overlays nil nil 'numen-fringe t))
        (t (numen-update-fringe-overlays))))

(defun numen-walk-minor-buffers (proc)
  (loop for buffer in (buffer-list)
        when (buffer-local-value 'numen-minor-mode buffer)
        do (with-current-buffer buffer
             (funcall proc))))

(defun numen-script-is-being-debugged-p (script)
  (loop for stack in (with-repl-buffer numen-call-stacks) do
        (loop for frame in stack when (string= script (hget frame :script))
              do (return t))))

(defun numen-update-fringe-overlays ()
  (let ((script (numen-buffer-script))
        (new (make-hash-table)))
    (remove-overlays nil nil 'numen-fringe t)
    (loop for bp in numen-breakpoints when (string= script (hget bp :script)) do
          (numen-put-fringe-overlay (hget bp :line) 'numen-breakpoint-face))
    (loop for frame in (car (with-repl-buffer numen-call-stacks))
          for face = 'numen-frame-arrow-face-top then 'numen-frame-arrow-face
          when (string= script (hget frame :script)) do
          ;; if there's already a breakpoint, just color it
          (or (numen-change-fringe-overlay-face (hget frame :line) face)
              (numen-put-fringe-overlay (hget frame :line) face)))))

(defun numen-change-fringe-overlay-face (line face)
  (wlet (ov (numen-get-fringe-overlay line))
    (let ((str (overlay-get ov 'before-string)))
      (put-text-property 0 (length str) 'face face str)
      (when window-system
        (wlet (display (get-text-property 0 'display str))
          (assert (and (eq (car display) 'left-fringe) (= 3 (length display))))
          (setf (third display) face))))))

(defun numen-get-fringe-overlay (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (let ((pos (line-beginning-position)))
      (loop for ov in (overlays-in pos pos)
            when (overlay-get ov 'numen-fringe) do (return ov)))))

(defun numen-put-fringe-overlay (line face)
  (assert (memq face '(numen-breakpoint-face numen-frame-arrow-face-top numen-frame-arrow-face)))
  (let ((str (numen-str (if (eq face 'numen-breakpoint-face) "B" "=>") face)))
    (overlay-put (numen-make-fringe-overlay line) 'before-string str)
    (when window-system
      (let ((bitmap (if (eq face 'numen-breakpoint-face) 'breakpoint 'right-triangle)))
        (put-text-property 0 (length str) 'display (list 'left-fringe bitmap face) str)))))

(defun numen-make-fringe-overlay (line)
  (save-excursion
    (goto-char (point-min))
    (forward-line line)
    (let* ((pos (line-beginning-position))
           (ov (make-overlay pos pos)))
      (overlay-put ov 'numen-fringe t)
      ov)))

;;;; backtrace

(defun numen-digits (n)
  "Number of digits of N in base 10."
  (if (= n 0)
      1
    (1+ (floor (log (abs n) 10)))))

(defun numen-print-backtrace (frames sel-id)
  (let ((maxd (numen-digits (1- (length frames)))))
    (loop for loc in (numen-padded-script-locations frames t)
          for frame in frames for n from 0 for pad = (- maxd (numen-digits n)) do
          (let* ((props (list 'frame-id (hget frame :id) 'script (hget frame :script)
                              'line (hget frame :line) 'column (hget frame :column)
                              'backtrace frames 'read-only t 'front-sticky '(read-only)))
                 (selp (equal (hget frame :id) sel-id))
                 (face (if selp 'numen-backtrace-bold-face 'numen-backtrace-face))
                 (str (format "%s%s %s" (make-string pad ?\ ) n loc)))
            (numen-insert str face 'script-location props)
            (numen-insert "\n" face 'script-location-no-highlight props)))))

(defun numen-insert-backtrace ()
  (numen-update-backtrace nil)         ; clear previous selected frame
  (with-output-before-prompt
   (when (and (> (point) (point-min)) (get-text-property (1- (point)) 'backtrace))
     (numen-insert "\n"))
   (numen-print-backtrace (car numen-call-stacks) (car numen-selected-frame-ids))))

(defun numen-update-backtrace (sel-id)
  (save-excursion
    (goto-char (point-max))
    (wbind (start end) (numen-find-previous-property-bounds 'backtrace)
      (goto-char start)
      (let ((inhibit-read-only t)
            (frames (get-text-property start 'backtrace)))
        (delete-region start end)
        (numen-print-backtrace frames sel-id)))))

;;;; breakpoints

(defun numen-bind-breakpoints (server-breakpoints)
  (setq numen-breakpoints (numen-sort-breakpoints (numen-listify server-breakpoints)))
  (numen-walk-minor-buffers 'numen-update-fringe-overlays))

(defun numen-toggle-breakpoint ()
  (interactive)
  (let ((script (if (in-repl-p) (get-text-property (point) 'script) (numen-buffer-script)))
        (line (if (in-repl-p) (get-text-property (point) 'line) (1- (line-number-at-pos)))))
    (cond ((not (and script line)) (message "Not a source location"))
          (t (when (in-repl-p) (numen-jump-to-script script line nil (point)))
             (let ((state (if (numen-find-breakpoint script line) :off :on)))
               (numen-request-breakpoint-state state script line (when (in-repl-p) (point))))))))

(defun numen-list-breakpoints ()
  (interactive)
  (with-repl-buffer
   (numen-switch-to-repl)
   (acond ((null numen-breakpoints) (message "No breakpoints"))
          (t (numen-insert-breaklist)))))

(defun numen-print-breaklist ()
  (loop for str in (numen-padded-script-locations numen-breakpoints)
        for bp in numen-breakpoints do
        (let ((props (list 'script (hget bp :script) 'line (hget bp :line)
                           'breaklist t 'read-only t 'front-sticky '(read-only))))
          (numen-insert str 'numen-breaklist-face 'script-location props)
          (numen-insert "\n" 'numen-breaklist-face 'script-location-no-highlight props))))

(defun numen-insert-breaklist ()
  (with-output-before-prompt
   (when (and (> (point) (point-min)) (get-text-property (1- (point)) 'breaklist))
     (numen-insert "\n"))
   (numen-print-breaklist)))

(defun numen-breakpoints-as-hashtable ()
  (when numen-breakpoints
    (let ((table (make-hash-table :test 'equal)))
      (loop for bp in numen-breakpoints do
            (push (hget bp :line) (gethash (hget bp :script) table)))
      table)))

(defun numen-find-breakpoint (script line)
  (loop for bp in numen-breakpoints
        when (and (equal (hget bp :script) script) (equal (hget bp :line) line))
        do (return bp)))

(defun numen-sort-breakpoints (breakpoints)
  (cl-flet ((pred (a b) (let ((ascript (numen-pretty-script-name (hget a :script)))
                              (bscript (numen-pretty-script-name (hget b :script))))
                          (cond ((string< ascript bscript) t)
                                ((string< bscript ascript) nil)
                                (t (< (hget a :line) (hget b :line)))))))
    (sort breakpoints (lambda (a b) (pred a b)))))

(defun numen-toggle-break-on-exception ()
  (interactive)
  (with-repl-buffer
   (numen-send-request (list :toggle_break_on_exception t))))

(defun numen-clear-all-breakpoints ()
  (interactive)
  (with-repl-buffer
   (numen-send-request (list :clear_all_breakpoints t))
   (numen-bind-breakpoints nil)))

;;;; source navigation

(defun numen-script-location-p ()
  "Return t if text properties at point indicate a script
location that could be jumped to."
  (and (get-text-property (point) 'script) (get-text-property (point) 'line)))

(define-button-type 'script-location
  'action 'numen-script-location-clicked 'follow-link t 'help-echo nil 'mouse-face 'match)

(define-button-type 'script-location-no-highlight
  :supertype 'script-location 'mouse-face nil)

(defun numen-script-location-clicked (&optional button)
  (with-repl-buffer
   (let ((props (text-properties-at (point)))
         (pos (point)))
     (wlet (id (pget props 'frame-id))
       (numen-maybe-select-frame id))
     (numen-jump-to-script (pget props 'script) (pget props 'line) (pget props 'column) pos))))

(defun numen-jump-to-script (script line &optional column goto)
  (let ((buffer (numen-obtain-script-buffer script)))
    (cond ((null buffer)
           (message "Requesting source for %s..." script)
           (numen-request-source script line column goto))
          (t (set-buffer buffer)
             (unless numen-minor-mode
               (numen-minor-mode +1))
             (numen-display-buffer-in-preferred-window)
             (goto-char (point-min))
             (forward-line line)
             (cond (column (forward-char column))
                   (t (back-to-indentation)))
             (numen-flash-entire-line)
             (when goto
               (numen-switch-to-repl goto))))))

(defun numen-obtain-script-buffer (script)
  (or (find-buffer-visiting script)
      (numen-find-secondary-source-buffer script)
      (when (file-exists-p script)
        (find-file-noselect script))))

(defvar numen-secondary-source-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'numen-quit-secondary-buffer)
    (define-key map "z" 'numen-kill-secondary-buffer)
    map))

(define-minor-mode numen-secondary-source-minor-mode
  "\\{numen-secondary-source-minor-mode-map}"
  :lighter " Snippet")

(defun numen-find-secondary-source-buffer (script)
  (let ((repl (with-repl-buffer (current-buffer))))
    (loop for buffer in (buffer-list) when (numen-secondary-p buffer) do
          (with-current-buffer buffer
            (when (and (eq numen-repl-buffer repl) (string= numen-script script))
              (return buffer))))))

(defun numen-make-secondary-source-buffer (script)
  (with-current-buffer (generate-new-buffer (format "*%s*" script))
    (js-mode)
    (numen-minor-mode)
    (numen-secondary-source-minor-mode)
    (setq buffer-read-only t)
    (set (make-local-variable 'numen-script) script)
    (set (make-local-variable 'numen-secondary-p) t)
    (current-buffer)))

(defun numen-update-secondary-source-buffer (script src)
  (with-current-buffer (or (numen-find-secondary-source-buffer script)
                           (numen-make-secondary-source-buffer script))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert src)
      (numen-fresh-line)
      (numen-update-fringe-overlays)
      (current-buffer))))

(defun numen-buffer-script ()
  (or (numen-buffer-file) numen-script))

(defun numen-buffer-file ()
  ;; buffer-file-truename seems not to do this
  (wlet (file (buffer-file-name))
    (file-truename file)))

(defun numen-quit-secondary-buffer ()
  "Bury the current buffer and switch back to REPL."
  (interactive)
  (bury-buffer)
  (numen-switch-to-repl (with-repl-buffer (point))))

(defun numen-kill-secondary-buffer ()
  "Kill the current buffer and switch back to REPL."
  (interactive)
  (kill-buffer)
  (numen-switch-to-repl (with-repl-buffer (point))))

(defun numen-secondary-p (&optional buffer)
  (buffer-local-value 'numen-secondary-p (or buffer (current-buffer))))

(defun* numen-padded-script-locations (locs &optional fns-p)
  (let ((strs (when fns-p
                (numen-pad-to-width
                 (loop for loc in locs collect (numen-pretty-fname (hget loc :fn)))))))
    (setq strs (numen-pad-to-width
                (loop for loc in locs for n from 0
                      for fn = (if fns-p (concat (elt strs n) " ") "") collect
                      (format "%s%s" fn (numen-pretty-script-loc (hget loc :script) (hget loc :line))))))
    (loop for loc in locs for str in strs collect
          (acond ((hget loc :text) (concat str " " it))
                 (t str)))))

(defun numen-pretty-script-loc (script line)
  (let ((str (numen-pretty-script-name script)))
    (cond ((null line) str)
          (t (format "%s:%s" str (1+ line))))))

(defun numen-pretty-fname (name)
  (when (equal name "")
    (setq name "<lambda>"))
  (let ((max 20))
    (when (> (length name) max)
      (setq name (concat (substring name 0 (1- max )) "~"))))
  name)

(defun numen-pretty-script-name (script)
  (cond ((null script) "<no script>")
        ((string-match "^/" script)
         (file-relative-name script (file-name-directory script)))
        (t script)))

;;;; repl ui

(defun numen-marker (key)
  (assert numen-markers)
  (or (gethash key numen-markers)
      (let ((marker (make-marker)))
        (set-marker marker (point-min)) ; so we can compare it to point etc. without crashing
        (setf (gethash key numen-markers) marker))))

(defun numen-scroll-to-bottom ()
  (goto-char (point-max))
  (wlet (window (get-buffer-window))
    (with-selected-window window
      (recenter -1))))

(defun numen-insert (text &optional face button properties)
  "Insert TEXT at point. Assign FACE and text PROPERTIES if specified.
Make text a button of type BUTTON if specified."
  (let ((start (point)))
    (insert-before-markers text)
    (when face
      (setf properties (append (list 'face face) properties)))
    (when properties
      (add-text-properties start (point) properties))
    (when button
      (make-text-button start (point) 'type button))))

(defmacro with-output-before-prompt (&rest body)
  "Bind `inhibit-read-only' to T, move point to before prompt,
evaluate BODY, then scroll to bottom of REPL."
  `(with-repl-buffer
    (let ((inhibit-read-only t))
      (goto-char (numen-prompt-start))
      ,@body
      (numen-scroll-to-bottom))))

(defun numen-output (str &optional face)
  (with-output-before-prompt
   (numen-insert str face)))

(defun numen-in-place-output (text &optional face preserve-p)
  "Delete any characters immediately before the prompt that were
written by a previous call to this function, then insert TEXT
before the prompt. This is useful when displaying text to
indicate the progress of a long-running operation.

When PRESERVE-P is non-nil, do not delete any previous output."
  (let ((inhibit-read-only t))
    (cond (preserve-p
           (let ((pos (numen-prompt-start)))
             (when (> pos (point-min))
               ;; prevent existing text from being deleted in a subsequent call
               (put-text-property (1- pos) pos 'in-place nil))))
          (t (save-excursion
               (goto-char (numen-prompt-start))
               (when (> (point) (point-min))
                 (backward-char))
               (when (get-text-property (point) 'in-place)
                 (dbind (start end) (numen-property-bounds 'in-place)
                   (delete-region start end))))))
    (let ((start (numen-prompt-start)))
      (goto-char start)
      (numen-insert text face)
      (put-text-property start (point) 'in-place t)
      (numen-scroll-to-bottom))))

(defun numen-fresh-line ()
  (unless (eq (char-before) ?\n)
    (numen-insert "\n")))

(defun numen-prompt-start ()
  (marker-position (numen-marker :prompt-start)))

(defun numen-prompt-end ()
  (let ((start (numen-prompt-start)))
    (next-property-change start nil (line-end-position start))))

(defun numen-insert-prompt ()
  (let ((start (point)))
    (set-marker (numen-marker :prompt-start) (point))
    (cond ((null numen-selected-frame-ids) (insert "> "))
          (t (loop for id in numen-selected-frame-ids for stack in numen-call-stacks
                   do (insert (format "%s> " (numen-find-frame-index id stack))))))
    (let ((properties '(face font-lock-keyword-face read-only t intangible t
                             prompt t rear-nonsticky (face read-only intangible prompt))))
      (add-text-properties start (point) properties))))

(defun numen-update-prompt ()
  (goto-char (numen-prompt-start))
  (let ((inhibit-read-only t))
    (delete-region (point) (numen-prompt-end))
    (numen-insert-prompt)))

(defun numen-current-input ()
  "Text from just after the prompt to `point-max'."
  (let ((input (numen-trim (buffer-substring-no-properties (numen-prompt-end) (point-max)))))
    (when (> (length input) 0)
      input)))

(defun numen-send-input (&optional arg)
  "Advance the prompt, add the current input to the input ring,
and evaluate it. With a prefix argument, break into the debugger
before evaluating."
  (interactive "P")
  (goto-char (point-max))
  (let ((input (numen-current-input)))
    (numen-fresh-line)
    (add-text-properties (numen-prompt-end) (point) '(old-input t))
    (numen-insert-prompt)
    (numen-scroll-to-bottom)
    (when input
      (let ((src (funcall numen-input-compiler input)))
        (cond ((and src (> (length src) 0))
               (numen-request-evaluation src (when arg t))
               (numen-add-to-input-ring (numen-trim input))
               (setq numen-input-ring-index -1))
              (t (numen-output "Nothing to evaluate\n" 'numen-info-face)))))))

(defun numen-return (&optional arg)
  "Call `numen-send-input' to evaluate the current input string,
unless point is at an old input, in which case call
`numen-grab-old-input' to insert the old input at the current
prompt."
  (interactive "P")
  (cond ((get-text-property (point) 'old-input) (numen-grab-old-input))
        (t (numen-send-input arg))))

(defun numen-grab-old-input ()
  "When point is in an old input expression, copy it to the current prompt."
  (assert (get-text-property (point) 'old-input))
  (dbind (start end) (numen-property-bounds 'old-input)
    (let ((input (numen-trim (buffer-substring-no-properties start end))))
      (when (> (length input) 0)
        (goto-char (point-max))
        (unless (eq (char-before) ?\ )
          (numen-insert " "))
        (numen-insert input)))))

(defun numen-back-to-prompt-if-on-same-line ()
  (when (get-text-property (point) 'prompt)
    (goto-char (next-single-property-change (point) 'prompt nil (point-max))))
  (cond ((get-text-property (1- (point)) 'prompt) (point))
        (t (wlet (pos (numen-find-previous-prompt))
             (when (= (line-number-at-pos (point)) (line-number-at-pos pos))
               (goto-char pos))))))

(defun numen-move-beginning-of-line (arg)
  (interactive "^p") ;; like the original
  (with-repl-buffer
   (unless (numen-back-to-prompt-if-on-same-line)
     (move-beginning-of-line arg))))

(defun numen-back-to-indentation ()
  (interactive "^") ;; like the original
  (with-repl-buffer
   (unless (numen-back-to-prompt-if-on-same-line)
     (back-to-indentation))))

(defun numen-kill-input ()
  (interactive)
  (with-repl-buffer
   (goto-char (numen-prompt-end))
   (kill-region (point) (point-max))))

(defun numen-goto-next-prompt ()
  "Move point to the next prompt, if there is one."
  (interactive)
  (wlet (pos (numen-find-next-prompt))
    (goto-char pos)))

(defun numen-find-next-prompt ()
  (wlet (pos (next-single-property-change (point) 'prompt nil (point-max)))
    (when (get-text-property pos 'prompt)
      (setq pos (or (next-single-property-change pos 'prompt) (point-max))))
    pos))

(defun numen-goto-previous-prompt ()
  "Move point to the previous prompt, if there is one."
  (interactive)
  (acond ((numen-find-previous-prompt) (goto-char it))
         (t (message "No previous prompt"))))

(defun numen-find-previous-prompt ()
  (when (> (point) (point-min))
    (wbind (start end) (numen-find-previous-property-bounds 'prompt)
      (cond ((< end (point)) end)
            ((> start (point-min))
             (save-excursion
               (goto-char (1- start))
               (second (numen-find-previous-property-bounds 'prompt))))))))

(defun numen-clear-output ()
  "Delete the output inserted after the previous prompt."
  (interactive)
  (let (start end)
    (save-excursion
      (numen-goto-previous-prompt)
      (ignore-errors (forward-sexp))
      (forward-line)
      (setq start (point))
      (numen-goto-next-prompt)
      (goto-char (previous-single-property-change (point) 'prompt))
      (setq end (point)))
    (when (< start end)
      (let ((inhibit-read-only t))
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (numen-output "cleared\n" 'numen-omitted-italic-face)))
      (when (get-text-property (point) 'prompt)
        (goto-char (next-single-property-change (point) 'prompt nil (point-max)))))))

(defun numen-clear-repl-buffer ()
  "If point is before the current prompt, clear everything
between the previous prompt and the current prompt. Otherwise
delete everything before the current prompt."
  (interactive)
  (with-repl-buffer
   (let ((start (point-min)))
     (wbind (prompt-start ignore) (numen-find-previous-property-bounds 'prompt)
       (when (< prompt-start (numen-prompt-start))
         (setq start prompt-start)))
     (let ((inhibit-read-only t))
       (delete-region start (numen-prompt-start))
       (goto-char (point-max))))))

;;;; printer

(defun numen-insert-value (id view)
  (let ((origin (point))
        (scope (or (pget view :restriction) (list id))))
    (pset view :start-pos origin)
    (pset view :extents (make-hash-table :test 'equal))
    (when (or (numen-inspector-buffer-p) (> (length scope) 1))
      (numen-print-header scope origin (pget view :extents)))
    (let* ((val (numen-find-val scope))
           (locals-p (hget val :locals)) ; stored on the value so if we reprint it,
           ;; it gets the local-variable formatting again. but we only
           ;; format it specially at the top level. if, for example,
           ;; you wrap it in a list, it will be printed generically.
           (draft (numen-draft-value val view locals-p)))
      (numen-print-draft nil draft scope origin view))
    (numen-fresh-line)
    (numen-put-scope origin (point) scope)
    (unless (pget view :undo)
      (numen-push-breadcrumb (numen-make-breadcrumb scope 0 view) view :undo))
    (add-text-properties origin (point) `(read-only t front-sticky (read-only)
                                                    keymap ,numen-inspector-map))))

(defun numen-print-header (restriction origin extents)
  (let ((start (point)))
    (loop for n from (1- (length restriction)) downto 0
          for scope = (nthcdr n restriction)
          for pval = (numen-find-val (cdr scope))
          for val = (numen-find-val scope) for pos = (point) do
          (numen-insert (numen-vkey-bit pval (car scope)) 'header-line)
          (cond ((> n 0) (setf (gethash (vkey val) extents) (list :anchor (- pos origin))))
                (t (let* ((suffix (when pval
                                    (format "(%s of %s)" (1+ (car scope)) (hget val :truelen))))
                          (avail (if (numen-inspector-buffer-p) (window-width) numen-print-wrap))
                          (spaces (max 1 (- avail (- (point) start) (length suffix)))))
                     (numen-insert (concat (make-string spaces ?\ ) suffix) 'header-line)
                     (numen-fresh-line))))
          (numen-put-scope pos (point) scope))))

(defun numen-draft-value (val view &optional locals-p)
  (acond ((not (hash-table-p val)) (numen-draft-atom "?!" 'numen-error-face))
         ((hget val :null) (numen-draft-atom it 'numen-null-face))
         ((hget val :bool) (numen-draft-atom it 'numen-boolean-face))
         ((hget val :num) (numen-draft-atom it 'numen-number-face))
         ((hget val :date) (numen-draft-atom it 'numen-date-face))
         ((hget val :errmsg?) (numen-draft-naked-string val view 'numen-error-face))
         ((hget val :loc?) (numen-draft-script-location val view))
         ((hget val :error?) (numen-draft-error val view))
         ((hget val :str) (numen-draft-quoted-string val view))
         ((hget val :fn) (numen-draft-function val))
         ((hget val :vals) (numen-draft-composite val view locals-p))
         (t (numen-str "?" 'numen-error-face))))

(defun numen-draft-atom (str face)
  (list :str (numen-str str face)))

(defun numen-maybe-ellipsis (val view dlen truelen)
  (when (< dlen truelen)
    (numen-set-display-len view val dlen)
    (list :ellipsis (numen-str (format "%s..." (- truelen dlen)) 'numen-omitted-italic-face))))

(defun numen-draft-naked-string (val view face &optional props)
  (hbind (str truelen) val
    (let ((dlen (numen-how-many-to-display view val)))
      (when (< dlen (length str))
        (setq str (substring str 0 dlen)))
      (setq str (apply #'numen-str str face props))
      (append (list :str str) (numen-maybe-ellipsis val view dlen truelen)))))

(defun numen-draft-quoted-string (val view)
  (let ((delim (numen-str "\"" 'numen-string-face)))
    (append (numen-draft-naked-string val view 'numen-string-face)
            (list :delim1 delim :delim2 delim))))

(defun numen-draft-function (val)
  (hbind (fn script line column) val
    (let* ((text (if script (first (numen-padded-script-locations (list val) t)) fn))
           (props (list 'script script 'line line 'column column)))
      (list :str (apply #'numen-str text 'numen-function-face 'mouse-face 'match props)
            :delim1 (apply #'numen-str "#<" 'numen-function-face props)
            :delim2 (apply #'numen-str ">" 'numen-function-face props)))))

(defun numen-draft-script-location (val view)
  (unless (hget val :str)
    ;; a multi-line stack trace wasn't printed, so do this one standalone
    (numen-render-script-locations (list val)))
  (hbind (printed script line column) val
    (let ((props `(mouse-face match script ,script line ,line column ,column)))
      (numen-draft-naked-string val view 'numen-info-face props))))

(defun numen-render-script-locations (locs)
  "Make a textual representation of each script location in LOCS
that is padded to display each field in a column. Store that text
as a \"printed\" entry with each location so they may be printed
individually by calling `numen-draft-value'."
  (loop for str in (numen-padded-script-locations locs t) for loc in locs do
        (hset loc :str str)
        (hset loc :truelen (length str))))

(defun numen-stub-err-msg (val)
  (hbind (vals) val
    (assert (and (> (length vals) 0) (hget (elt vals 0) :errmsg?)))
    (let* ((str (hget (elt vals 0) :str))
           (end (min 16 (length str))))
      (when (string-match "^[^:]+:" str)
        (setq end (1- (second (match-data)))))
      (numen-str (substring str 0 end) 'numen-error-face))))

(defun numen-draft-error (val view)
  (hbind (vals) val
    (numen-render-script-locations
     (loop for n from 1 below (numen-how-many-to-display view val) collect (elt vals n)))
    (numen-draft-composite val view nil "#<" ">")))

(defun numen-draft-stub (val delim1 delim2)
  (setq delim1 (numen-trim delim1) delim2 (numen-trim delim2))
  (hbind (vals truelen) val
    (let ((str (cond ((= truelen 0) "")
                     ((hget val :error?) (numen-stub-err-msg val))
                     (t (numen-str (format "%d..." truelen)
                                   (cond ((> (length vals) 0) 'numen-omitted-face)
                                         (t 'numen-omitted-italic-face)))))))
      (list :str str :delim1 delim1 :delim2 delim2))))

(defun numen-draft-composite (val view &optional locals-p delim1 delim2)
  (hbind (keys vals truelen) val
    (setq delim1 (or delim1 (if keys "{ " "(")))
    (setq delim2 (or delim2 (if keys " }" ")")))
    (cond ((or (= (length vals) 0) (numen-hiding-p view (vkey val)))
           (numen-draft-stub val delim1 delim2))
          (t (let* ((keyface (if locals-p 'numen-backtrace-face 'numen-object-key-face))
                    (ks (loop for k across keys collect (numen-str k keyface)))
                    (dlen (numen-how-many-to-display view val))
                    (vs (loop for n from 0 below dlen for v across vals
                              collect (numen-draft-value v view))))
               (append (list :ks ks :vs vs :delim1 delim1 :delim2 delim2)
                       (numen-maybe-ellipsis val view dlen truelen)))))))

(defun numen--throw-if-too-big (dr acc max)
  (cl-flet ((add% (len) (when (> (incf (car acc) len) max)
                          (throw 'too-big t))))
    (add% (length (pget dr :delim1)))
    (add% (length (pget dr :delim2)))
    (when (pget dr :ellipsis)
      (add% (1+ (length (pget dr :ellipsis))))) ;; add one space
    (acond ((pget dr :str) (add% (length it)))
           (t (loop for k in (pget dr :ks) do (add% (1+ (length k))))
              (add% (1- (length (pget dr :vs)))) ; spaces between
              (loop for v in (pget dr :vs) do (numen--throw-if-too-big v acc max))))))

(defun numen-one-liner-p (dr)
  (not (catch 'too-big (numen--throw-if-too-big dr (list 0) numen-print-wrap))))

(defun numen-print-draft (k v scope origin view)
  (assert (and (consp v) (or (pget v :str) (pget v :vs))))
  (let ((vkey (vkey (numen-find-val scope)))
        (anchor (- (point) origin))
        (delim1 (pget v :delim1))
        (delim2 (pget v :delim2))
        (ellipsis (pget v :ellipsis)))
    (when k (numen-insert (concat k " ")))
    (when delim1 (numen-insert delim1))
    (let ((content1 (- (point) origin))
          (content2 nil))
      (cond ((pget v :str)
             (numen-insert (pget v :str))
             (setq content2 (- (point) origin))
             (numen-insert (if ellipsis " " (or delim2 ""))))
            (t (let ((pos^ (- (point) (numen-trailing-spaces delim1)))
                     (ks (pget v :ks))
                     (len (length (pget v :vs)))
                     (sep (acond ((numen-one-liner-p v) " ")
                                 ((- (point) (line-beginning-position))
                                  (concat "\n" (make-string it ?\ ))))))
                 (loop for v+ in (pget v :vs) for n from 0 for scope+ = (cons n scope)
                       for pos = pos^ then (point) do
                       (when (> n 0) (numen-insert sep))
                       (numen-print-draft (elt ks n) v+ scope+ origin view)
                       (when (= n (1- len))
                         (setq content2 (- (point) origin))
                         (numen-insert (if ellipsis sep delim2)))
                       (numen-put-scope pos (point) scope+)))))
      (when ellipsis
        (numen-insert (concat ellipsis (or delim2 ""))))
      (setf (gethash vkey (pget view :extents))
            (list :anchor anchor :content1 content1
                  :content2 content2 :stop (- (point) origin))))))

(defun* numen-get-scope (&optional (pos (point)))
  (get-text-property pos 'scope))

(defun numen-put-scope (start end scope)
  ;; inner scopes have already been set
  (loop for (a b) on (numen-property-intervals start end 'scope) by #'cdr
        when (and b (not (get-text-property a 'scope))) do
        (put-text-property a b 'scope scope)))

(defun* numen-scope-anchor (&optional (scope (numen-get-scope)))
  "Position of the first meaningful character of SCOPE. When
SCOPE is hidden, return this value for the nearest ancestor
instead."
  (numen-scope-pos scope :anchor))

(defun* numen-scope-content-start (&optional (scope (numen-get-scope)))
  "Like `numen-scope-anchor', but skips the opening delimiter to
get to the first character of printed data for SCOPE. When SCOPE
is hidden, return this value for the nearest ancestor instead."
  (numen-scope-pos scope :content1))

(defun* numen-scope-content-end (&optional (scope (numen-get-scope)))
  "Like `numen-scope-stop', but excludes any ellipsis and closing
delimiter to obtain one past the last character of printed
content for SCOPE. When SCOPE is hidden, return this value for
the nearest ancestor instead."
  (numen-scope-pos scope :content2))

(defun* numen-scope-stop (&optional (scope (numen-get-scope)))
  "One past the position of the last meaningful character of
SCOPE, including any ellipsis and delimiter. When SCOPE is
hidden, return this value for the nearest ancestor instead."
  (numen-scope-pos scope :stop))

(defun numen-scope-pos (scope prop)
  (wlet (view (numen-view (id scope)))
    (cl-flet ((pos% (sc) (wlet (vkey (hget (numen-find-val sc) :vkey))
                           (pget (gethash vkey (pget view :extents)) prop))))
      (loop while scope for pos = (pos% scope) until pos do (setq scope (cdr scope))
            finally (return (+ pos (numen-value-start (id scope))))))))

(defun numen-value-start (id)
  (wlet (view (numen-view id))
    (let ((start (pget view :start-pos)))
      (when (and start (not (numen-value-start-p id start)))
        (setq start nil))
      (unless start
        (setq start (or (numen-search-value-backward id)
                        (numen-search-value-forward id)
                        (error "Can't find printed value $[%s]." id)))
        (pset view :start-pos start))
      start)))

(defun numen-search-value-backward (id)
  (cond ((numen-value-start-p id (point-min)) (point-min)) ; inspector buffer
        (t (let ((pos (point)))
             (loop when (numen-value-start-p id pos) do (return pos)
                   do (setq pos (previous-single-property-change pos 'scope))
                   (acond ((null pos) (return nil))
                          ((get-text-property pos 'scope)
                           (when (< (id it) id)
                             (return nil)))))))))

(defun numen-search-value-forward (id)
  (let ((pos (point)))
    (loop when (numen-value-start-p id pos) do (return pos)
          do (setq pos (next-single-property-change pos 'scope))
          (acond ((null pos) (return nil))
                 ((get-text-property pos 'scope)
                  (when (> (id it) id)
                    (return nil)))))))

(defun numen-value-start-p (id pos)
  "Return non-nil if POS is the first character position of the
printed value identified by ID."
  (and pos (>= pos (point-min)) (<= pos (point-max))
       (equal (get-text-property pos 'scope) (list id))
       (or (= pos (point-min))
           (not (equal id (id (get-text-property (1- pos) 'scope)))))))

(defun numen-search-value-end ()
  (assert (numen-get-scope))
  (loop for pos = (point) then (next-single-property-change pos 'scope nil (point-max))
        when (null (get-text-property pos 'scope)) do (return pos)))

(defun numen-delete-value (id)
  (wlet (start (numen-value-start id))
    (goto-char start)
    (delete-region start (numen-search-value-end))
    (pset (numen-view id) :extents nil)))

(defun numen-reinsert-value (id)
  (let ((inhibit-read-only t))
    (numen-delete-value id)
    (numen-insert-value id (numen-view id))))

;;;; inspector

(defun numen-assign-inspector-bindings (&optional map)
  (unless map
    (setq map (setq numen-inspector-map (make-sparse-keymap))))
  (suppress-keymap map)
  (define-key map (kbd "h") 'numen-inspector-highlight)
  (define-key map (kbd "f") 'numen-inspector-forward)
  (define-key map (kbd "b") 'numen-inspector-backward)
  (define-key map (kbd "u") 'numen-inspector-up)
  (define-key map (kbd "d") 'numen-inspector-down)
  (define-key map (kbd "l") 'numen-inspector-set-display-length)
  (define-key map (kbd "RET") 'numen-inspector-return)
  (define-key map (kbd "x") 'numen-inspector-elide)
  (define-key map (kbd "r") 'numen-inspector-restrict)
  (define-key map (kbd "w") 'numen-inspector-widen)
  (define-key map (kbd "/") 'numen-inspector-backtrack)
  (define-key map (kbd ">") 'numen-inspector-reset)
  (define-key map (kbd ".") 'numen-inspector-retrace)
  (define-key map (kbd "o") 'numen-inspector-other-window)
  (define-key map [mouse-1] 'numen-inspector-mouse-left-click)
  (define-key map (kbd "q") 'numen-inspector-quit)
  map)

(defvar numen-inspector-map (numen-assign-inspector-bindings))

(defmacro do-leaving-breadcrumbs (&rest body)
  `(progn
     (numen-leave-breadcrumb)
     (prog1 (progn ,@body)
       (numen-leave-breadcrumb))))

(defun numen-inspector-highlight ()
  "When point falls within a printed value, highlight the
innermost scope containing point and print it as an expression to
the minibuffer."
  (interactive)
  (wlet (val (numen-find-val-or-report-deletion (numen-get-scope)))
    (do-leaving-breadcrumbs
     (numen-flash-region (numen-scope-anchor) (numen-scope-stop))
     (message "%s" (vkey val)))))

(defun numen-inspector-forward (&optional times)
  "When point falls within a printed value, move point to the
start of the next sibling scope. If there is no next sibling,
move to the next sibling of the nearest parent scope.  If no
parent scope has a next sibling, move to end of printed form."
  (interactive "p")
  (numen-inspector-navigate #'numen-scope-forward times t))

(defun numen-inspector-backward (&optional times)
  "When point falls within a printed value, move point to the
start of the current scope. If point is already there, move to
the start of the previous sibling scope. If there is no previous
sibling, move to the start of the parent scope."
  (interactive "p")
  (numen-inspector-navigate
   (lambda (scope)
     (cond ((> (point) (numen-scope-anchor)) scope)
           (t (numen-scope-backward scope))))
   times))

(defun numen-inspector-up (&optional times)
  "When point is within a printed value, move point to the start
of the current scope. If point is already there, move to the
start of the parent scope."
  (interactive "p")
  (numen-inspector-navigate
   (lambda (scope)
     (cond ((> (point) (numen-scope-anchor)) scope)
           (t (numen-scope-up scope))))
   times))

(defun numen-inspector-down (&optional times)
  "If point is within a printed value, search forward for a scope
that has a child and move point to that child. If no child is
found before exiting the parent scope, move forward instead. (See
`numen-inspector-forward'.)"
  (interactive "p")
  (numen-inspector-navigate
   (lambda (scope)
     (let* ((scope+ (numen-scope-down scope))
            (pos (numen-scope-anchor scope+)))
       (cond ((and pos (> pos (point))) scope+) ; must be visible and move point forward
             (t (numen-scope-forward scope)))))
   times t))

(defun* numen-inspector-navigate (scopefn &optional (times 1) forward-p)
  (wlet (scope (numen-get-scope))
    (save-excursion
      (loop repeat times for scope+ = (funcall scopefn scope)
            until (or (null scope+) (equal scope scope+))
            do (setq scope scope+)))
    (when scope
      (wlet (val (numen-find-val-or-report-deletion scope))
        (do-leaving-breadcrumbs
         (cond ((<= (length scope) (length (numen-restriction (id scope))))
                (numen-place-restriction scope val (list scope nil)))
               (t (let ((pos (when (and forward-p (null (cdr scope))) ; only place forward is the end
                               (numen-scope-stop scope))))
                    (numen-goto-scope scope val pos)))))))))

(defun* numen-goto-scope (scope &optional val pos)
  (unless val (setq val (numen-find-val scope)))
  (goto-char (or pos (numen-scope-anchor scope)))
  (message "%s" (vkey val)))

(defun numen-goto-spot (scope where)
  "Like `numen-goto-scope', but exactly where to go depends on
WHERE. When SCOPE represents an array or object and WHERE is an
integer denoting a child scope, go to that child scope. When
SCOPE represents a string and WHERE is an integer, go that many
characters into the string. Alternatively, when WHERE is T, go to
the end of SCOPE."
  (wlet (val (numen-find-val scope))
    (wlet (n (when (and (hget val :vals) (integerp where))
               (min where (1- (length (hget val :vals))))))
      (when (>= n 0)
        (setq scope (cons n scope))))
    (let ((endp (eq where t)))
      (numen-goto-scope scope nil (when endp (1- (numen-scope-stop scope)))))
    (wlet (chars (when (and (hget val :str) (integerp where))
                   (min where (1- (length (hget val :str))))))
      (when (>= chars 0)
        (let ((pos (point)))
          (when (eq (char-after) ?\")
            (forward-char))
          (forward-char chars)
          (unless (equal scope (numen-get-scope)) ; if not still in string, bail
            (goto-char pos)))))))

(defun numen-spot (scope &optional n)
  "Return a list of arguments that can later be passed to `numen-goto-spot'.
SCOPE is assumed to be that of a composite object or string. If
point is at or before the scope anchor, return the start of the
scope. If point is at or after the scope stop, return the end of
the scope. If point is within the scope, the return value depends
on N: if N is an integer, return the Nth child scope (or
character position, if the value is a string); if N is t, return
the scope end; if N is nil, return the scope start."
  (cond ((<= (point) (numen-scope-anchor scope)) (list scope nil))
        ((>= (point) (1- (numen-scope-stop scope))) (list scope t))
        (t (list scope n))))

(defun numen-inspector-return (&optional depth)
  "If the value at point is an object or array, toggle the
hiddenness of that value, or set its display depth to DEPTH if a
prefix argument is supplied. If the value at point is a script
location, display the script and go to that line."
  (interactive "P")
  (wlet (scope (numen-get-scope))
    (wlet (val (numen-find-val-or-report-deletion scope))
      (let ((id (id scope))
            (jump nil)
            (change nil))
        (do-leaving-breadcrumbs
         (cond ((numen-script-location-p)
                (setq jump (text-properties-at (point))))
               (t (let ((spot (numen-spot scope t)))
                    (when (numen-fetch-initial-data-up-to-depth scope val spot (or depth 1))
                      (setq change :fetching))
                    (when (cond (depth (let ((unfolded-p (numen-unfold id val (1- depth))))
                                         (or (numen-fold id val depth) unfolded-p)))
                                ((numen-unfold id val 0) t)
                                ((numen-fold id val 0) t))
                      (setq change :depth)
                      (numen-reinsert-value (id scope))
                      (apply #'numen-goto-spot spot))))))
        (cond (jump (numen-jump-to-script (pget jump 'script) (pget jump 'line)
                                          (pget jump 'column) (point)))
              ((eq change :fetching) (message "Fetching..."))
              ((null change) (message "No change")))))))

(defun numen-unfold (id val depth)
  "Unhide any child scopes whose depth is less than or equal to
DEPTH. Return t if anything changed."
  (lexical-let ((changed-p nil))
    (numen-walk-val
     (lambda (v d ignore)
       (wlet (view (numen-view id))
         (when (numen-hiding-p view (vkey v))
           (numen-unhide view (vkey v))
           (setq changed-p t))))
     val depth)
    changed-p))

(defun numen-fold (id val depth)
  "Hide any child scopes at depth DEPTH. Return t if anything changed."
  (lexical-let ((changed-p nil))
    (numen-walk-val
     (lambda (v d ignore)
       (wlet (view (numen-view id))
         (when (and (= d depth)
                    (> (length (hget v :vals)) 0)
                    (not (numen-hiding-p view (vkey v))))
           (numen-hide view (vkey v))
           (setq changed-p t))))
     val depth)
    changed-p))

(defun numen-fetch-initial-data-up-to-depth (scope val spot &optional depth)
  "For any value up to DEPTH for which no data has yet been
loaded, fetch some data."
  (unless depth (setq depth numen-default-value-depth))
  (lexical-let ((needs '()))
    (numen-walk-val
     (lambda (v d scope+)
       (when (and (= (numen-have-len v) 0) (> (or (hget v :truelen) 0) 0))
         (push (list :scope scope+ :vkey (vkey v) :from 0 :fold-level (- depth d)) needs)))
     val depth scope)
    (when needs
      (numen-request-details (id scope) needs spot)
      t)))

(defun numen-inspector-set-display-length (&optional how-many)
  "If the value at point is an object or array, display it with
up to HOW-MANY elements. (The default is 10.) If it is a string,
display it with up to HOW-MANY characters. If the value at point
is atomic, use the parent scope instead.

If HOW-MANY is zero, this hides the value rather than altering
its display length. If the value is hidden and HOW-MANY is
greater than zero, unhide it first."
  (interactive "P")
  (unless how-many (setq how-many 10))
  (wlet (scope (numen-get-scope))
    (wlet (val (numen-find-val-or-report-deletion scope))
      (do-leaving-breadcrumbs
       ;; if current scope is an atom, use parent instead.
       (unless (or (hget val :vals) (hget val :str) (null (cdr scope)))
         (setq scope (cdr scope))
         (setq val (numen-find-val scope)))
       (let ((spot (numen-spot scope (1- (min how-many (hget val :truelen)))))
             (change nil))
         (when val
           (cond ((= how-many 0) (setq change (numen-fold (id scope) val 0)))
                 (t (let ((unhid-p (numen-unfold (id scope) val 0)))
                      (setq change (or (numen-display-up-to scope val how-many) unhid-p))))))
         (cond ((null change) (message "No change"))
               ((eq change :fetching) (message "Fetching..."))
               (t (numen-reinsert-value (id scope))
                  (apply #'numen-goto-spot spot))))))))

(defun numen-inspector-elide ()
  "If the point is within a child scope, set the display length
of the parent scope to end at this child. If point is within a
string, set the display length of the string instead."
  (interactive)
  (wlet (scope (numen-get-scope))
    (wlet (val (numen-find-val-or-report-deletion scope))
      (do-leaving-breadcrumbs
       (let* ((str-pos (numen-string-elide-pos scope val))
              (pscope (if str-pos scope (cdr scope)))
              (len (1+ (or str-pos (car scope))))
              (pval (if str-pos val (numen-find-val (cdr scope)))))
         (cond ((and pval (numen-display-up-to pscope pval len))
                (numen-reinsert-value (id scope))
                (numen-goto-spot pscope (1- len)))
               (t (message "No change"))))))))

(defun numen-string-elide-pos (scope val)
  (when (and (hget val :str) (< (point) (numen-scope-content-end scope)))
    (let* ((content1 (numen-scope-content-start scope))
           (pos (- (point) content1)))
      ;; naked strings have to claim the first character of content
      ;; for the parent. quoted strings have a delimiter to play that
      ;; role, so it's ok to elide at the first char (pos 0).
      (when (cond ((= content1 (numen-scope-anchor scope)) (> pos 0))
                  (t (>= pos 0)))
        pos))))

(defun numen-display-up-to (scope val how-many)
  (when (and (> how-many 0) (> (or (hget val :truelen) 0) 0))
    (let* ((have (numen-have-len val))
           (unfetched (- (hget val :truelen) have))
           (fetch (min (- how-many have) unfetched)))
      (when (> fetch 0)
        (let* ((need (list :scope scope :vkey (vkey val) :from have :batch fetch
                           :fold-level numen-default-value-depth))
               (spot (numen-spot scope (pget need :from))))
          (numen-request-details (id scope) (list need) spot)))
      (let ((changed-p (numen-set-display-len (numen-view (id scope)) val how-many)))
        (cond ((> fetch 0) :fetching)
              (changed-p :redisplay))))))

(defun numen-inspector-restrict ()
  "When point is within a printed value, restrict the printed
value to the innermost scope. If this causes a parent scope to be
hidden, print the parent scope in abbreviated form before the
value. `numen-inspector-return' can later be invoked on the
abbreviation to redisplay contents. Alternatively,
`numen-inspector-widen' can be invoked to relax the restricted scope."
  (interactive)
  (wlet (scope (numen-get-scope))
    (wlet (val (numen-find-val-or-report-deletion scope))
      (do-leaving-breadcrumbs
       (numen-place-restriction scope val (list scope nil))))))

(defun numen-place-restriction (scope val spot)
  (wlet (view (numen-view (id scope)))
    (pset view :restriction (when (or (cdr scope) (numen-inspector-buffer-p)) scope))
    (numen-reinsert-value (id scope))
    (apply #'numen-goto-spot spot)
    (numen-fetch-initial-data-up-to-depth scope (or val (numen-find-val scope)) spot)))

(defun numen-inspector-widen ()
  "When point is within a printed value that has been restricted
to a child scope, relax the restriction by one level. This is
equivalent to `numen-inspector-up', except that the point stays at
the current scope."
  (interactive)
  (wlet (val (numen-find-val-or-report-deletion (numen-get-scope)))
    (do-leaving-breadcrumbs
     (let* ((scope (numen-get-scope))
            (wider (cdr (numen-restriction (id scope)))))
       (cond ((null wider) (message "%s" (vkey val)))
             (t (numen-place-restriction wider nil (list scope nil))))))))

(defun numen-inspector-backtrack ()
  "Attempt to move point to where the most recent inspector
command was issued, as well as restore the view to what it looked
like then. If backtracking all the way to the original inspector
state, stay there. The original state may be reset by invoking
`numen-inspector-reset'."
  (interactive)
  (numen-inspector-undo-or-redo :undo))

(defun numen-inspector-reset ()
  "Clear any inspector state that would otherwise be reachable
from `numen-inspector-backtrack' and `numen-inspector-retrace'.
Make the current state the new original state (see
`numen-inspector-backtrack')."
  (interactive)
  (wlet (view (numen-view (id (numen-get-scope))))
    (do-leaving-breadcrumbs
     (pset view :undo nil)
     (pset view :redo nil))))

(defun numen-inspector-retrace ()
  "Restore the inspector state that `numen-inspector-backtrack'
just backed away from. If the last inspector command was not
`numen-inspector-backtrack', don't do anything."
  (interactive)
  (numen-inspector-undo-or-redo :redo))

(defun numen-inspector-undo-or-redo (prop)
  (wlet (scope (numen-get-scope))
    (wlet (view (numen-view (id scope)))
      (let ((prop+ (cond ((eq prop :undo) :redo)
                         ((eq prop :redo) :undo))))
        (numen-push-breadcrumb (numen-current-state-as-breadcrumb) view prop+)
        (when (equal (car (pget view prop)) (numen-current-state-as-breadcrumb))
          (when (or (eq prop :redo) (cdr (pget view :undo)))
            (numen-pop-breadcrumb view prop)))
        (wlet (crumb (car (pget view prop)))
          (numen-push-breadcrumb crumb view prop+)
          (when (numen-set-view-state-to-breadcrumb view crumb)
            (numen-reinsert-value (id scope)))
          (wlet (val (numen-find-val-or-report-deletion (pget crumb :scope)))
            (let ((pos (+ (numen-value-start (id scope)) (pget crumb :pos))))
              (numen-goto-scope (pget crumb :scope) val pos))))))))

(defun numen-leave-breadcrumb ()
  "Leaves current state on the backtracking stack if it isn't there already."
  (wlet (view (numen-view (id (numen-get-scope))))
    (when (numen-push-breadcrumb (numen-current-state-as-breadcrumb) view :undo)
      (pset view :redo nil))))

(defun numen-current-state-as-breadcrumb ()
  (wlet (scope (numen-get-scope))
    (let ((rel-pos (- (point) (numen-value-start (id scope)))))
      (numen-make-breadcrumb scope rel-pos (numen-view (id scope))))))

(defun numen-inspector-other-window ()
  "When point is within a printed value, copy the value to a
temporary buffer in another window. The value may be inspected
there using the same commands as in the REPL."
  (interactive)
  (let* ((scope (numen-get-scope))
         (id (id scope)))
    (wlet (val (numen-find-val-or-report-deletion scope))
      (let ((view (numen-make-view (numen-view id))))
        (pset view :restriction scope)
        (switch-to-buffer-other-window (numen-spawn-inspector scope))
        (setf (gethash id numen-views) view)
        (numen-plist-to-hash (list id view))
        (let ((inhibit-read-only t))
          (numen-insert-value id view)
          (goto-char (numen-scope-anchor scope))
          (pset (numen-view id) :undo nil)
          (numen-leave-breadcrumb) ; backstop at initial inspector view
          (message "%s" (vkey val)))))))

(defun numen-spawn-inspector (scope)
  (let ((evals numen-evals)
        (repl-buffer (with-repl-buffer (current-buffer))))
    (with-current-buffer (generate-new-buffer "*Numen Inspector*")
      (set (make-local-variable 'numen-buffer-id) (incf numen-counter))
      (set (make-local-variable 'numen-evals) evals)
      (set (make-local-variable 'numen-views) (make-hash-table))
      (set (make-local-variable 'numen-repl-buffer) repl-buffer)
      (set (make-local-variable 'numen-secondary-p) t)
      (setq buffer-read-only t)
      (current-buffer))))

(defun numen-inspector-buffer-p (&optional buffer)
  (string-match "\*Numen Inspector\*" (buffer-name buffer)))

(defun numen-inspector-mouse-left-click ()
  "Delegate to `numen-inspector-return' when appropriate.
Currently that's only for clicking on script locations."
  (interactive)
  (when (numen-script-location-p)
    (numen-inspector-return)))

(defun numen-inspector-quit ()
  "In the REPL, return to prompt. In an inspector buffer, kill
the buffer and return to REPL."
  (interactive)
  (cond ((numen-inspector-buffer-p) (numen-kill-secondary-buffer))
        (t (numen-switch-to-repl))))

;;;; eval store

(defun numen-have-len (val)
  "If VAL is a composite, return the number of its children that
we have received from the eval process. If VAL is a string,
return the number of characters received."
  (length (or (hget val :vals) (hget val :str))))

(defun numen-scope-next-sibling (scope)
  (wlet (val (numen-find-val (cdr scope)))
    (wlet (view (numen-view (id scope)))
      (when (< (car scope) (1- (numen-how-many-to-display view val)))
        (cons (1+ (car scope)) (cdr scope))))))

(defun numen-scope-forward (scope)
  (cond ((null (cdr scope)) scope)
        (t (or (numen-scope-next-sibling scope)
               (numen-scope-forward (cdr scope))))))

(defun numen-scope-backward (scope)
  (cond ((null (cdr scope)) scope)
        ((> (car scope) 0) (cons (1- (car scope)) (cdr scope)))
        (t (numen-scope-up scope))))

(defun numen-scope-up (scope)
  (or (cdr scope) scope))

(defun numen-scope-downish (scope)
  "Return the next sibling scope that has a child (whether or not
it's displayed)."
  (loop do (setq scope (numen-scope-next-sibling scope))
        when (or (null scope) (hget (numen-find-val scope) :vals))
        do (return scope)))

(defun numen-scope-down (scope)
  (let ((val (numen-find-val scope)))
    (acond ((> (length (hget val :vals)) 0) (cons 0 scope))
           ((numen-scope-downish scope) it)
           (t (numen-scope-forward scope)))))

(defun numen-splice-details (parent details from)
  (hbind (keys vals truelen str) details
    (assert (= (hget parent :truelen) truelen))
    (cond (vals (assert (= from (length (hget parent :vals))))
                (hset parent :vals (vconcat (hget parent :vals) vals))
                (when keys
                  (assert (= from (length (hget parent :keys))))
                  (hset parent :keys (vconcat (hget parent :keys) keys)))
                (numen-assign-vkeys-to-children parent from))
          (str (hset parent :str (concat (hget parent :str) str)))
          (t (error "Unexpected details: not an array, object, nor string.")))))

(defun numen-vkey-bit (pval n)
  "Given a parent value PVAL and the index N of a child value,
return a string that can be concatenated to the vkey (unique
name) of PVAL to produce the vkey of its Nth child. When PVAL is
nil, return the vkey for element N of `numen-evals'."
  (let ((keys (hget pval :keys)))
    (cond ((null pval) (format "$[%d]" n))
          ((null keys) (format "[%d]" n))
          ((string-match "^[0-9]+$" (elt keys n)) (format "[%s]" (elt keys n)))
          (t (format ".%s" (elt keys n))))))

(defun* numen-assign-vkeys-to-children (parent &optional (from 0))
  (wlet (vals (hget parent :vals))
    (loop for n from from below (length vals) for v = (elt vals n) do
          (setf (vkey v) (concat (vkey parent) (numen-vkey-bit parent n)))
          (numen-assign-vkeys-to-children v))))

(defun numen-find-val (scope)
  (wbind (id . ns) (reverse scope)
    (let ((obj (gethash id numen-evals)))
      (loop for n in ns while obj for vals = (hget obj :vals) do
            (setq obj (when (< n (length vals)) (elt vals n))))
      obj)))

(defun numen-find-val-or-report-deletion (scope)
  (acond ((numen-find-val scope) it)
         (t (message "Client has deleted this value. You can configure how many values are stored with `numen-max-stored-eval-results'.")
            nil)))

(defun* numen-walk-val (proc val max-depth &optional scope (cur-depth 0))
  (when (<= cur-depth max-depth)
    (funcall proc val cur-depth scope)
    (when (< cur-depth max-depth)
      (loop for v across (hget val :vals) for n from 0 do
            (numen-walk-val proc v max-depth (when scope (cons n scope)) (1+ cur-depth))))))

(defun numen-store-val (val id)
  (setf (vkey val) (numen-vkey-bit nil id))
  (numen-assign-vkeys-to-children val)
  (setf (gethash id numen-evals) val)
  (when (> (hash-table-count numen-evals) numen-max-stored-eval-results)
    (numen-prune-evals)))

(defun numen-prune-evals ()
  (lexical-let ((ks '()))
    (maphash (lambda (k v) (push k ks)) numen-evals)
    (let ((old (nthcdr numen-max-stored-eval-results (sort ks (lambda (a b) (> a b))))))
      (loop for k in old do (remhash k numen-evals))
      (loop for views in (numen-all-views) do
            (loop for k in old do (remhash k views))))))

;;;; view store

(defun numen-view (id)
  (gethash id numen-views))

(defun numen-restriction (id)
  (pget (numen-view id) :restriction))

(defun numen-hiding-p (view vkey)
  (member vkey (pget view :hiding)))

(defun numen-hide (view vkey)
  (pset view :hiding (cons vkey (pget view :hiding))))

(defun numen-unhide (view vkey)
  (pset view :hiding (remove vkey (pget view :hiding))))

(defun numen-how-many-to-display (view val)
  (min (acond ((member (vkey val) (pget view :up-to)) (second it))
              (t (hget val :truelen)))
       (numen-have-len val)))

(defun numen-set-display-len (view val dlen)
  (unless (< dlen (hget val :truelen))
    (setq dlen nil))
  (let* ((up-to (pget view :up-to))
         (cur (second (member (vkey val) up-to))))
    (unless (equal dlen cur)
      (cond ((and dlen cur)        ; modify existing (copy then write)
             (setq up-to (copy-sequence up-to))
             (setf (second (member (vkey val) up-to)) dlen))
            (dlen (setq up-to (append (list (vkey val) dlen) up-to))) ; add new
            (t (setq up-to (loop for (vkey num) on up-to by #'cddr ; copy without
                                 unless (equal vkey (vkey val))
                                 append (list vkey num)))))
      (pset view :up-to up-to)
      t)))

(defun numen-set-view-state-to-breadcrumb (view crumb)
  (lexical-let ((changed-p nil))
    (cl-flet ((set% (prop) (unless (equal (pget crumb prop) (pget view prop))
                             (pset view prop (pget crumb prop))
                             (setq changed-p t))))
      (set% :restriction)
      (set% :hiding)
      (set% :up-to))
    changed-p))

(defun numen-push-breadcrumb (crumb view prop)
  (assert (or (eq prop :undo) (eq prop :redo)))
  (unless (equal crumb (car (pget view prop)))
    (pset view prop (cons crumb (pget view prop)))
    t))

(defun numen-pop-breadcrumb (view prop)
  (assert (or (eq prop :undo) (eq prop :redo)))
  (prog1 (car (pget view prop))
    (pset view prop (cdr (pget view prop)))))

(defun numen-make-breadcrumb (scope rel-pos view)
  (list :scope scope :pos rel-pos :restriction (pget view :restriction)
        :hiding (pget view :hiding) :up-to (pget view :up-to)))

(defun numen-make-view (&optional view)
  (list :extents nil :start-pos nil :undo nil :redo nil
        :restriction (pget view :restriction)
        ;; we copy these on write, so reusing references is ok
        :hiding (pget view :hiding) :up-to (pget view :up-to)))

(defun numen-all-views ()
  (loop for buffer in (buffer-list)
        when (eq numen-evals (buffer-local-value 'numen-evals buffer))
        collect (buffer-local-value 'numen-views buffer)))

;;;; input ring

(defun numen-input-search-in-progress-p ()
  (member last-command '(numen-previous-input numen-next-input)))

(defun numen-previous-input ()
  (interactive)
  (with-repl-buffer
   (numen-input-search :backward)))

(defun numen-next-input ()
  (interactive)
  (with-repl-buffer
   (numen-input-search :forward)))

(defun numen-input-search (dir)
  (cond ((ring-empty-p numen-input-ring)
         (message "No history"))
        (t (unless (numen-input-search-in-progress-p)
             (setq numen-search-prefix (buffer-substring-no-properties (numen-prompt-end) (point))))
           (let ((match (numen-input-position-matching numen-search-prefix dir)))
             (cond (match (numen-select-input-at-index match))
                   (t (message "No match")))))))

(defun numen-step-ring-index (index len dir)
  (if (eq dir :forward)
      (ring-minus1 index len)
    (ring-plus1 index len)))

(defun numen-input-position-matching (prefix dir)
  (let* ((len (ring-length numen-input-ring))
         (i (numen-step-ring-index numen-input-ring-index len dir))
         (n 0))
    (catch 'found
      (while (<= (incf n) len)
        (let ((prev (ring-ref numen-input-ring i)))
          (cond ((and (<= (length prefix) (length prev))
                      (string= prefix (substring prev 0 (length prefix))))
                 (throw 'found i))
                (t (setq i (numen-step-ring-index i len dir)))))))))

(defun numen-select-input-at-index (index)
  (message "History item %d" index)
  (setq numen-input-ring-index index)
  (delete-region (numen-prompt-end) (point-max))
  (insert (numen-trim (ring-ref numen-input-ring index))))

(defun numen-add-to-input-ring (input)
  (unless (and (not (ring-empty-p numen-input-ring))
               (equal input (ring-ref numen-input-ring 0)))
    (ring-insert numen-input-ring input)))

;;;; utilities

(defun* numen-display-buffer-in-preferred-window (&optional (buffer (current-buffer)))
  "When BUFFER is not visible, display it in `numen-preferred-window',
provided that window is live and not dedicated. Otherwise display
BUFFER using `pop-to-buffer'.

Then update `numen-preferred-window' so BUFFER will show up in
the same window the next time this function is called."
  (with-current-buffer buffer
    (when numen-preferred-window
      (when (and (null (get-buffer-window buffer))
                 (window-live-p numen-preferred-window)
                 (not (window-dedicated-p numen-preferred-window)))
        (set-window-buffer numen-preferred-window buffer))
      (pop-to-buffer buffer)
      (setq numen-preferred-window (selected-window)))))

(defun* numen-cycle-window (&optional (buffer (current-buffer)))
  "Display BUFFER in window returned by `next-window', unless
that window is dedicated, in which case keep calling
`next-window'. If there is a buffer-local variable
NUMEN-PREFERRED-WINDOW, set it to the new window."
  (interactive)
  (with-current-buffer buffer
    (let ((w^ (or numen-preferred-window (get-buffer-window) (selected-window))))
      (loop for w = (next-window w^) until (or (not (window-dedicated-p w)) (eq w w^))
            do (setq w (next-window w))
            finally do
            (cond (numen-preferred-window
                   (setq numen-preferred-window w)
                   (replace-buffer-in-windows buffer)
                   (numen-display-buffer-in-preferred-window buffer))
                  (t (replace-buffer-in-windows buffer)
                     (set-window-buffer w buffer)
                     (select-window w)))))))

(defun numen-strip-newlines (string)
  (replace-regexp-in-string "\n" "" string))

(defun numen-process-running-p (process)
  (when (processp process)
    (member (process-status process) '(run open))))

(defun numen-pad-to-width (strs)
  (let ((width (loop for str in strs maximize (length str))))
    (loop for str in strs for pad = (- width (length str))
          collect (concat str (make-string pad ?\ )))))

(defun numen-property-intervals (start end prop)
  "Return a list of all the positions between START and END at
which the specified text property has a value change. START and
END are always included."
  (let ((positions (list start)))
    (loop for pos = start then next
          for next = (next-single-property-change pos prop nil end)
          do (push next positions)
          until (= next end))
    (nreverse positions)))

(defun numen-find-previous-property-bounds (prop &optional limit)
  "Search backward from point until text property PROP is non-nil,
then return the start and end positions of the contiguous
interval over which PROP has the same value."
  (dbind (start end) (numen-property-bounds prop)
    (cond ((get-text-property start prop) (list start end))
          ((> start (or limit (point-min)))
           (save-excursion
             (goto-char (1- start))
             (numen-find-previous-property-bounds prop limit))))))

(defun numen-property-bounds (prop)
  "Return the positions of the previous and next changes to text
property PROP."
  (let ((end (next-single-property-change (point) prop nil (point-max))))
    (list (previous-single-property-change end prop nil (point-min)) end)))

(defun numen-flash-region (start end &optional secs face)
  "Highlight the region from START to END for SECS seconds, then
clear the highlighting. SECS defaults to 0.25 and FACE to 'match."
  (lexical-let ((overlay (make-overlay start end)))
    (run-with-timer (or secs 0.25) nil #'delete-overlay overlay)
    (overlay-put overlay 'face (or face 'match))))

(defun numen-flash-entire-line (&optional secs face)
  (let ((pos (line-beginning-position))
        (pos+ (line-end-position)))
    (save-excursion
      (forward-line)
      (setq pos+ (max pos+ (line-beginning-position))))
    (numen-flash-region pos pos+ secs face)))

(defun numen-trim (string) ; http://xahlee.blogspot.com/2011/09/emacs-lisp-function-to-trim-string.html
  "Remove whitespace at beginning and end of STRING.
Whitespace here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun numen-trailing-spaces (str)
  "Number of spaces at end of STR."
  (- (length str) (or (string-match " +$" str) (length str))))

(defun numen-parse-host-and-port (hostport)
  "Return a pair (HOST PORT). If HOSTPORT is a string of the form
\"host:port\", split it into its two pieces; if it is a number or
a numeric string, take that as the port and NIL as the host; if
it is a non-numeric string, take that as the host and NIL as the
port; otherwise NIL for both."
  (let ((h nil) (p nil))
    (cond ((stringp hostport)
           (dbind (&optional x y) (split-string hostport ":" t)
             (when (equal x "") (setq x nil))
             (when (equal y "") (setq y nil))
             (cond (y (setq h x p (string-to-number y)))
                   ((string-match "^[0-9]+$" (or x ""))
                    (setq p (string-to-number x)))
                   (t (setq h x)))))
          ((numberp hostport)
           (setq p hostport)))
    (when hostport
      (list h p))))

(defun* numen-plist-to-hash (plist &optional (test 'eq))
  (let ((table (make-hash-table :test test)))
    (loop for (k v) on plist by #'cddr do (setf (gethash k table) v))
    table))

(defun numen-listify (obj)
  (if (vectorp obj) (loop for x across obj collect (numen-listify x)) obj))

(defun numen-str (str &optional face &rest props)
  "Return a copy of str with text properties for FACE and
key-value pairs PROPS. If neither FACE nor PROPS is specified,
simply return STR."
  (when face (setq props (append (list 'face face) props)))
  (when props
    (setq str (copy-sequence str))
    (add-text-properties 0 (length str) props str))
  str)

(add-hook 'javascript-mode-hook 'numen-minor-mode)
(provide 'numen)
