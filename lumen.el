(require 'slime)
(require 'paredit)
(require 'numen)

;;; Major mode for Lumen files

(defvar lumen-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-shared-map)
    (define-key map (kbd "RET") 'paredit-newline)
    (define-key map (kbd "C-c j") 'lumen-code-expand)
    (define-key map (kbd "C-c C-c") 'lumen-eval-defun)
    map)
  "Keymap for Lumen mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(define-derived-mode lumen-mode lisp-mode "Lumen"
  "Major mode for editing Lumen code.

\\{lumen-mode-map}")

(defun lumen-code-expand ()
  (interactive)
  (let ((str (slime-sexp-at-point)))
    (cond ((null str) (message "End of file"))
          (t (with-repl-buffer
              (numen-send-request (list :compile str)))))))

(defun lumen-handle-repl-message (msg)
  "Intercept messages sent from the Numen JavaScript server to
Emacs and handle the ones that are intended for Lumen."
  (acond ((hget msg :evaluation)
          (wlet (stuff (hget msg :stuff))
            (wlet (src (hget stuff :evaldefun))
              (let ((s (lumen--substring (numen-strip-newlines src) 0 50)))
                (numen-output s 'numen-console-face))
              (numen-fresh-line)
              t)))
         ((hget msg :compiled)
          (lumen-display-compiled-code it)
          t)))

(add-hook 'numen-repl-message-hook 'lumen-handle-repl-message)

(defun lumen-display-compiled-code (js)
  (with-current-buffer (numen-update-secondary-source-buffer "lumen-to-js" js)
    (numen-display-buffer-in-preferred-window)
    (goto-char (point-min))))

(defun lumen-eval-defun (&optional arg)
  (interactive)
  (let ((src (lumen--top-sexp)))
    (with-repl-buffer
     (numen-send-request (list :evaluate src) (list :evaldefun src)))))

(defun lumen--top-sexp ()
  "Return a string containing the topmost sexp at point."
  (save-excursion
    (mark-defun)
    (numen-trim (buffer-substring-no-properties (point) (mark)))))

(defun lumen--substring (string from &optional to)
  "Like `substring' but doesn't raise an error if FROM or TO
  exceed the length of STRING."
  (substring string (min from (length string))
             (when to (min to (length string)))))

(add-hook 'lumen-mode-hook
          (lambda ()
            ;; Slime messes with the keyboard shortcuts defined above
            ;; and shouldn't be necessary for editing Lumen files.
            (slime-mode -1)
            (when (fboundp 'slime-autodoc-mode)
              (slime-autodoc-mode -1))))

;;; Extend the Numen REPL for Lumen

(defun start-lumen-repl ()
  (interactive)
  (define-key numen-mode-map (kbd "RET") 'lumen-newline-and-indent)
  (define-key numen-mode-map (kbd "M-RET") 'numen-return)
  (define-key numen-mode-map (kbd "C-c j") 'lumen-code-expand)
  (run-numen nil nil t))

(defun lumen-newline-and-indent ()
  (interactive)
  (save-restriction
    (let ((inhibit-read-only t))
      (cond ((get-text-property (point) 'old-input)
             (numen-grab-old-input)
             (goto-char (numen-prompt-end))
             (paredit-ignore-sexp-errors (indent-sexp))
             (goto-char (point-max)))
            ((numen-current-input)
             (narrow-to-region (numen-prompt-start) (point-max))
             (paredit-newline))
            ;; let empty input produce a new prompt (feels more like a terminal)
            (t (numen-send-input))))))

(add-hook 'numen-startup-hook (lambda () (paredit-mode 1)))

(provide 'lumen)
