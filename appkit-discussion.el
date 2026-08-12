;;; appkit-discussion.el --- Threaded discussion presentation -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-neutral rows for post details, comments, reviews, and similar
;; threaded discussions.  Applications own identities, bodies, and actions;
;; Appkit owns the shared avatar, nesting, chain connector, heading, footer,
;; and navigation geometry.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)
(require 'appkit-ui)

(cl-defstruct (appkit-discussion-entry
               (:constructor appkit-discussion-entry-create))
  key
  parent-key
  depth
  avatar
  avatar-fallback
  avatar-action
  avatar-help-echo
  heading
  heading-inserter
  heading-face
  heading-line-face
  time
  time-face
  body-inserter
  footer
  footer-face
  connector
  properties)

(defconst appkit-discussion-key-property 'appkit-discussion-key
  "Text property carrying an opaque discussion entry key.")

(defconst appkit-discussion-parent-key-property 'appkit-discussion-parent-key
  "Text property carrying an opaque parent discussion entry key.")

(defconst appkit-discussion-depth-property 'appkit-discussion-depth
  "Text property carrying a discussion entry's visual nesting depth.")

(defface appkit-discussion-connector
  '((t :inherit shadow))
  "Face used for a linear discussion chain connector."
  :group 'appkit)

(defun appkit-discussion--validate-entry (entry)
  "Require a complete, internally consistent discussion ENTRY."
  (unless (appkit-discussion-entry-p entry)
    (error "Appkit discussion entry is not an entry object"))
  (unless (appkit-discussion-entry-key entry)
    (error "Appkit discussion entry has no stable key"))
  (let ((depth (or (appkit-discussion-entry-depth entry) 0)))
    (unless (and (integerp depth) (>= depth 0))
      (error "Appkit discussion depth must be a non-negative integer"))
    (when (and (> depth 0)
               (null (appkit-discussion-entry-parent-key entry)))
      (error "Nested Appkit discussion entry has no parent key")))
  (let ((connector (appkit-discussion-entry-connector entry)))
    (unless (memq connector '(nil continue end))
      (error "Appkit discussion connector must be nil, continue, or end")))
  (when (and (appkit-discussion-entry-heading entry)
             (appkit-discussion-entry-heading-inserter entry))
    (error "Appkit discussion entry has two heading sources"))
  (dolist (slot '(heading-inserter body-inserter avatar-action))
    (when-let* ((value (pcase slot
                         ('heading-inserter
                          (appkit-discussion-entry-heading-inserter entry))
                         ('body-inserter
                          (appkit-discussion-entry-body-inserter entry))
                         ('avatar-action
                          (appkit-discussion-entry-avatar-action entry)))))
      (unless (functionp value)
        (error "Appkit discussion %s is not callable" slot))))
  entry)

(defun appkit-discussion--entry-properties (entry)
  "Return standard and application-owned text properties for ENTRY."
  (let ((cursor (appkit-discussion-entry-properties entry))
        (reserved (list appkit-discussion-key-property
                        appkit-discussion-parent-key-property
                        appkit-discussion-depth-property
                        'rear-nonsticky))
        custom
        custom-rear)
    (while cursor
      (unless (cdr cursor)
        (error "Appkit discussion entry properties are not a valid plist"))
      (let ((property (pop cursor))
            (value (pop cursor)))
        (if (eq property 'rear-nonsticky)
            (setq custom-rear value)
          (unless (memq property reserved)
            (setq custom (append custom (list property value)))))))
    (unless (or (null custom-rear)
                (eq custom-rear t)
                (listp custom-rear))
      (error "Appkit discussion rear-nonsticky must be t or a property-name list"))
    (append
     (list appkit-discussion-key-property
           (appkit-discussion-entry-key entry)
           appkit-discussion-parent-key-property
           (appkit-discussion-entry-parent-key entry)
           appkit-discussion-depth-property
           (or (appkit-discussion-entry-depth entry) 0)
           'rear-nonsticky
           (if (eq custom-rear t)
               t
             (delete-dups
              (append
               (list appkit-discussion-key-property
                     appkit-discussion-parent-key-property
                     appkit-discussion-depth-property)
               (and (listp custom-rear) custom-rear)))))
     custom)))

(defun appkit-discussion--avatar-properties (entry)
  "Return interaction properties for ENTRY's avatar prefixes."
  (when-let* ((action (appkit-discussion-entry-avatar-action entry)))
    (let ((map (make-sparse-keymap))
          (command
           (lambda (&optional _event)
             (interactive)
             (funcall action))))
      (define-key map (kbd "RET") command)
      (define-key map [mouse-1] command)
      (list 'keymap map
            'mouse-face 'highlight
            'help-echo
            (or (appkit-discussion-entry-avatar-help-echo entry)
                "Open avatar")))))

(defun appkit-discussion--decorate-prefix (prefix properties)
  "Return a copy of PREFIX carrying PROPERTIES."
  (let ((copy (copy-sequence (or prefix ""))))
    (when (and properties (> (length copy) 0))
      (add-text-properties 0 (length copy) properties copy))
    copy))

(defun appkit-discussion--connector-column (connector line)
  "Return the prefix column for CONNECTOR on LINE.

CONNECTOR is nil, `continue', or `end'.  LINE is `header',
`first-body', `rest-body', or `separator'."
  (cond
   ((eq connector 'continue)
    (propertize "│ " 'face 'appkit-discussion-connector))
   ((and (eq connector 'end)
         (memq line '(header first-body)))
    (propertize "│ " 'face 'appkit-discussion-connector))
   ((eq connector 'end) "  ")
   (t "")))

(cl-defun appkit-discussion-insert-entry
    (entry &key width avatar-pixel-size (indent-width 4) (separate-p t)
           (avatar-p t))
  "Insert one threaded discussion ENTRY and return its buffer span.

WIDTH is the common right edge used by the timestamp.  AVATAR-PIXEL-SIZE
defaults to a two-line chat avatar.  INDENT-WIDTH is multiplied by ENTRY's
depth.  AVATAR-P controls whether the shared two-line avatar prefix is
reserved; when nil, only nesting indentation is applied.  ENTRY's connector
is nil, `continue', or `end': a continue mark draws a prefix spine through
the row and its trailing separator, and an end mark stops that spine after
the heading.  When SEPARATE-P is non-nil, append one blank line.

ENTRY's heading inserter is called with no arguments.  Its body inserter is
called with the mutable, display-only body prefix state and the complete row
properties.  Body inserters should apply that state through Appkit prefix
helpers instead of inserting it into buffer text."
  (appkit-discussion--validate-entry entry)
  (unless (and (integerp indent-width) (>= indent-width 0))
    (error "Appkit discussion indent width must be a non-negative integer"))
  (let* ((depth (or (appkit-discussion-entry-depth entry) 0))
         (connector (appkit-discussion-entry-connector entry))
         (indent (make-string (* depth indent-width) ?\s))
         (pixel-size (and avatar-p
                          (or avatar-pixel-size
                              (appkit-chat-avatar-two-line-pixel-size))))
         (avatar-properties
          (and avatar-p
               (appkit-discussion--avatar-properties entry)))
         (avatar-prefixes
          (and avatar-p
               (appkit-chat-avatar-prefixes
                (appkit-discussion-entry-avatar entry)
                (or (appkit-discussion-entry-avatar-fallback entry) "@")
                :pixel-size pixel-size
                :resize t)))
         (header-prefix
          (concat (appkit-discussion--connector-column connector 'header)
                  indent
                  (when avatar-prefixes
                    (appkit-discussion--decorate-prefix
                     (plist-get avatar-prefixes :header)
                     avatar-properties))))
         (first-body-prefix
          (concat (appkit-discussion--connector-column connector 'first-body)
                  indent
                  (when avatar-prefixes
                    (appkit-discussion--decorate-prefix
                     (plist-get avatar-prefixes :first-body)
                     avatar-properties))))
         (rest-body-prefix
          (concat (appkit-discussion--connector-column connector 'rest-body)
                  indent
                  (or (plist-get avatar-prefixes :rest-body) "")))
         (body-prefix
          (appkit-ui-make-prefix-state first-body-prefix rest-body-prefix))
         (properties (appkit-discussion--entry-properties entry))
         (start (point))
         (header-start (point)))
    (if-let* ((inserter (appkit-discussion-entry-heading-inserter entry)))
        (funcall inserter)
      (insert (or (appkit-discussion-entry-heading entry) "")))
    (let ((heading-end (point)))
      (when-let* ((face (appkit-discussion-entry-heading-face entry)))
        (add-face-text-property header-start heading-end face 'append))
      (when-let* ((time (appkit-discussion-entry-time entry)))
        (unless (string-empty-p time)
          (appkit-chat-ins-insert-right-aligned-text
           time (or width 80)
           :face (or (appkit-discussion-entry-time-face entry) 'shadow)
           :left-prefix-width (string-width header-prefix))))
      (insert "\n")
      (appkit-ui-apply-line-prefix
       header-start (point)
       (appkit-ui-make-prefix-state header-prefix rest-body-prefix))
      (when-let* ((face (appkit-discussion-entry-heading-line-face entry)))
        (add-face-text-property header-start (point) face 'append))
      (add-text-properties header-start (point) properties))
    (if-let* ((body-inserter (appkit-discussion-entry-body-inserter entry)))
        (funcall body-inserter body-prefix properties)
      ;; Keep the lower avatar slice visible for body-less entries.
      (appkit-ui-insert-prefixed-lines body-prefix "" :properties properties))
    (when-let* ((footer (appkit-discussion-entry-footer entry)))
      (unless (string-empty-p footer)
        (appkit-ui-insert-prefixed-lines
         body-prefix footer
         :face (or (appkit-discussion-entry-footer-face entry) 'shadow)
         :properties properties)))
    (when separate-p
      (let ((separator-start (point)))
        (insert "\n")
        (when (eq connector 'continue)
          (appkit-ui-apply-line-prefix
           separator-start (point)
           (appkit-discussion--connector-column connector 'separator)))))
    (add-text-properties start (point) properties)
    (cons start (point))))

(defun appkit-discussion-key-at-point (&optional position)
  "Return the opaque discussion key at POSITION or point."
  (let ((probe (or position (point))))
    (when (and (integer-or-marker-p probe)
               (<= (point-min) probe)
               (<= probe (point-max)))
      (or (and (< probe (point-max))
               (get-text-property probe appkit-discussion-key-property))
          (save-excursion
            (goto-char probe)
            (get-text-property (line-beginning-position)
                               appkit-discussion-key-property))))))

(defun appkit-discussion--entry-positions ()
  "Return start positions of discussion entries in the current buffer."
  (let ((position (point-min))
        (limit (point-max))
        positions
        previous-key)
    (while (< position limit)
      (let* ((key (get-text-property position appkit-discussion-key-property))
             (next (next-single-property-change
                    position appkit-discussion-key-property nil limit)))
        (when (and key (not (equal key previous-key)))
          (push position positions))
        (setq previous-key key
              position (if (> next position) next (1+ position)))))
    (nreverse positions)))

(defun appkit-discussion-next-position (&optional position)
  "Return the next discussion entry position after POSITION or point."
  (let ((probe (or position (point))))
    (seq-find (lambda (candidate) (> candidate probe))
              (appkit-discussion--entry-positions))))

(defun appkit-discussion-previous-position (&optional position)
  "Return the previous discussion entry position before POSITION or point."
  (let* ((probe (or position (point)))
         (current-key (appkit-discussion-key-at-point probe))
         (current-start
          (and current-key
               (seq-find
                (lambda (candidate)
                  (equal current-key
                         (get-text-property
                          candidate appkit-discussion-key-property)))
                (appkit-discussion--entry-positions))))
         (boundary (or current-start probe))
         previous)
    (dolist (candidate (appkit-discussion--entry-positions) previous)
      (when (< candidate boundary)
        (setq previous candidate)))))

(defun appkit-discussion-next-entry ()
  "Move point to the next discussion entry."
  (interactive)
  (if-let* ((position (appkit-discussion-next-position)))
      (goto-char position)
    (message "Appkit: no next discussion entry")))

(defun appkit-discussion-previous-entry ()
  "Move point to the previous discussion entry."
  (interactive)
  (if-let* ((position (appkit-discussion-previous-position)))
      (goto-char position)
    (message "Appkit: no previous discussion entry")))

(provide 'appkit-discussion)

;;; appkit-discussion.el ends here
