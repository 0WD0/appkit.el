;;; appkit-compose.el --- Shared standalone compose surfaces -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Provide a protocol-neutral standalone compose buffer.  Clients supply
;; context, status fields, attachment records, and transport actions; Appkit
;; owns the editable-body boundary, generated presentation invariants, and
;; in-flight submit presentation.  A surface may contain one or more ordered
;; editable parts.  Generated chrome lives in overlays so it cannot enter
;; undo or shift body positions.  Clients own protocol progress values and
;; cancel hooks.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst appkit-compose--divider "\n\n"
  "Fixed read-only separator kept between editable compose parts.")

(defconst appkit-compose--divider-properties
  '(read-only t
    rear-nonsticky (read-only)
    field appkit-compose-divider)
  "Text properties for the in-buffer part divider.")

(defvar-local appkit-compose--parts nil
  "Editable compose parts as plists with `:start', `:end', and `:overlay'.")

(defvar-local appkit-compose--footer-overlay nil
  "Overlay that displays generated compose footer text.")

(defvar-local appkit-compose--pending-bodies nil
  "Body strings consumed by the next `appkit-compose-refresh', or nil.")

(defvar-local appkit-compose-context-function nil
  "Function returning generated compose context text, or nil.")

(defvar-local appkit-compose-status-fields-function nil
  "Function returning generated compose status field plists, or nil.")

(defvar-local appkit-compose-attachments-function nil
  "Function returning a generated compose attachment section, or nil.")

(defvar-local appkit-compose-parts-function nil
  "Function returning a list of compose part plists, or nil.")

(defvar-local appkit-compose-footer-function nil
  "Function returning generated compose footer text, or nil.")

(defvar-local appkit-compose--submit nil
  "In-flight compose submit plist, or nil when idle.

Keys are `:state' (`submitting' or `cancelling'), optional `:label',
optional `:progress' as a 0-1 float, and optional `:cancel-function'.")

(defun appkit-compose--normalize-progress (progress)
  "Return PROGRESS as a 0-1 float, or nil."
  (cond
   ((null progress) nil)
   ((and (numberp progress) (<= 0 progress) (<= progress 1))
    (float progress))
   (t
    (error "Appkit compose submit progress must be nil or 0-1: %S"
           progress))))

(defun appkit-compose--normalize-label (label)
  "Return LABEL when it is a non-empty string, otherwise nil."
  (cond
   ((null label) nil)
   ((and (stringp label) (not (string-empty-p label))) label)
   (t (error "Appkit compose submit label must be a string: %S" label))))

(defun appkit-compose-submitting-p ()
  "Return non-nil when the current compose buffer has an in-flight submit."
  (and appkit-compose--submit t))

(defun appkit-compose-progress-bar (progress &optional width)
  "Return a WIDTH-column bar for PROGRESS, a 0-1 float.

WIDTH defaults to 10.  Nil or non-positive PROGRESS yields an empty
track.  The filled portion uses `=' characters with a trailing `>'."
  (let* ((width (max 1 (or width 10)))
         (ratio (if (and (numberp progress) (> progress 0))
                    (min 1.0 (max 0.0 (float progress)))
                  0.0))
         (filled (min width (round (* width ratio)))))
    (cond
     ((= filled 0) (make-string width ?\s))
     ((= filled 1) (concat ">" (make-string (1- width) ?\s)))
     (t (concat (make-string (1- filled) ?=)
                ">"
                (make-string (- width filled) ?\s))))))

(defun appkit-compose-progress-text ()
  "Return the current submit status string, or nil when idle.

When progress is known the string includes a bar and a percent.  Clients
own protocol meaning; this is only presentation."
  (when-let* ((submit appkit-compose--submit)
              (label (or (plist-get submit :label) "Submitting...")))
    (if-let* ((progress (plist-get submit :progress)))
        (format "%s  [%s]  %d%%"
                label
                (appkit-compose-progress-bar progress 10)
                (round (* 100 progress)))
      label)))

(cl-defun appkit-compose-begin-submit
    (&key label progress cancel-function)
  "Start a compose submit session in the current buffer.

LABEL is optional status text.  PROGRESS is an optional 0-1 float.
CANCEL-FUNCTION is an optional zero-argument hook the client uses to
abort transport.  Signal an error when a submit is already in flight."
  (when appkit-compose--submit
    (error "Appkit compose submit is already in progress"))
  (when (and cancel-function (not (functionp cancel-function)))
    (error "Appkit compose submit cancel function is not callable: %S"
           cancel-function))
  (setq-local appkit-compose--submit
              (list :state 'submitting
                    :label (or (appkit-compose--normalize-label label)
                               "Submitting...")
                    :progress (appkit-compose--normalize-progress progress)
                    :cancel-function cancel-function))
  appkit-compose--submit)

(cl-defun appkit-compose-update-submit
    (&key (label nil label-p)
          (progress nil progress-p)
          (cancel-function nil cancel-p))
  "Update the current compose submit session.

LABEL, PROGRESS, and CANCEL-FUNCTION replace the current values when
those keywords are supplied.  Omitted keyword arguments keep their
current values.  Signal an error when the surface is idle."
  (unless appkit-compose--submit
    (error "Appkit compose submit is not in progress"))
  (when label-p
    (setf (plist-get appkit-compose--submit :label)
          (or (appkit-compose--normalize-label label) "Submitting...")))
  (when progress-p
    (setf (plist-get appkit-compose--submit :progress)
          (appkit-compose--normalize-progress progress)))
  (when cancel-p
    (when (and cancel-function (not (functionp cancel-function)))
      (error "Appkit compose submit cancel function is not callable: %S"
             cancel-function))
    (setf (plist-get appkit-compose--submit :cancel-function)
          cancel-function))
  appkit-compose--submit)

(defun appkit-compose-finish-submit ()
  "Clear the current compose submit session.

Return the previous session plist, or nil when the surface was idle."
  (let ((submit appkit-compose--submit))
    (setq-local appkit-compose--submit nil)
    submit))

(defun appkit-compose-cancel-submit ()
  "Cancel the current compose submit when a cancel function is installed.

Return non-nil when cancellation was requested.  Return nil when the
surface is idle so the client can abandon the draft.  Signal
`user-error' when a submit is already cancelling, or is in flight
without a cancel function."
  (let ((submit appkit-compose--submit))
    (cond
     ((null submit) nil)
     ((eq (plist-get submit :state) 'cancelling)
      (user-error "The current submit is already being canceled"))
     ((not (functionp (plist-get submit :cancel-function)))
      (user-error "Wait for the current submit to finish"))
     (t
      (setf (plist-get submit :state) 'cancelling)
      (funcall (plist-get submit :cancel-function))
      t))))

(defun appkit-compose--clear-marker (marker)
  "Detach MARKER when it is a marker."
  (when (markerp marker)
    (set-marker marker nil)))

(defun appkit-compose--delete-overlay (overlay)
  "Delete OVERLAY when it is an overlay."
  (when (overlayp overlay)
    (delete-overlay overlay)))

(defun appkit-compose--clear-parts ()
  "Detach every compose part marker and overlay."
  (dolist (part appkit-compose--parts)
    (appkit-compose--clear-marker (plist-get part :start))
    (appkit-compose--clear-marker (plist-get part :end))
    (appkit-compose--delete-overlay (plist-get part :overlay)))
  (setq-local appkit-compose--parts nil)
  (appkit-compose--delete-overlay appkit-compose--footer-overlay)
  (setq-local appkit-compose--footer-overlay nil))

(defun appkit-compose--callback-value (function)
  "Call FUNCTION when callable, returning nil when it is nil."
  (when function
    (unless (functionp function)
      (error "Appkit compose callback is not callable: %S" function))
    (funcall function)))

(defvar appkit-compose-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'appkit-compose-invoke-overlay-action)
    (define-key map [mouse-2] #'appkit-compose-invoke-overlay-action)
    map)
  "Keymap used by clickable compose overlay chrome.")

(defun appkit-compose-invoke-overlay-action (event)
  "Invoke the compose overlay action at mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (spec (posn-string start))
         (action (if (consp spec)
                     (get-text-property (cdr spec)
                                        'appkit-compose-action
                                        (car spec))
                   (get-text-property (posn-point start)
                                      'appkit-compose-action))))
    (unless (functionp action)
      (user-error "No compose action at click"))
    (funcall action)))

(defun appkit-compose--action-string (text action &optional help-echo face)
  "Return TEXT that invokes ACTION when clicked.

HELP-ECHO and FACE are optional presentation properties."
  (let ((properties (list 'keymap appkit-compose-overlay-map
                          'appkit-compose-action action
                          'mouse-face 'highlight)))
    (when help-echo
      (setq properties (append (list 'help-echo help-echo) properties)))
    (when face
      (setq properties (append (list 'face face) properties)))
    (apply #'propertize text properties)))

(defun appkit-compose--field-text (field)
  "Return display text for status FIELD."
  (let ((label (plist-get field :label))
        (value (plist-get field :value)))
    (unless (stringp label)
      (error "Appkit compose status field needs a string label: %S" field))
    (if (null value)
        label
      (format "%s: %s" label value))))

(defun appkit-compose--status-field-string (field)
  "Return the generated string for one status FIELD."
  (unless (listp field)
    (error "Appkit compose status field must be a plist: %S" field))
  (let ((text (appkit-compose--field-text field))
        (action (plist-get field :action))
        (face (plist-get field :face))
        (help-echo (plist-get field :help-echo)))
    (when (and action (not (functionp action)))
      (error "Appkit compose status field action is not callable: %S" field))
    (cond
     (action (appkit-compose--action-string text action help-echo face))
     (face (propertize text 'face face))
     (t text))))

(defun appkit-compose--status-fields-string (fields)
  "Return generated status FIELDS as one action-capable line."
  (when fields
    (let ((pieces '())
          (index 0))
      (dolist (field fields)
        (when (> index 0)
          (push "   " pieces))
        (push (appkit-compose--status-field-string field) pieces)
        (setq index (1+ index)))
      (concat (apply #'concat (nreverse pieces)) "\n"))))

(defun appkit-compose--attachment-string (attachment)
  "Return the generated string for one ATTACHMENT row."
  (unless (listp attachment)
    (error "Appkit compose attachment must be a plist: %S" attachment))
  (let* ((preview (plist-get attachment :preview))
         (label (or (plist-get attachment :label) "[attachment]"))
         (description (plist-get attachment :description))
         (description-label (or (plist-get attachment :description-label)
                                "Description"))
         (state (plist-get attachment :state))
         (action (plist-get attachment :action))
         (object (if (plist-member attachment :object)
                     (plist-get attachment :object)
                   attachment))
         (help-echo (plist-get attachment :help-echo))
         (text "  "))
    (unless (stringp label)
      (error "Appkit compose attachment label must be a string: %S"
             attachment))
    (when (and action (not (functionp action)))
      (error "Appkit compose attachment action is not callable: %S"
             attachment))
    (when preview
      (setq text (concat text (propertize " " 'display preview
                                          'rear-nonsticky '(display)))))
    (setq text (concat text label))
    (when (and (stringp description)
               (not (string-empty-p description)))
      (setq text (concat text (format "  %s: %s"
                                      description-label description))))
    (when (and (stringp state)
               (not (string-empty-p state)))
      (setq text (concat text (format "  [%s]" state))))
    (setq text (concat text "\n"))
    (if action
        (appkit-compose--action-string
         text (lambda () (funcall action object)) help-echo)
      text)))

(defun appkit-compose--attachments-string (section)
  "Return generated attachment SECTION text.

SECTION is a plist with `:title', `:items', and optional `:empty-label'."
  (unless (listp section)
    (error "Appkit compose attachment section must be a plist: %S" section))
  (let ((title (or (plist-get section :title) "Attachments"))
        (items (plist-get section :items))
        (empty-label (or (plist-get section :empty-label)
                         "  No attachments.")))
    (unless (stringp title)
      (error "Appkit compose attachment title must be a string: %S" section))
    (unless (listp items)
      (error "Appkit compose attachment items must be a list: %S" section))
    (concat title "\n"
            (if items
                (mapconcat #'appkit-compose--attachment-string items "")
              (concat empty-label "\n")))))

(defun appkit-compose--header-string (context fields)
  "Return generated header text for CONTEXT and status FIELDS."
  (let ((field-text (appkit-compose--status-fields-string fields)))
    (when (or context field-text)
      (concat (or context "")
              (when (and context field-text) "\n")
              (or field-text "")
              "\n"))))

(defun appkit-compose--part-chrome-string (part &optional header)
  "Return generated chrome for PART, optionally prefixed by HEADER."
  (let ((title (plist-get part :title))
        (attachments (plist-get part :attachments)))
    (concat
     (or header "")
     (when (and (stringp title) (not (string-empty-p title)))
       (if (string-suffix-p "\n" title) title (concat title "\n")))
     (when attachments
       (appkit-compose--attachments-string attachments))
     (when (or (and (stringp title) (not (string-empty-p title)))
               attachments)
       "\n"))))

(defun appkit-compose--part-body (part)
  "Return the editable text stored in PART."
  (let ((start (plist-get part :start))
        (end (plist-get part :end)))
    (if (and (markerp start) (markerp end))
        (buffer-substring-no-properties start end)
      "")))

(defun appkit-compose-bodies ()
  "Return every editable compose body without text properties."
  (cond
   (appkit-compose--pending-bodies
    (copy-sequence appkit-compose--pending-bodies))
   (appkit-compose--parts
    (mapcar #'appkit-compose--part-body appkit-compose--parts))
   (t
    (list (buffer-substring-no-properties (point-min) (point-max))))))

(defun appkit-compose-set-bodies (bodies)
  "Use BODIES as the editable text on the next compose refresh.

BODIES must be a list of strings.  Call `appkit-compose-refresh' after
changing the part list so the pending bodies are applied."
  (unless (and (listp bodies)
               (cl-every #'stringp bodies))
    (error "Appkit compose bodies must be a list of strings: %S" bodies))
  (setq-local appkit-compose--pending-bodies (copy-sequence bodies)))

(defun appkit-compose-current-part-index ()
  "Return the 0-based compose part that contains point, or nil."
  (when appkit-compose--parts
    (let ((pos (point))
          (index 0)
          found)
      (dolist (part appkit-compose--parts)
        (let* ((start (and (markerp (plist-get part :start))
                           (marker-position (plist-get part :start))))
               (end (and (markerp (plist-get part :end))
                         (marker-position (plist-get part :end)))))
          (when (and start end (>= pos start) (<= pos end))
            (setq found index)))
        (setq index (1+ index)))
      (cond
       (found found)
       ((< pos (marker-position
                (plist-get (car appkit-compose--parts) :start)))
        0)
       (t (1- (length appkit-compose--parts)))))))

(defun appkit-compose--current-part ()
  "Return the compose part plist containing point, or the first part."
  (or (and appkit-compose--parts
           (nth (or (appkit-compose-current-part-index) 0)
                appkit-compose--parts))
      (car appkit-compose--parts)))

(defun appkit-compose-body-region-bounds ()
  "Return the editable compose body bounds for the current part, or nil."
  (when-let* ((part (appkit-compose--current-part))
              (start (plist-get part :start))
              (end (plist-get part :end))
              ((markerp start))
              ((markerp end)))
    (cons (marker-position start) (marker-position end))))

(defun appkit-compose-body-start-position ()
  "Return the start position of the current editable compose body, or nil."
  (car (appkit-compose-body-region-bounds)))

(defun appkit-compose-body-end-position ()
  "Return the end position of the current editable compose body, or nil."
  (cdr (appkit-compose-body-region-bounds)))

(defun appkit-compose-body ()
  "Return the current editable compose body without text properties."
  (if-let* ((bounds (appkit-compose-body-region-bounds)))
      (buffer-substring-no-properties (car bounds) (cdr bounds))
    ""))

(defun appkit-compose-goto-part (index)
  "Move point to the start of compose part INDEX."
  (when-let* ((part (nth index appkit-compose--parts))
              (start (plist-get part :start))
              ((markerp start)))
    (goto-char start)))

(defun appkit-compose--resolved-parts ()
  "Return the client part list, defaulting to one attachments-only part."
  (if appkit-compose-parts-function
      (let ((parts (appkit-compose--callback-value
                    appkit-compose-parts-function)))
        (unless (listp parts)
          (error "Appkit compose parts must be a list: %S" parts))
        (or parts (list nil)))
    (list (list :attachments
                (appkit-compose--callback-value
                 appkit-compose-attachments-function)))))

(defun appkit-compose--align-bodies (bodies count)
  "Return BODIES padded or truncated to COUNT strings."
  (let ((aligned (copy-sequence (or bodies '())))
        (needed count))
    (while (< (length aligned) needed)
      (setq aligned (append aligned (list ""))))
    (when (> (length aligned) needed)
      (setq aligned (cl-subseq aligned 0 needed)))
    aligned))

(defun appkit-compose--insert-divider ()
  "Insert the read-only separator between compose parts."
  (let ((start (point)))
    (insert appkit-compose--divider)
    (add-text-properties start (point)
                         appkit-compose--divider-properties)))

(defun appkit-compose--rebuild-skeleton (bodies)
  "Replace buffer text with editable BODIES and fixed dividers.

This discards undo history because body positions are rewritten."
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t)
        (buffer-undo-list t)
        (modified (buffer-modified-p))
        (index 0)
        rendered)
    (appkit-compose--clear-parts)
    (erase-buffer)
    (dolist (body bodies)
      (when (> index 0)
        (when-let* ((previous (car rendered))
                    (end (plist-get previous :end)))
          (set-marker-insertion-type end nil)
          (appkit-compose--insert-divider)
          (set-marker-insertion-type end t)))
      (let ((start (copy-marker (point) nil)))
        (insert (or body ""))
        (push (list :start start
                    :end (copy-marker (point) t))
              rendered))
      (setq index (1+ index)))
    (setq-local appkit-compose--parts (nreverse rendered))
    (set-buffer-modified-p modified))
  (setq buffer-undo-list nil))

(defun appkit-compose--make-overlay (start end front-advance rear-advance)
  "Return an overlay from START to END.

FRONT-ADVANCE and REAR-ADVANCE are passed to `make-overlay'."
  (make-overlay start end nil front-advance rear-advance))

(defun appkit-compose--refresh-overlays (spec-parts)
  "Rebuild generated overlays from SPEC-PARTS without touching bodies."
  (let* ((context (appkit-compose--callback-value
                   appkit-compose-context-function))
         (fields (appkit-compose--callback-value
                  appkit-compose-status-fields-function))
         (footer (appkit-compose--callback-value
                  appkit-compose-footer-function))
         (header (appkit-compose--header-string context fields))
         (index 0))
    (when (and fields (not (listp fields)))
      (error "Appkit compose status fields must be a list: %S" fields))
    (dolist (part appkit-compose--parts)
      (appkit-compose--delete-overlay (plist-get part :overlay))
      (let* ((start (plist-get part :start))
             (overlay (appkit-compose--make-overlay start start nil nil))
             (chrome (appkit-compose--part-chrome-string
                      (nth index spec-parts)
                      (and (zerop index) header))))
        (when (and chrome (not (string-empty-p chrome)))
          (overlay-put overlay 'before-string chrome))
        (overlay-put overlay 'evaporate nil)
        (setf (nth index appkit-compose--parts)
              (list :start start
                    :end (plist-get part :end)
                    :overlay overlay)))
      (setq index (1+ index)))
    (appkit-compose--delete-overlay appkit-compose--footer-overlay)
    (setq-local appkit-compose--footer-overlay
                (and footer
                     (not (string-empty-p footer))
                     (let ((overlay (appkit-compose--make-overlay
                                     (point-max) (point-max) t t)))
                       (overlay-put overlay 'after-string
                                    (concat "\n\n" footer))
                       (overlay-put overlay 'evaporate nil)
                       overlay)))))

(defun appkit-compose-display-string ()
  "Return visible compose text, including overlay chrome.

Use this when a test or command needs the generated context, status,
attachments, and footer together with the editable bodies.  The buffer
string itself contains only bodies and fixed part dividers."
  (let ((chunks '()))
    (dolist (part appkit-compose--parts)
      (when-let* ((overlay (plist-get part :overlay))
                  (text (overlay-get overlay 'before-string)))
        (push text chunks))
      (push (appkit-compose--part-body part) chunks)
      (when (cdr (memq part appkit-compose--parts))
        (push appkit-compose--divider chunks)))
    (when-let* ((overlay appkit-compose--footer-overlay)
                (text (overlay-get overlay 'after-string)))
      (push text chunks))
    (apply #'concat (nreverse chunks))))

(defun appkit-compose-call-display-action (needle)
  "Call the overlay action whose visible text contains NEEDLE.

NEEDLE is a fixed string compared against the current display text.
Signal an error when no matching action exists."
  (let* ((text (appkit-compose-display-string))
         (start (and (stringp needle)
                     (string-match (regexp-quote needle) text)))
         (action (and start
                      (get-text-property start 'appkit-compose-action text))))
    (unless (functionp action)
      (error "No compose display action matches %S" needle))
    (funcall action)))

(defun appkit-compose-refresh ()
  "Refresh generated compose presentation while preserving bodies.

Generated chrome is applied as overlays.  Editable bodies stay in the
buffer so ordinary Emacs undo remains valid.  The skeleton is rewritten
only when the part count changes or pending bodies are applied."
  (let* ((spec-parts (appkit-compose--resolved-parts))
         (bodies (appkit-compose--align-bodies
                  (appkit-compose-bodies) (length spec-parts)))
         (old-index (or (appkit-compose-current-part-index) 0))
         (old-start (appkit-compose-body-start-position))
         (point-offset (if (and old-start (>= (point) old-start))
                           (- (point) old-start)
                         0))
         (rebuild (or appkit-compose--pending-bodies
                      (/= (length appkit-compose--parts)
                          (length spec-parts)))))
    (setq-local appkit-compose--pending-bodies nil)
    (when rebuild
      (appkit-compose--rebuild-skeleton bodies))
    (appkit-compose--refresh-overlays spec-parts)
    (when-let* ((part (nth (min old-index
                                (1- (length appkit-compose--parts)))
                           appkit-compose--parts))
                (start (plist-get part :start)))
      (goto-char (+ (marker-position start)
                    (min point-offset
                         (length (nth (min old-index
                                           (1- (length bodies)))
                                      bodies))))))
    (appkit-compose-body-region-bounds)))

(cl-defun appkit-compose-setup
    (&key context-function status-fields-function attachments-function
          parts-function footer-function)
  "Configure generated compose callbacks and refresh the current buffer.

CONTEXT-FUNCTION returns a context string.  STATUS-FIELDS-FUNCTION returns a
list of field plists with `:label', `:value', and optional `:action'.
ATTACHMENTS-FUNCTION returns an attachment-section plist for a single part.
PARTS-FUNCTION returns a list of part plists with optional `:title' and
`:attachments'; when it is set, it replaces ATTACHMENTS-FUNCTION.
FOOTER-FUNCTION returns the generated footer string.  Client callbacks own
all protocol semantics and may be nil when a section is not needed."
  (dolist (entry `((:context . ,context-function)
                   (:status-fields . ,status-fields-function)
                   (:attachments . ,attachments-function)
                   (:parts . ,parts-function)
                   (:footer . ,footer-function)))
    (let ((function (cdr entry)))
      (when (and function (not (functionp function)))
        (error "Appkit compose %s callback is not callable: %S"
               (car entry) function))))
  (setq-local appkit-compose-context-function context-function
              appkit-compose-status-fields-function status-fields-function
              appkit-compose-attachments-function attachments-function
              appkit-compose-parts-function parts-function
              appkit-compose-footer-function footer-function)
  (appkit-compose-refresh))

(define-derived-mode appkit-compose-mode text-mode "Appkit-Compose"
  "Major mode for a standalone protocol-neutral compose surface.

Buffer text is only the editable bodies and fixed part dividers.
Generated context, status, attachments, and footer are overlays."
  (setq-local header-line-format nil)
  (setq-local require-final-newline nil)
  (setq-local appkit-compose--submit nil)
  (setq-local appkit-compose--parts nil)
  (setq-local appkit-compose--footer-overlay nil)
  (setq-local appkit-compose--pending-bodies nil))

(provide 'appkit-compose)

;;; appkit-compose.el ends here
