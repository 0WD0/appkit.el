;;; appkit-ui.el --- Shared UI rendering primitives for appkit -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Small shared insertion helpers used by room/root renderers.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'svg nil t)

;;; ── Vertical bar (SVG in GUI, Unicode in terminal) ──────────────────

(defvar appkit-ui--vbar-image-cache (make-hash-table :test #'equal)
  "Cache of SVG vertical-bar images keyed by (COLOR XH CHAR-W).")

(defun appkit-ui--face-foreground-color (face)
  "Extract foreground colour string from FACE.
FACE may be a symbol, a plist (:foreground ...) or a list of faces."
  (cond
   ((and (listp face) (plist-get face :foreground))
    (plist-get face :foreground))
   ((symbolp face)
    (ignore-errors (face-foreground face nil t)))
   ((and (listp face) (symbolp (car face)))
    (ignore-errors (face-foreground (car face) nil t)))))

(defun appkit-ui--create-vbar-svg (face)
  "Create an SVG vertical-bar image coloured with FACE.
The image fills the full line height so consecutive lines join seamlessly.
Returns nil in terminal frames or when SVG is unavailable."
  (when (and (display-graphic-p)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-rectangle))
    (let* ((xh (or (ignore-errors (default-line-height))
                   (frame-char-height) 16))
           (char-w (max 1 (frame-char-width)))
           (bar-w (max 3 (round (* char-w 0.18))))
           (color (or (appkit-ui--face-foreground-color face)
                      (face-foreground 'default nil t)
                      "gray"))
           (key (list color xh char-w)))
      (or (gethash key appkit-ui--vbar-image-cache)
          (let* ((svg (svg-create char-w xh))
                 (_ (svg-rectangle svg 0 0 bar-w xh
                                   :fill-opacity 1 :fill color))
                 (data (with-temp-buffer (svg-print svg) (buffer-string)))
                 (image (create-image data 'svg t
                                      :scale 1.0
                                      :width char-w :height xh
                                      :ascent 'center)))
            (when image
              (puthash key image appkit-ui--vbar-image-cache))
            image)))))

(defun appkit-ui--vbar-string (face)
  "One-column vertical bar string coloured with FACE.
GUI frames get an SVG image; terminal frames get a plain ▏ character."
  (let ((image (appkit-ui--create-vbar-svg face)))
    (if image
        (propertize " " 'display image 'rear-nonsticky '(display))
      (if face (propertize "▏" 'face face) "▏"))))

;;; ── Buttons & styled lines ──────────────────────────────────────────

(defconst appkit-ui-action-property 'appkit-ui-action
  "Text property holding the zero-argument function for an action span.")

(defvar appkit-ui-action-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'appkit-ui-activate)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'appkit-ui-activate)
    map)
  "Keymap used by inline Appkit action spans.")

(defun appkit-ui--event-position (&optional event)
  "Return the buffer position for EVENT, or point when EVENT is not a click."
  (or (and event
           (eventp event)
           (posn-point (event-start event)))
      (point)))

(defun appkit-ui-action-at (&optional position)
  "Return the Appkit action at POSITION or point.

POSITION may sit on the action or immediately after it."
  (let ((pos (or position (point))))
    (when (and (integer-or-marker-p pos)
               (<= (point-min) pos)
               (<= pos (point-max)))
      (or (and (< pos (point-max))
               (get-text-property pos appkit-ui-action-property))
          (and (> pos (point-min))
               (get-text-property (1- pos) appkit-ui-action-property))))))

(defun appkit-ui-activate-at (&optional position)
  "Run the Appkit action at POSITION or point.

Return non-nil when an action ran."
  (when-let* ((action (appkit-ui-action-at position)))
    (funcall action)
    t))

(defun appkit-ui-activate (&optional event)
  "Activate the Appkit action at point or EVENT.

When EVENT is a click, move point to that position first so the action
sees the same entry the user clicked."
  (interactive (list last-nonmenu-event))
  (let ((position (appkit-ui--event-position event)))
    (when (and event
               (integer-or-marker-p position)
               (/= position (point)))
      (goto-char position))
    (unless (appkit-ui-activate-at position)
      (user-error "No action at point"))))

(cl-defun appkit-ui-add-action (start end action &key help-echo face mouse-face)
  "Make START..END an Appkit action span that calls ACTION.

ACTION is a zero-argument function.  HELP-ECHO describes that action.
FACE is appended when non-nil.  MOUSE-FACE defaults to `highlight'.
Do nothing when ACTION is not callable or the region is empty."
  (when (and (functionp action)
             (integer-or-marker-p start)
             (integer-or-marker-p end)
             (< start end))
    (add-text-properties
     start end
     (list appkit-ui-action-property action
           'keymap appkit-ui-action-map
           'mouse-face (or mouse-face 'highlight)
           'pointer 'hand
           'help-echo (or help-echo "Activate")))
    (when face
      (add-face-text-property start end face 'append))
    action))

(cl-defun appkit-ui-insert-action-button (label action
                                               &key face help-echo properties)
  "Insert clickable button LABEL calling ACTION.

ACTION is a no-arg function.  FACE, HELP-ECHO and PROPERTIES customize button
text properties passed to `insert-text-button'."
  (let ((button-props
         (append
          (list 'follow-link t
                'action (lambda (_button)
                          (funcall action)))
          (when help-echo
            (list 'help-echo help-echo))
          (when face
            (list 'face face))
          properties)))
    (apply #'insert-text-button label button-props)))

(defvar appkit-ui-action-row-map
  (let ((map (make-sparse-keymap)))
    ;; Keep primary-click activation direct.  In particular, do not inherit
    ;; `button-map' or bind `follow-link': either would let Emacs translate a
    ;; mouse-1 event before the button at the event position is dispatched.
    (define-key map (kbd "RET") #'push-button)
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'push-button)
    ;; Retain the traditional button activation gesture explicitly, without
    ;; inheriting any of the default button keymap's link behavior.
    (define-key map [mouse-2] #'push-button)
    map)
  "Keymap used by whole-row Appkit action buttons.")

(defun appkit-ui--activate-action-row (button)
  "Activate action-row BUTTON using its exact stored object and action."
  (let ((object (button-get button 'appkit-ui-action-row-object))
        (action (button-get button 'appkit-ui-action-row-action)))
    (funcall action object)))

(define-button-type 'appkit-ui-action-row-button
  'face nil
  'mouse-face nil
  'help-echo nil
  'follow-link nil
  'keymap appkit-ui-action-row-map
  'action #'appkit-ui--activate-action-row)

(cl-defun appkit-ui-make-action-row
    (start end object action &key help-echo mouse-face)
  "Make START..END an action row invoking ACTION with OBJECT.

ACTION must be callable.  HELP-ECHO supplies hover help, while MOUSE-FACE
controls hover highlighting explicitly and defaults to nil.  When END follows
a terminating newline, that newline is excluded from the button.  Action rows
must otherwise be single-line.  Return the text button, or nil when ACTION is
invalid, the resulting span is empty, or it contains an internal newline."
  (let ((row-start (and (integer-or-marker-p start)
                        (if (markerp start) (marker-position start) start)))
        (row-end (and (integer-or-marker-p end)
                      (if (markerp end) (marker-position end) end))))
    (when (and (functionp action)
               (integerp row-start)
               (integerp row-end)
               (<= (point-min) row-start)
               (<= row-start row-end)
               (<= row-end (point-max)))
      (when (and (< row-start row-end)
                 (eq (char-before row-end) ?\n))
        (setq row-end (1- row-end)))
      (when (and (< row-start row-end)
                 (not (save-excursion
                        (goto-char row-start)
                        (search-forward "\n" row-end t))))
        (make-text-button
         row-start row-end
         :type 'appkit-ui-action-row-button
         'mouse-face mouse-face
         'help-echo help-echo
         'follow-link nil
         'keymap appkit-ui-action-row-map
         'action #'appkit-ui--activate-action-row
         'appkit-ui-action-row-object object
         'appkit-ui-action-row-action action)))))

;;; ── Prefix state machinery ─────────────────────────────────────────

(defconst appkit-ui--prefix-state-tag 'appkit-ui-prefix-state
  "Internal marker used to identify `line-prefix' state objects.")

(defun appkit-ui-prefix-state-p (value)
  "Return non-nil when VALUE is an `appkit-ui' `line-prefix' state object."
  (and (vectorp value)
       (= (length value) 4)
       (eq (aref value 0) appkit-ui--prefix-state-tag)))

(defun appkit-ui-make-prefix-state (first-prefix rest-prefix)
  "Return mutable `line-prefix' state with FIRST-PREFIX and REST-PREFIX."
  (vector appkit-ui--prefix-state-tag first-prefix rest-prefix nil))

(defun appkit-ui-prefix-state-current (state)
  "Return current prefix string for prefix STATE without consuming it."
  (when (appkit-ui-prefix-state-p state)
    (if (aref state 3)
        (aref state 2)
      (or (aref state 1) (aref state 2)))))

(defun appkit-ui-prefix-state-rest (state)
  "Return rest-prefix string from prefix STATE."
  (when (appkit-ui-prefix-state-p state)
    (aref state 2)))

(defun appkit-ui-prefix-state-consume (state)
  "Return current prefix from STATE and mark first-prefix as consumed."
  (when (appkit-ui-prefix-state-p state)
    (let ((prefix (appkit-ui-prefix-state-current state)))
      (aset state 3 t)
      prefix)))

(defun appkit-ui-prefix-string (prefix &optional consume default)
  "Return normalized prefix string from PREFIX source.

When PREFIX is state and CONSUME is non-nil, consume its first-prefix.
DEFAULT is used when PREFIX yields nil."
  (or (cond
       ((appkit-ui-prefix-state-p prefix)
        (if consume
            (appkit-ui-prefix-state-consume prefix)
          (appkit-ui-prefix-state-current prefix)))
       ((stringp prefix) prefix)
       (t nil))
      (or default "")))

;;; ── Card line-prefix helpers ───────────────────────────────────────

(defun appkit-ui-combine-faces (&rest faces)
  "Return one face value from FACES, dropping nil entries."
  (let ((values (delq nil faces)))
    (cond
     ((null values) nil)
     ((null (cdr values)) (car values))
     (t values))))

(defvar appkit-ui-card-indent-prefix "    "
  "Dynamic base indent prefix used by `appkit-ui-card-line-prefix'.")

(defvar appkit-ui-card-indent-prefix-state nil
  "Dynamic `line-prefix' state used by card renderers in insertion scope.")

(cl-defun appkit-ui-card-line-prefix (&key face (indent appkit-ui-card-indent-prefix))
  "Return a display-only card prefix string.

FACE colours the vertical bar marker; INDENT is kept plain.  In GUI frames
the marker is an SVG image that fills the full line height so consecutive
lines produce a seamless vertical bar.  The marker replaces INDENT's last
column so card content stays column-aligned with normal lines."
  (let* ((base (or indent ""))
         (mark (appkit-ui--vbar-string face))
         (base-len (length base)))
    (if (> base-len 0)
        (concat (substring base 0 (1- base-len)) mark)
      mark)))

(cl-defun appkit-ui-card-prefix-state (&key face indent)
  "Return card `line-prefix' state for current insertion scope.

FACE styles the vertical border and INDENT supplies the default indentation.
When `appkit-ui-card-indent-prefix-state' is bound to a prefix state, this
function consumes its first prefix for the card's first row and uses its rest
prefix for subsequent card rows."
  (let* ((line-state appkit-ui-card-indent-prefix-state)
         (default-indent (or indent appkit-ui-card-indent-prefix))
         (first-indent (appkit-ui-prefix-string line-state t default-indent))
         (rest-indent (appkit-ui-prefix-string line-state nil default-indent)))
    (appkit-ui-make-prefix-state
     (appkit-ui-card-line-prefix :face face :indent first-indent)
     (appkit-ui-card-line-prefix :face face :indent rest-indent))))

;;; ── Line prefix application ────────────────────────────────────────

(defun appkit-ui--apply-line-prefix-span (start end line-prefix-str &optional wrap-prefix-str)
  "Apply line/wrap prefix strings to START..END span.

LINE-PREFIX-STR is prepended to existing `line-prefix'.  WRAP-PREFIX-STR
defaults to LINE-PREFIX-STR and is prepended to existing `wrap-prefix'."
  (when (< start end)
    (let* ((line (or line-prefix-str ""))
           (wrap (or wrap-prefix-str line))
           (existing-line (get-text-property start 'line-prefix))
           (existing-wrap (get-text-property start 'wrap-prefix)))
      (add-text-properties
       start end
       (list 'line-prefix (concat line (if (stringp existing-line) existing-line ""))
             'wrap-prefix (concat wrap (if (stringp existing-wrap) existing-wrap "")))))))

(defun appkit-ui-apply-line-prefix (start end prefix)
  "Apply PREFIX as display prefix for region START..END.

PREFIX can be a string or a mutable prefix-state created by
`appkit-ui-make-prefix-state'."
  (when (< start end)
    (if (appkit-ui-prefix-state-p prefix)
        (let* ((pos start)
               (first-prefix (appkit-ui-prefix-state-consume prefix))
               (rest-prefix (or (appkit-ui-prefix-state-rest prefix)
                                first-prefix))
               (line-prefix first-prefix))
          (save-excursion
            (goto-char start)
            (while (< pos end)
              (goto-char pos)
              (let* ((line-end (line-end-position))
                     (next-pos (if (< line-end end)
                                   (1+ line-end)
                                 end)))
                ;; Telega-like behavior: wrapped continuations use rest-prefix,
                ;; so avatar/image prefix is not repeated on visual wraps.
                (appkit-ui--apply-line-prefix-span pos next-pos line-prefix rest-prefix)
                (setq line-prefix rest-prefix)
                (setq pos next-pos)))))
      (appkit-ui--apply-line-prefix-span
       start end (appkit-ui-prefix-string prefix nil "")))))

;;; ── High-level inserters ───────────────────────────────────────────

(cl-defun appkit-ui-insert-prefixed-lines (prefix text &key face properties)
  "Insert TEXT as newline-separated lines using display-only PREFIX.

PREFIX can be a prefix string or prefix state.  FACE and PROPERTIES are applied
per inserted line so copied text stays clean."
  (dolist (line (split-string (or text "") "\n" nil))
    (let ((start (point)))
      (insert line "\n")
      (when (or face properties)
        (add-text-properties
         start
         (point)
         (append properties
                 (when face
                   (list 'face face)))))
      (appkit-ui-apply-line-prefix start (point) prefix))))

(defun appkit-ui-append-face (start end face)
  "Append FACE to region START..END."
  (when (and face (< start end))
    (add-face-text-property start end face 'append)))

(cl-defun appkit-ui-render-list-view (&key title summary loading-note
                                          items item-inserter empty-text
                                          footer-lines)
  "Render a simple list view block in current buffer.

TITLE, SUMMARY and LOADING-NOTE are optional header lines.
ITEMS are rendered by ITEM-INSERTER when present; otherwise EMPTY-TEXT is
inserted (defaults to `(empty)').  FOOTER-LINES is an optional list of lines
printed after the list with an extra separating blank line."
  (when title
    (insert title "\n"))
  (when summary
    (insert summary "\n"))
  (when loading-note
    (insert loading-note "\n"))
  (insert "\n")
  (if (and items (functionp item-inserter))
      (dolist (item items)
        (funcall item-inserter item))
    (insert (or empty-text "(empty)") "\n"))
  (when footer-lines
    (insert "\n")
    (dolist (line footer-lines)
      (insert line "\n"))))

(provide 'appkit-ui)

;;; appkit-ui.el ends here
