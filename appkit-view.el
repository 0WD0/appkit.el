;;; appkit-view.el --- Shared list and row presentation geometry -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Protocol-independent list, row, elision, and window geometry used by
;; root-style views.  Position preservation and keyed EWOC reconciliation
;; remain owned by `appkit-position' and `appkit-ewoc', respectively.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-ui)

(cl-defstruct (appkit-view-list-spec
               (:constructor appkit-view-list-spec-create))
  title
  summary
  loading-note
  items
  item-inserter
  empty-text
  footer-lines)

(defun appkit-view-render-list-spec (spec)
  "Render list SPEC in current buffer using `appkit-ui-render-list-view'."
  (appkit-ui-render-list-view
   :title (appkit-view-list-spec-title spec)
   :summary (appkit-view-list-spec-summary spec)
   :loading-note (appkit-view-list-spec-loading-note spec)
   :items (appkit-view-list-spec-items spec)
   :item-inserter (appkit-view-list-spec-item-inserter spec)
   :empty-text (appkit-view-list-spec-empty-text spec)
   :footer-lines (appkit-view-list-spec-footer-lines spec)))

(cl-defun appkit-view-render-list-spec-preserving-position
    (spec &key anchor-property preserve-window-start after-restore)
  "Render list SPEC and restore cursor/viewport context.

ANCHOR-PROPERTY, PRESERVE-WINDOW-START, and AFTER-RESTORE are forwarded to
`appkit-position-render-preserving'."
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (appkit-view-render-list-spec spec)
       (goto-char (point-min))))
   :anchor-property anchor-property
   :preserve-window-start preserve-window-start
   :after-restore after-restore))

(cl-defstruct (appkit-view-label-row
               (:constructor appkit-view-label-row-create))
  label
  prefix
  suffix
  icon-inserter
  icon-separator
  face
  line-properties
  help-echo
  mouse-face)

(defun appkit-view-insert-label-row (row)
  "Insert one simple label ROW."
  (let ((start (point)))
    (when-let* ((prefix (appkit-view-label-row-prefix row)))
      (insert prefix))
    (when-let* ((icon-inserter (appkit-view-label-row-icon-inserter row)))
      (funcall icon-inserter)
      (when-let* ((icon-separator (appkit-view-label-row-icon-separator row)))
        (insert icon-separator)))
    (insert (or (appkit-view-label-row-label row) ""))
    (when-let* ((suffix (appkit-view-label-row-suffix row)))
      (insert suffix))
    (insert "\n")
    (add-text-properties
     start
     (point)
     (append (or (appkit-view-label-row-line-properties row) '())
             (when-let* ((face (appkit-view-label-row-face row)))
               (list 'face face))
             (when-let* ((help-echo (appkit-view-label-row-help-echo row)))
               (list 'help-echo help-echo))
             (when-let* ((mouse-face (appkit-view-label-row-mouse-face row)))
               (list 'mouse-face mouse-face))))))

(cl-defun appkit-view-insert-label-line
    (label &key prefix suffix icon-inserter icon-separator
           face line-properties help-echo mouse-face)
  "Insert LABEL as one styled line.

PREFIX, SUFFIX, ICON-INSERTER, ICON-SEPARATOR, FACE, LINE-PROPERTIES,
HELP-ECHO, and MOUSE-FACE customize its presentation and interaction."
  (appkit-view-insert-label-row
   (appkit-view-label-row-create
    :label label
    :prefix prefix
    :suffix suffix
    :icon-inserter icon-inserter
    :icon-separator icon-separator
    :face face
    :line-properties line-properties
    :help-echo help-echo
    :mouse-face mouse-face)))

(cl-defun appkit-view-insert-heading-line
    (text &key face line-properties help-echo mouse-face)
  "Insert heading TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (appkit-view-insert-label-line
   text
   :face face
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defun appkit-view-insert-note-line
    (text &key face line-properties help-echo mouse-face)
  "Insert note TEXT using FACE, LINE-PROPERTIES, HELP-ECHO, and MOUSE-FACE."
  (appkit-view-insert-heading-line
   text
   :face (or face 'shadow)
   :line-properties line-properties
   :help-echo help-echo
   :mouse-face mouse-face))

(cl-defstruct (appkit-view-one-line-row
               (:constructor appkit-view-one-line-row-create))
  icon-inserter
  context
  context-open
  context-close
  context-trail
  context-trail-face
  preview
  time
  time-face
  time-tail-face
  line-properties
  help-echo
  mouse-face)

(defun appkit-view-canonicalize-number (spec base)
  "Resolve SPEC against BASE columns.

SPEC can be an integer, float ratio, or list (VALUE MIN MAX)."
  (let* ((raw (if (consp spec) (car spec) spec))
         (min-value (when (consp spec) (nth 1 spec)))
         (max-value (when (consp spec) (nth 2 spec)))
         (value (cond
                 ((integerp raw) raw)
                 ((floatp raw) (round (* raw base)))
                 ((numberp raw) (round raw))
                 (t base))))
    (when (numberp min-value)
      (setq value (max value min-value)))
    (when (numberp max-value)
      (setq value (min value max-value)))
    value))

(defun appkit-view-truncate-fill (text width &optional right-align)
  "Return TEXT truncated and padded to WIDTH.

When RIGHT-ALIGN is non-nil, pad on the left instead of right."
  (let* ((target (max 0 (or width 0)))
         (trimmed (truncate-string-to-width (or text "") target nil nil ""))
         (padding (max 0 (- target (string-width trimmed)))))
    (if right-align
        (concat (make-string padding ?\s) trimmed)
      (concat trimmed (make-string padding ?\s)))))

(defun appkit-view-elide-string (str max &optional face)
  "Return STR elided to MAX columns using display properties and FACE."
  (let* ((text (or str ""))
         (str-width (string-width text))
         (limit (max 0 (or max 0))))
    (if (<= str-width limit)
        text
      (let* ((elide-str "…")
             (elide-width (string-width elide-str))
             (elide-pos 1)
             (str-len (length text))
             (elide-trail (floor (* limit (- 1 elide-pos))))
             (trail-width
              (progn
                (while (and (> elide-trail 0)
                            (> (string-width text (- str-len elide-trail))
                               (floor (* limit (- 1 elide-pos)))))
                  (setq elide-trail (1- elide-trail)))
                (string-width text (- str-len elide-trail))))
             (elide-lead (- (min limit str-len) elide-width trail-width))
             (result (copy-sequence text)))
        (when (< elide-lead 0)
          (setq elide-lead 0))
        (while (and (> elide-lead 0)
                    (> (+ (string-width result 0 elide-lead)
                          elide-width trail-width)
                       limit))
          (setq elide-lead (1- elide-lead)))
        (add-text-properties
         elide-lead
         (- str-len elide-trail)
         (list 'display elide-str
               'rear-nonsticky '(display)
               'face face)
         result)
        result))))

(defun appkit-view--string-pixel-width (text &optional face)
  "Return graphical pixel width of TEXT rendered with FACE.

Return nil when the current buffer has no graphical display window."
  (when-let* ((window (get-buffer-window (current-buffer) t))
              ((window-live-p window))
              (frame (window-frame window))
              ((display-graphic-p frame))
              ((fboundp 'string-pixel-width)))
    (let ((measured (copy-sequence (or text ""))))
      (when (and face (> (length measured) 0))
        (add-face-text-property 0 (length measured) face t measured))
      (string-pixel-width measured (current-buffer)))))

(defun appkit-view--pixel-continuation-char-p (character)
  "Return non-nil when CHARACTER continues the preceding display cluster."
  (and character
       (or (memq (get-char-code-property character 'general-category)
                 '(Mn Mc Me))
           (<= #xFE00 character #xFE0F)
           (<= #xE0100 character #xE01EF)
           (<= #x1F3FB character #x1F3FF)
           (= character #x20E3))))

(defun appkit-view--regional-indicator-p (character)
  "Return non-nil when CHARACTER is a regional-indicator symbol."
  (and character (<= #x1F1E6 character #x1F1FF)))

(defun appkit-view--safe-elide-boundary (text boundary)
  "Move BOUNDARY left to a safe display-cluster edge in TEXT."
  (let ((position (max 0 (min (length text) boundary)))
        changed)
    (while (and (> position 0) (< position (length text))
                (progn
                  (setq changed nil)
                  (cond
                   ((appkit-view--pixel-continuation-char-p
                     (aref text position))
                    (setq position (1- position)
                          changed t))
                   ((= (aref text (1- position)) #x200D)
                    (setq position (1- position)
                          changed t))
                   ((and (appkit-view--regional-indicator-p
                          (aref text (1- position)))
                         (appkit-view--regional-indicator-p
                          (aref text position)))
                    (setq position (1- position)
                          changed t)))
                  changed)))
    position))

(defun appkit-view--elide-string-to-pixels (text pixel-limit face)
  "Return TEXT right-elided within PIXEL-LIMIT using FACE metrics."
  (let* ((ellipsis "…")
         (text-length (length text))
         (low 0)
         (high (max 0 (1- text-length)))
         (best 0))
    (while (<= low high)
      (let* ((middle (/ (+ low high) 2))
             (candidate (concat (substring text 0 middle) ellipsis))
             (width (appkit-view--string-pixel-width candidate face)))
        (if (and (numberp width) (<= width pixel-limit))
            (setq best middle
                  low (1+ middle))
          (setq high (1- middle)))))
    (setq best (appkit-view--safe-elide-boundary text best))
    (let ((result (copy-sequence text)))
      (add-text-properties
       best text-length
       (list 'display ellipsis
             'rear-nonsticky '(display)
             'face face)
       result)
      result)))

(defun appkit-view-elide-string-for-columns (str max &optional face)
  "Return STR visually elided to MAX display columns.

Graphical buffers use actual font pixels so emoji and variable-width glyphs
cannot push following aligned columns to the right.  Terminals use ordinary
column widths.  FACE supplies the font metrics used for measurement."
  (let* ((text (or str ""))
         (limit (max 0 (or max 0)))
         (pixel-width (appkit-view--string-pixel-width text face)))
    (if (numberp pixel-width)
        (let ((pixel-limit (appkit-view--chars-xwidth limit)))
          (if (<= pixel-width pixel-limit)
              text
            (appkit-view--elide-string-to-pixels text pixel-limit face)))
      (appkit-view-elide-string text limit face))))

(defun appkit-view-display-window (&optional buffer)
  "Return the canonical live window displaying BUFFER.

The selected window wins when it displays BUFFER.  Otherwise return the
widest live window displaying BUFFER across all frames.  Width-sensitive
generated content is shared by every window showing one buffer, so callers
must use this one presentation window consistently."
  (let* ((buffer (or buffer (current-buffer)))
         (selected (selected-window)))
    (if (and (window-live-p selected)
             (eq (window-buffer selected) buffer))
        selected
      (let ((best nil)
            (best-width -1))
        (dolist (window (get-buffer-window-list buffer nil t) best)
          (when (window-live-p window)
            (let ((width (window-width window 'remap)))
              (when (> width best-width)
                (setq best window
                      best-width width)))))))))

(defun appkit-view-default-line-pixel-height ()
  "Return the default-face line height for the current buffer in pixels.

`window-font-height' selects its window and would otherwise move point to
that window's point.  Row printers, including asynchronous media redraws,
must keep point at the insertion position, so this function restores
point after the query."
  (save-excursion
    (let ((window (appkit-view-display-window)))
      (max 1
           (or (and window
                    (eq (window-buffer window) (current-buffer))
                    (ignore-errors (window-font-height window 'default)))
               (ignore-errors (default-line-height))
               (frame-char-height)
               16)))))

(defun appkit-view--chars-xwidth (columns &optional window)
  "Return pixel width for COLUMNS using WINDOW metrics."
  (let* ((win (or window (appkit-view-display-window)))
         (frame (and (window-live-p win)
                     (window-frame win)))
         (buffer (and (window-live-p win) (window-buffer win)))
         (char-width
          (or (and (frame-live-p frame)
                   (display-graphic-p frame)
                   (fboundp 'string-pixel-width)
                   ;; STRING-PIXEL-WIDTH already accepts the buffer whose face
                   ;; remapping should be used.  Selecting WIN here would sync
                   ;; buffer point with its window-point during row insertion.
                   (ignore-errors
                     (string-pixel-width
                      (propertize "0" 'face 'default)
                      buffer)))
              (and (frame-live-p frame)
                   (let* ((font (ignore-errors (face-font 'default frame)))
                          (info (and font (ignore-errors (font-info font frame)))))
                     (when info
                       (let ((width (aref info 11)))
                         (if (> width 0)
                             width
                           (aref info 10))))))
              (and (frame-live-p frame)
                   (frame-char-width frame))
              (frame-char-width)
              1)))
    (* (max 0 columns)
       (if (and (frame-live-p frame) (display-graphic-p frame))
           (max 1 char-width)
         1))))

(defun appkit-view-current-column ()
  "Like `current-column', but account for prior `:align-to' spacers."
  (let* ((bol (line-beginning-position))
         (point-now (point))
         (scan point-now)
         align-column)
    (while (and (not align-column)
                (> scan bol)
                (setq scan (previous-single-char-property-change
                            scan 'display nil bol)))
      (let ((display (get-text-property scan 'display)))
        (when (and (listp display)
                   (> (length display) 2)
                   (eq (nth 0 display) 'space)
                   (eq (nth 1 display) :align-to))
          (let ((align-val (nth 2 display)))
            (setq align-column
                  (+ (if (listp align-val)
                         (ceiling (/ (or (car align-val) 0)
                                     (float (max 1 (appkit-view--chars-xwidth 1)))))
                       (or align-val 0))
                     (string-width (buffer-substring scan point-now))))))))
    (or align-column (current-column))))

(defun appkit-view-move-to-column (column)
  "Insert one absolute align-to spacer for COLUMN."
  (let* ((target (max 0 (or column 0)))
         (win (appkit-view-display-window))
         (frame (and (window-live-p win) (window-frame win))))
    (let ((align-to (if (and (frame-live-p frame)
                             (display-graphic-p frame))
                        (list (appkit-view--chars-xwidth target win))
                      target)))
      (insert (propertize " " 'display `(space :align-to ,align-to))))))

(defun appkit-view-window-fill-column (&optional window margin-columns)
  "Return telega-style usable columns for WINDOW.

The result follows face remapping/text scaling, includes window margins,
subtracts display line-number width, and reserves MARGIN-COLUMNS at the right
edge.  Return nil when WINDOW is not live."
  (let ((win (or window (appkit-view-display-window))))
    (when (window-live-p win)
      (let* ((margins (window-margins win))
             (width (+ (window-width win 'remap)
                       (or (car margins) 0)
                       (or (cdr margins) 0)))
             (line-numbers-p
              (with-current-buffer (window-buffer win)
                (bound-and-true-p display-line-numbers-mode)))
             (line-number-pixels
              (if line-numbers-p
                  (with-selected-window win
                    (line-number-display-width 'pixels))
                0))
             (char-pixels (max 1 (appkit-view--chars-xwidth 1 win)))
             (line-number-columns
              (if (and (numberp line-number-pixels)
                       (> line-number-pixels 0))
                  (ceiling (/ line-number-pixels (float char-pixels)))
                0)))
        (max 1 (- width
                  (max 0 (or margin-columns 0))
                  line-number-columns))))))

(defvar-local appkit-view--responsive-geometry-p nil
  "Non-nil when the current view observes display geometry.")

(defvar-local appkit-view--responsive-window nil
  "Canonical window used for the last responsive geometry measurement.")

(defvar-local appkit-view--responsive-width nil
  "Last responsive presentation width measured in columns.")

(defun appkit-view-responsive-width (&optional margin-columns)
  "Return the responsive presentation width less MARGIN-COLUMNS.

The width is measured in columns for `appkit-view-display-window'.  A hidden
buffer uses its current `fill-column' until a live window can be measured."
  (when appkit-view--responsive-geometry-p
    (unless appkit-view--responsive-width
      (when-let* ((window (appkit-view-display-window))
                  (width (appkit-view-window-fill-column window)))
        (setq-local appkit-view--responsive-window window
                    appkit-view--responsive-width width)))
    (when (numberp appkit-view--responsive-width)
      (max 1
           (- appkit-view--responsive-width
              (max 0 (or margin-columns 0)))))))

(cl-defun appkit-view-refresh-responsive-geometry (&key force)
  "Remeasure the current view's responsive geometry.

Request one position-preserving `geometry' synchronization when the canonical
window or its usable width changed.  FORCE requests synchronization even when
both values compare equal, for display metric changes such as text scaling.
Return the measured width in columns, or nil when no live view is displayed."
  (when appkit-view--responsive-geometry-p
    (when-let* ((view (appkit-current-view))
                ((appkit-view-live-p view))
                (window (appkit-view-display-window))
                (width (appkit-view-window-fill-column window)))
      (let ((changed
             (or (not (eq window appkit-view--responsive-window))
                 (not (equal width appkit-view--responsive-width)))))
        (setq-local appkit-view--responsive-window window
                    appkit-view--responsive-width width)
        (when (or force changed)
          (appkit-request-sync view :part 'geometry :position t))
        width))))

(defun appkit-view--on-window-geometry-change (_window)
  "Refresh responsive geometry after one displaying window changes."
  (appkit-view-refresh-responsive-geometry))

(defun appkit-view--on-display-geometry-change ()
  "Refresh responsive geometry after buffer display metrics change."
  (appkit-view-refresh-responsive-geometry :force t))

(defun appkit-view-enable-responsive-geometry (view)
  "Make VIEW observe its canonical display geometry.

Appkit owns canonical-window selection, width measurement, and coalesced
`geometry' invalidation.  The client sync function owns the resulting
presentation update."
  (unless (appkit-view-live-p view)
    (error "Cannot enable responsive geometry for a dead Appkit view"))
  (with-current-buffer (appkit-view-buffer view)
    (unless (eq view (appkit-current-view))
      (error "Cannot enable geometry for a detached Appkit view"))
    (setq-local appkit-view--responsive-geometry-p t)
    (unless appkit-view--responsive-width
      (when-let* ((window (appkit-view-display-window)))
        (setq-local appkit-view--responsive-window window))
      (setq-local appkit-view--responsive-width
                  (or (appkit-view-window-fill-column
                       appkit-view--responsive-window)
                      fill-column)))
    (add-hook 'window-size-change-functions
              #'appkit-view--on-window-geometry-change nil t)
    (add-hook 'window-selection-change-functions
              #'appkit-view--on-window-geometry-change nil t)
    (add-hook 'display-line-numbers-mode-hook
              #'appkit-view--on-display-geometry-change nil t)
    (add-hook 'text-scale-mode-hook
              #'appkit-view--on-display-geometry-change nil t))
  view)

(defun appkit-view-one-line-column-widths
    (content-width context-width-spec &optional delimiter-width)
  "Split CONTENT-WIDTH using CONTEXT-WIDTH-SPEC for the context column.

DELIMITER-WIDTH is the combined display width of the opening and closing
context delimiters, defaulting to two columns."
  (let* ((delimiter-width (max 0 (or delimiter-width 2)))
         (fixed-width (1+ delimiter-width))
         (max-context-inner (max 8 (- content-width fixed-width)))
         (context-inner-width
          (max 8
               (min max-context-inner
                    (appkit-view-canonicalize-number context-width-spec
                                                     content-width))))
         (preview-width
          (max 0 (- content-width context-inner-width fixed-width))))
    (list :context-inner-width context-inner-width
          :preview-width preview-width
          :separator-width (if (> preview-width 0) 1 0))))


(cl-defun appkit-view-insert-one-line-row
    (row &key indent width icon-slot-width context-width-spec time-slot-width)
  "Insert ROW using one-line activity-style layout.

ROW is an `appkit-view-one-line-row' object.  INDENT is left padding in spaces.
WIDTH sets the total row width.  ICON-SLOT-WIDTH reserves columns for the
icon slot; zero suppresses the slot entirely.  CONTEXT-WIDTH-SPEC controls
context width using
`appkit-view-canonicalize-number' semantics.  TIME-SLOT-WIDTH reserves a stable
right-aligned timestamp column.  PREVIEW is an
`appkit-ui-one-line-preview'.  CONTEXT-OPEN and CONTEXT-CLOSE default to
square brackets; Appkit measures their actual display widths.  A non-empty
context trail is kept inside those delimiters and aligned to their right edge;
its width is reserved before the context is elided."
  (let* ((padding (make-string (max 0 (or indent 0)) ?\s))
         (context-text
          (appkit-ui-one-line-text (appkit-view-one-line-row-context row)))
         (context-trail-text
          (appkit-ui-one-line-text
           (appkit-view-one-line-row-context-trail row)))
         (context-open
          (appkit-ui-one-line-text
           (or (appkit-view-one-line-row-context-open row) "[")))
         (context-close
          (appkit-ui-one-line-text
           (or (appkit-view-one-line-row-context-close row) "]")))
         (preview (appkit-view-one-line-row-preview row))
         (time-text
          (appkit-ui-one-line-text (appkit-view-one-line-row-time row)))
         (context-open-width (string-width context-open))
         (context-close-width (string-width context-close))
         (context-delimiter-width
          (+ context-open-width context-close-width))
         (time-width
          (max (max 0 (or time-slot-width 0))
               (if (string-empty-p time-text)
                   0
                 (max 6 (string-width time-text)))))
         (line-start (point)))
    (insert padding)
    (let* ((icon-start (appkit-view-current-column))
           (slot-width (max 0 (if (null icon-slot-width)
                                  2
                                icon-slot-width))))
      (when (> slot-width 0)
        (when-let* ((icon-inserter
                     (appkit-view-one-line-row-icon-inserter row)))
          (funcall icon-inserter))
        (appkit-view-move-to-column
         (max icon-start (1- (+ icon-start slot-width))))
        (insert " ")))
    (let* ((content-start (appkit-view-current-column))
           (time-gap (if (> time-width 0) 1 0))
           (content-width (max 20 (- (max 20 (or width 20))
                                     content-start
                                     time-width
                                     time-gap)))
           (widths (appkit-view-one-line-column-widths
                    content-width
                    (or context-width-spec '(0.45 20))
                    context-delimiter-width))
           (context-inner-width (or (plist-get widths :context-inner-width) 8))
           (preview-width (or (plist-get widths :preview-width) 0))
           (separator-width (or (plist-get widths :separator-width) 0)))
      (let ((context-start (appkit-view-current-column)))
        (insert context-open)
        (if (string-empty-p context-trail-text)
            (progn
              (insert (appkit-view-elide-string-for-columns
                       context-text context-inner-width 'default))
              (appkit-view-move-to-column
               (+ context-start context-open-width context-inner-width)))
          (let* ((raw-trail-width (string-width context-trail-text))
                 (trail-width (min context-inner-width raw-trail-width))
                 (trail-start-offset
                  (max 0 (- context-inner-width trail-width)))
                 (context-separator-width
                  (if (and (not (string-empty-p context-text))
                           (> trail-start-offset 0))
                      1
                    0))
                 (context-width
                  (max 0 (- trail-start-offset context-separator-width)))
                 (trail-text
                  (if (> raw-trail-width trail-width)
                      (appkit-view-elide-string-for-columns
                       context-trail-text trail-width
                       (appkit-view-one-line-row-context-trail-face row))
                    context-trail-text)))
            (when (> context-width 0)
              (insert (appkit-view-elide-string-for-columns
                       context-text context-width 'default)))
            (appkit-view-move-to-column
             (+ context-start context-open-width trail-start-offset))
            (let ((trail-start (point)))
              (insert trail-text)
              (when-let* ((trail-face
                           (appkit-view-one-line-row-context-trail-face row)))
                (add-text-properties trail-start (point)
                                     (list 'face trail-face))))))
        (insert context-close))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (insert
         (appkit-ui-render-one-line-preview
          preview preview-width
          :face 'shadow
          :elide-function #'appkit-view-elide-string-for-columns)))
      (when (> time-width 0)
        (let ((target-time-col (- (max 20 (or width 20)) time-width)))
          (appkit-view-move-to-column target-time-col)
          (let* ((time-start (point))
                 (time-face (or (appkit-view-one-line-row-time-face row) 'shadow))
                 (tail-face (appkit-view-one-line-row-time-tail-face row)))
            (insert (appkit-view-truncate-fill time-text time-width t))
            (if (and tail-face (> (point) time-start))
                (let ((tail-start (max time-start (1- (point)))))
                  (when (< time-start tail-start)
                    (add-text-properties time-start tail-start (list 'face time-face)))
                  (add-text-properties tail-start (point) (list 'face tail-face)))
              (add-text-properties time-start (point) (list 'face time-face))))))
      (insert "\n")
      (add-text-properties
       line-start
       (point)
       (append (or (appkit-view-one-line-row-line-properties row) '())
               (when-let* ((help-echo (appkit-view-one-line-row-help-echo row)))
		 (list 'help-echo help-echo))
               (when-let* ((mouse-face (appkit-view-one-line-row-mouse-face row)))
		 (list 'mouse-face mouse-face)))))))

(provide 'appkit-view)

;;; appkit-view.el ends here
