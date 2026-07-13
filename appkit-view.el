;;; appkit-view.el --- Shared list and row presentation geometry -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Protocol-independent list, row, elision, and window geometry used by
;; root-style views.  Position preservation and keyed EWOC reconciliation
;; remain owned by `appkit-position' and `appkit-ewoc', respectively.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
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
  context-trail
  context-trail-face
  preview
  preview-leading-length
  preview-leading-face
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

(defun appkit-view--chars-xwidth (columns &optional window)
  "Return pixel width for COLUMNS using WINDOW metrics."
  (let* ((win (or window (get-buffer-window (current-buffer) t)))
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
         (win (get-buffer-window (current-buffer) t))
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
  (let ((win (or window (get-buffer-window (current-buffer) t))))
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

(defun appkit-view-one-line-column-widths (content-width context-width-spec)
  "Split CONTENT-WIDTH using CONTEXT-WIDTH-SPEC for the context column."
  (let* ((max-context-inner (max 8 (- content-width 3)))
         (context-inner-width
          (max 8
               (min max-context-inner
                    (appkit-view-canonicalize-number context-width-spec
                                                   content-width))))
         (preview-width (max 0 (- content-width context-inner-width 3))))
    (list :context-inner-width context-inner-width
          :preview-width preview-width
          :separator-width (if (> preview-width 0) 1 0))))

(defun appkit-view--one-line-text (text)
  "Return TEXT with physical line-breaking whitespace collapsed."
  (string-trim
   (replace-regexp-in-string "[\t\n\r ]+" " " (or text "") nil t)))

(cl-defun appkit-view-insert-one-line-row
    (row &key indent width icon-slot-width context-width-spec time-slot-width)
  "Insert ROW using one-line activity-style layout.

ROW is a `appkit-view-one-line-row' object.  INDENT is left padding in spaces.
WIDTH sets the total row width.  ICON-SLOT-WIDTH reserves columns for the
icon slot.  CONTEXT-WIDTH-SPEC controls context width using
`appkit-view-canonicalize-number' semantics.  TIME-SLOT-WIDTH reserves a stable
right-aligned timestamp column.  A non-empty context trail is kept inside the
context brackets and aligned to their right edge; its width is reserved before
the context is elided."
  (let* ((padding (make-string (max 0 (or indent 0)) ?\s))
         (context-text
          (appkit-view--one-line-text (appkit-view-one-line-row-context row)))
         (context-trail-text
          (appkit-view--one-line-text
           (appkit-view-one-line-row-context-trail row)))
         (preview-text
          (appkit-view--one-line-text (appkit-view-one-line-row-preview row)))
         (time-text
          (appkit-view--one-line-text (appkit-view-one-line-row-time row)))
         (time-width
          (max (max 0 (or time-slot-width 0))
               (if (string-empty-p time-text)
                   0
                 (max 6 (string-width time-text)))))
         (line-start (point)))
    (insert padding)
    (let* ((icon-start (appkit-view-current-column))
           (slot-width (max 2 (or icon-slot-width 2)))
           (slot-target (max icon-start
                             (1- (+ icon-start slot-width)))))
      (when-let* ((icon-inserter (appkit-view-one-line-row-icon-inserter row)))
        (funcall icon-inserter))
      (appkit-view-move-to-column slot-target)
      (insert " "))
    (let* ((content-start (appkit-view-current-column))
           (time-gap (if (> time-width 0) 1 0))
           (content-width (max 20 (- (max 20 (or width 20))
                                     content-start
                                     time-width
                                     time-gap)))
           (widths (appkit-view-one-line-column-widths
                    content-width
                    (or context-width-spec '(0.45 20))))
           (context-inner-width (or (plist-get widths :context-inner-width) 8))
           (preview-width (or (plist-get widths :preview-width) 0))
           (separator-width (or (plist-get widths :separator-width) 0)))
      (let ((context-start (appkit-view-current-column)))
        (insert "[")
        (if (string-empty-p context-trail-text)
            (progn
              (insert (appkit-view-elide-string-for-columns
                       context-text context-inner-width 'default))
              (appkit-view-move-to-column
               (+ context-start 1 context-inner-width)))
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
             (+ context-start 1 trail-start-offset))
            (let ((trail-start (point)))
              (insert trail-text)
              (when-let* ((trail-face
                           (appkit-view-one-line-row-context-trail-face row)))
                (add-text-properties trail-start (point)
                                     (list 'face trail-face))))))
        (insert "]"))
      (when (> preview-width 0)
        (when (> separator-width 0)
          (insert " "))
        (let ((preview-start (point)))
          (insert (appkit-view-elide-string-for-columns
                   preview-text preview-width 'shadow))
          (add-text-properties preview-start (point) (list 'face 'shadow))
          (let ((leading-length (appkit-view-one-line-row-preview-leading-length row))
                (leading-face (appkit-view-one-line-row-preview-leading-face row)))
            (when (and (integerp leading-length)
                       (> leading-length 0)
                       leading-face)
              (add-text-properties preview-start
                                   (min (point) (+ preview-start leading-length))
                                   (list 'face leading-face))))))
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
