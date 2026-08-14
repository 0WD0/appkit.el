;;; appkit-media-image.el --- Shared inline image rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-neutral image primitives for stateful chat applications.  This
;; module owns preview sizing, vertical image slices, bounded inline animation,
;; and image-byte format detection.  Cacheable previews come from
;; `appkit-media-preview-image-from-file'; display goes through
;; `appkit-media-insert-image-slices' or `appkit-media-image-slice-rows'.
;; Backend payloads remain application concerns; `appkit-media-resource'
;; owns resource acquisition and caching.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'pcase)
(require 'seq)
(require 'svg nil t)
(require 'appkit-core)
(require 'appkit-media-card)

(defgroup appkit-media nil
  "Media rendering primitives for Appkit applications."
  :group 'appkit)

(defcustom appkit-media-inline-animation-enabled t
  "When non-nil, play bounded animated images inside rendered buffers."
  :type 'boolean
  :group 'appkit-media)

(defcustom appkit-media-inline-animation-max-duration 10.0
  "Maximum duration in seconds for automatic inline image playback."
  :type 'number
  :group 'appkit-media)

(defcustom appkit-media-inline-animation-max-file-size (* 8 1024 1024)
  "Maximum source size in bytes eligible for inline image playback."
  :type 'integer
  :group 'appkit-media)

(defcustom appkit-media-preview-max-width 460
  "Default maximum pixel width for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defcustom appkit-media-preview-max-height 360
  "Default maximum pixel height for inline image previews."
  :type 'integer
  :group 'appkit-media)

(defvar-local appkit-media--inline-animation-window-starts nil
  "Last scanned window starts for inline animation discovery.")

(defvar-local appkit-media--inline-animation-occurrences nil
  "Weak registry of mutable inline animation occurrences in this buffer.

Cached image descriptors must never be registered here.  Each key is the
occurrence-specific copy created by `appkit-media-insert-image-slices'.")

(defun appkit-media--inline-animation-occurrence-p (image)
  "Return non-nil when IMAGE is a mutable inline animation occurrence."
  (and (appkit-media-inline-animation-image-p image)
       (plist-get
        (cdr image) :appkit-media-inline-animation-occurrence)))

(defun appkit-media--make-inline-animation-occurrence (image)
  "Return a mutable occurrence copy of animated image descriptor IMAGE.

Eligibility metadata is copied, while playback state belongs exclusively to
the returned image spec.  IMAGE remains suitable for immutable caches."
  (let ((occurrence (copy-tree image)))
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-occurrence t)
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-played nil)
    (plist-put (cdr occurrence)
               :appkit-media-inline-animation-reset-timer nil)
    occurrence))

(defun appkit-media--inline-animation-occurrence-registry ()
  "Return the current buffer's weak animation occurrence registry."
  (or appkit-media--inline-animation-occurrences
      (setq appkit-media--inline-animation-occurrences
            (make-hash-table :test #'eq :weakness 'key))))

(defun appkit-media--register-inline-animation-occurrence (image)
  "Track mutable animation occurrence IMAGE in the current buffer."
  (puthash image t
           (appkit-media--inline-animation-occurrence-registry))
  (add-hook 'kill-buffer-hook
            #'appkit-media--teardown-inline-animation-occurrences nil t)
  image)

(defun appkit-media--teardown-inline-animation-occurrences ()
  "Stop and forget every inline animation occurrence in this buffer."
  (when (hash-table-p appkit-media--inline-animation-occurrences)
    (maphash (lambda (image _present)
               (appkit-media-stop-inline-animation image))
             appkit-media--inline-animation-occurrences)
    (clrhash appkit-media--inline-animation-occurrences))
  (setq appkit-media--inline-animation-occurrences nil
        appkit-media--inline-animation-window-starts nil))

(defun appkit-media-stop-buffer-inline-animations (&optional buffer)
  "Stop and forget inline animation occurrences owned by BUFFER.

BUFFER defaults to the current buffer.  This is safe when no animated images
have been displayed."
  (with-current-buffer (or buffer (current-buffer))
    (appkit-media--teardown-inline-animation-occurrences)))

(defun appkit-media-inline-image-rendering-available-p ()
  "Return non-nil when the current frame can render inline images."
  (and (display-images-p)
       (or (image-type-available-p 'png)
           (image-type-available-p 'webp)
           (image-type-available-p 'jpeg)
           (image-type-available-p 'gif)
           (image-type-available-p 'imagemagick))))

(defun appkit-media-image-object-valid-p (image)
  "Return non-nil when IMAGE can be rendered by Emacs."
  (and image
       (condition-case nil
           (progn
             (image-size image t)
             t)
         (error nil))))

(defun appkit-media-image-mime-type (file)
  "Return the SVG-embeddable image MIME type of FILE, or nil.

Prefer the file header over its name so extensionless cache files and stale
filename hints cannot select the wrong decoder."
  (when (and (stringp file) (file-readable-p file))
    (pcase (ignore-errors (image-type-from-file-header file))
      ('png "image/png")
      ((or 'jpeg 'jpg) "image/jpeg")
      ('gif "image/gif")
      ('webp "image/webp")
      ('svg "image/svg+xml")
      (_ nil))))

(defun appkit-media-circular-image-from-file (file pixel-size)
  "Return FILE center-cropped to a circular PIXEL-SIZE image, or nil.

The source is embedded in an SVG and clipped geometrically; `:mask
heuristic' is intentionally insufficient because it follows source colors
instead of the avatar outline.  Unsupported image formats or displays fall
back to nil so applications can retain their ordinary square image path."
  (when (and (stringp file)
             (file-readable-p file)
             (numberp pixel-size)
             (> pixel-size 0)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-circle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (condition-case nil
        (when-let* ((mime-type (appkit-media-image-mime-type file)))
          (let* ((size (max 1 (round pixel-size)))
                 (radius (/ size 2.0))
                 (svg (svg-create size size))
                 (clip (svg-clip-path svg :id "appkit-avatar-clip")))
            (svg-circle clip radius radius radius)
            (svg-embed
             svg file mime-type nil
             :x 0 :y 0 :width size :height size
             :preserveAspectRatio "xMidYMid slice"
             :clip-path "url(#appkit-avatar-clip)")
            (let ((image
                   (svg-image
                    svg :ascent 'center :width size :height size)))
              (and (appkit-media-image-object-valid-p image) image))))
      (error nil))))

(defun appkit-media--one-line-thumbnail-from-file
    (file pixel-width pixel-height)
  "Return FILE center-cropped to a one-line thumbnail.

PIXEL-WIDTH and PIXEL-HEIGHT are the exact output box.  The source is
embedded in an SVG so a large source image cannot escape the compact
container's geometry."
  (when (and (stringp file)
             (file-readable-p file)
             (numberp pixel-width)
             (> pixel-width 0)
             (numberp pixel-height)
             (> pixel-height 0)
             (image-type-available-p 'svg)
             (fboundp 'svg-create)
             (fboundp 'svg-clip-path)
             (fboundp 'svg-rectangle)
             (fboundp 'svg-embed)
             (fboundp 'svg-image))
    (condition-case nil
        (when-let* ((mime-type (appkit-media-image-mime-type file)))
          (let* ((width (max 1 (round pixel-width)))
                 (height (max 1 (round pixel-height)))
                 (radius (max 1 (/ (min width height) 5.0)))
                 (svg (svg-create width height))
                 (clip
                  (svg-clip-path
                   svg :id "appkit-one-line-thumbnail-clip")))
            (svg-rectangle clip 0 0 width height :rx radius :ry radius)
            (svg-embed
             svg file mime-type nil
             :x 0 :y 0 :width width :height height
             :preserveAspectRatio "xMidYMid slice"
             :clip-path "url(#appkit-one-line-thumbnail-clip)")
            (let ((image
                   (svg-image
                    svg :ascent 'center :width width :height height
                    :scale 1.0)))
              (when (appkit-media-image-object-valid-p image)
                (plist-put (cdr image) :appkit-media-nslices 1)
                image))))
      (error nil))))

(defun appkit-media--file-size (file)
  "Return FILE size in bytes, or nil when it is unavailable."
  (when (and (stringp file) (file-exists-p file))
    (file-attribute-size (file-attributes file))))

(defun appkit-media--inline-animation-frame-data (image)
  "Return (FRAME-COUNT . DURATION) for multi-frame IMAGE, or nil."
  (when-let* ((multi (ignore-errors (image-multi-frame-p image))))
    (let* ((count (car multi))
           (delay (or (and (numberp (cdr multi)) (cdr multi))
                      image-default-frame-delay))
           (duration (* count delay)))
      (and (> count 1)
           (> duration 0)
           (cons count duration)))))

(defun appkit-media--mark-inline-animation-image (image file)
  "Mark bounded multi-frame IMAGE from FILE for inline playback."
  (when (and appkit-media-inline-animation-enabled
             (appkit-media-image-object-valid-p image)
             (let ((size (appkit-media--file-size file)))
               (and size
                    (<= size
                        appkit-media-inline-animation-max-file-size))))
    (when-let* ((frame-data
                 (appkit-media--inline-animation-frame-data image))
                (duration (cdr frame-data))
                ((<= duration
                     appkit-media-inline-animation-max-duration)))
      (plist-put (cdr image) :appkit-media-inline-animation t)
      (plist-put (cdr image)
                 :appkit-media-inline-animation-duration
                 duration)))
  image)

(defun appkit-media-inline-animation-image-p (image)
  "Return non-nil when IMAGE is marked for bounded inline animation."
  (and (eq (car-safe image) 'image)
       (plist-get (cdr image) :appkit-media-inline-animation)))

(defun appkit-media-image-display-string (image fallback)
  "Return FALLBACK displayed as IMAGE, or FALLBACK when IMAGE is nil.

Animated cache descriptors are copied into buffer-owned playback occurrences
before display, so callers do not need to invoke Appkit's private animation
registry functions."
  (if (not image)
      fallback
    (let ((render-image
           (if (appkit-media-inline-animation-image-p image)
               (appkit-media--make-inline-animation-occurrence image)
             image)))
      (when (not (eq render-image image))
        (appkit-media--register-inline-animation-occurrence render-image)
        (appkit-media--install-inline-animation-discovery))
      (propertize (or fallback " ")
                  'display render-image
                  'rear-nonsticky '(display)))))

(defun appkit-media-one-line-image-display-string (image fallback)
  "Return FALLBACK displayed as a current-line thumbnail for IMAGE.

IMAGE should come from `appkit-media-one-line-preview-image-from-file'.
The cached descriptor is copied and sized to the current line in pixels;
the original is not mutated.  FALLBACK remains the underlying terminal
text.  Return FALLBACK unchanged when IMAGE is nil."
  (if (not image)
      fallback
    (let* ((animated-p (appkit-media-inline-animation-image-p image))
           (source-image
            (if animated-p
                (appkit-media--make-inline-animation-occurrence image)
              image)))
      (pcase-let ((`(,render-image ,_slice-count ,slice-height)
                   (appkit-media--line-slice-geometry source-image)))
        (when animated-p
          (appkit-media--register-inline-animation-occurrence render-image)
          (appkit-media--install-inline-animation-discovery))
        (propertize
         (or fallback " ")
         'display
         (list (list 'slice 0 0 1.0 slice-height)
               render-image)
         'rear-nonsticky '(display))))))

(defun appkit-media-stop-inline-animation (image)
  "Stop bounded inline playback for IMAGE and reset it to frame zero."
  (when (appkit-media--inline-animation-occurrence-p image)
    (when-let* ((timer (image-animate-timer image)))
      (cancel-timer timer))
    (when-let* ((timer
                 (plist-get
                  (cdr image)
                  :appkit-media-inline-animation-reset-timer)))
      (when (timerp timer)
        (cancel-timer timer)))
    (ignore-errors (image-show-frame image 0 t))
    (plist-put (cdr image)
               :appkit-media-inline-animation-reset-timer nil)
    (plist-put (cdr image) :appkit-media-inline-animation-played nil)))

(defun appkit-media--finish-inline-animation (image)
  "Finish one inline playback cycle for IMAGE."
  (when (appkit-media--inline-animation-occurrence-p image)
    (when-let* ((timer (image-animate-timer image)))
      (cancel-timer timer))
    (ignore-errors (image-show-frame image 0 t))
    (plist-put (cdr image)
               :appkit-media-inline-animation-reset-timer nil)
    (plist-put (cdr image) :appkit-media-inline-animation-played nil)))

(defun appkit-media-start-inline-animation (image)
  "Play marked IMAGE once when its current buffer is visible."
  (when (and appkit-media-inline-animation-enabled
             (appkit-media--inline-animation-occurrence-p image)
             (not (plist-get
                   (cdr image) :appkit-media-inline-animation-played))
             (get-buffer-window (current-buffer) t))
    (let ((duration
           (plist-get
            (cdr image) :appkit-media-inline-animation-duration)))
      (when (and (numberp duration) (> duration 0))
        (plist-put (cdr image) :appkit-media-inline-animation-played t)
        (image-animate image 0 nil)
        (plist-put
         (cdr image)
         :appkit-media-inline-animation-reset-timer
         (run-at-time (+ duration 0.4) nil
                      #'appkit-media--finish-inline-animation image))
        t))))

(defun appkit-media--display-image-spec (display)
  "Return an image spec nested in DISPLAY, including sliced displays."
  (cond
   ((and (consp display) (eq (car display) 'image)) display)
   ((and (consp display)
         (consp (car display))
         (eq (caar display) 'slice)
         (consp (cadr display))
         (eq (car (cadr display)) 'image))
    (cadr display))))

(defun appkit-media--start-window-inline-animations
    (window &optional start end)
  "Start animations visible in WINDOW between START and END."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((position (or start (window-start window)))
          (end (or end (window-end window t) (point-max))))
      (while (< position end)
        (when-let* ((image
                     (appkit-media--display-image-spec
                      (get-text-property position 'display))))
          (appkit-media-start-inline-animation image))
        (setq position
              (or (next-single-property-change
                   position 'display nil end)
                  end)))
      (setf (alist-get window
                       appkit-media--inline-animation-window-starts
                       nil nil #'eq)
            (window-start window)))))

(defun appkit-media--start-window-inline-animations-after-scroll
    (window display-start)
  "Start animations after WINDOW has scrolled to DISPLAY-START."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((display-end
           (save-excursion
             (goto-char display-start)
             (vertical-motion (1+ (window-body-height window)) window)
             (point))))
      (appkit-media--start-window-inline-animations
       window display-start display-end))))

(defun appkit-media--start-buffer-inline-animations-after-command ()
  "Discover animations when a displayed window moves after a command."
  (setq appkit-media--inline-animation-window-starts
        (seq-filter (lambda (entry) (window-live-p (car entry)))
                    appkit-media--inline-animation-window-starts))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (unless (equal
             (alist-get window
                        appkit-media--inline-animation-window-starts
                        nil nil #'eq)
             (window-start window))
      (appkit-media--start-window-inline-animations window))))

(defun appkit-media-image-slice-count (image)
  "Return the line count used to render IMAGE as vertical slices."
  (let* ((properties (cdr-safe image))
         (explicit-slices
          (plist-get properties :appkit-media-nslices))
         (size (and (appkit-media-image-object-valid-p image)
                    (ignore-errors
                      (image-size image nil (selected-frame)))))
         (height (and (consp size) (cdr size))))
    (max 1
         (cond
          ((and (integerp explicit-slices) (> explicit-slices 0))
           explicit-slices)
          ((numberp height) (round height))
          (t 1)))))

(defun appkit-media-insert-slice-newline ()
  "Insert a newline between image slices without an extra line gap."
  (let ((newline-start (point)))
    (insert "\n")
    (add-text-properties newline-start (point)
                         '(line-height t
                           rear-nonsticky (line-height)))))

(defun appkit-media--install-inline-animation-discovery ()
  "Install buffer-local hooks that discover visible inline animations."
  (add-hook 'window-state-change-functions
            #'appkit-media--start-window-inline-animations nil t)
  (add-hook 'window-scroll-functions
            #'appkit-media--start-window-inline-animations-after-scroll nil t)
  (add-hook 'post-command-hook
            #'appkit-media--start-buffer-inline-animations-after-command nil t)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (appkit-media--start-window-inline-animations window)))

(defun appkit-media--image-with-pixel-height (image pixel-height)
  "Return a copy of IMAGE whose `:height' is PIXEL-HEIGHT.

The original IMAGE is not mutated, so cached `:height Nch' descriptors
stay reusable after `text-scale-mode'."
  (let ((properties (copy-sequence (cdr-safe image))))
    (cons 'image (plist-put properties :height pixel-height))))

(defun appkit-media--line-slice-geometry (image)
  "Return (RENDER-IMAGE SLICE-COUNT SLICE-HEIGHT) for displaying IMAGE.

RENDER-IMAGE is a copy sized to SLICE-COUNT current text lines.
IMAGE is not mutated."
  (let* ((slice-count (appkit-media-image-slice-count image))
         (slice-height (appkit-media--char-pixel-height)))
    (list (appkit-media--image-with-pixel-height
           image (* slice-count slice-height))
          slice-count
          slice-height)))

(defun appkit-media-image-slice-rows (image)
  "Return IMAGE as one current-line display string per slice.

This is the non-inserting counterpart of
`appkit-media-insert-image-slices'.  Each string is a single space
whose `display' is one row of a copy sized to N current text lines.
IMAGE is not mutated.  After `text-scale-mode', rebuild and call this
again so the rows follow the new line height."
  (pcase-let ((`(,render-image ,slice-count ,slice-height)
               (appkit-media--line-slice-geometry image)))
    (cl-loop for slice-index below slice-count
             collect
             (propertize
              " "
              'display (list (list 'slice
                                   0
                                   (* slice-index slice-height)
                                   1.0
                                   slice-height)
                             render-image)
              'rear-nonsticky '(display)))))

(defun appkit-media-insert-image-slices
    (image &optional action prefix-string fallback help-echo)
  "Insert IMAGE as vertical line slices with optional ACTION.

IMAGE should come from `appkit-media-preview-image-from-file'.  This
function does not mutate IMAGE.  It copies IMAGE, sizes the copy to N
times the current line, and inserts one slice per line.  After
`text-scale-mode', rebuild the buffer and call this again.

Insert PREFIX-STRING before every slice after the first.  FALLBACK is
the image's protocol alt text.  HELP-ECHO describes ACTION."
  (let* ((animated-p (appkit-media-inline-animation-image-p image))
         (source-image
          (if animated-p
              (appkit-media--make-inline-animation-occurrence image)
            image)))
    (pcase-let
        ((`(,render-image ,slice-count ,slice-height)
          (appkit-media--line-slice-geometry source-image))
         (label (or fallback "[image]")))
      (dotimes (slice-index slice-count)
        (when (> slice-index 0)
          (appkit-media-insert-slice-newline)
          (when prefix-string
            (insert prefix-string)))
        (let ((slice-start (point))
              (slice (list 0
                           (* slice-index slice-height)
                           1.0
                           slice-height)))
          (insert-image render-image label nil slice)
          (appkit-media-add-action-properties
           slice-start (point) action (or help-echo "Open media"))))
      (when animated-p
        (appkit-media--register-inline-animation-occurrence render-image)
        (appkit-media--install-inline-animation-discovery)))))

(defun appkit-media--char-pixel-width ()
  "Return the default character width in pixels for the current frame."
  (max 1 (frame-char-width)))

(defun appkit-media--render-window ()
  "Return the preferred live window displaying the current buffer."
  (let* ((buffer (current-buffer))
         (selected (selected-window)))
    (cond
     ((and (window-live-p selected)
           (eq (window-buffer selected) buffer))
      selected)
     ((cl-find-if #'window-live-p
                  (get-buffer-window-list buffer nil (selected-frame))))
     ((cl-find-if #'window-live-p
                  (get-buffer-window-list buffer nil 'visible)))
     (t
      (cl-find-if #'window-live-p
                  (get-buffer-window-list buffer nil t))))))

(defun appkit-media--char-pixel-height ()
  "Return the default character height for the current buffer in pixels.

Use a window displaying the target buffer when possible.  This deliberately
does not use `line-pixel-height': asynchronous rendering can run while the
selected window displays an unrelated full-size image, making that function
return the source image height instead of a character height."
  (save-excursion
    (let ((window (appkit-media--render-window)))
      (max 1
           (or (and window
                    (ignore-errors
                      (window-font-height window 'default)))
               (ignore-errors (default-line-height))
               (frame-char-height)
               16)))))

(defun appkit-media--base-char-pixel-height ()
  "Return the unscaled default-face character height in pixels."
  (max 1 (or (frame-char-height) 16)))

(defun appkit-media--pixels->chars-width (pixels)
  "Convert PIXELS to character columns using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (appkit-media--char-pixel-width))))))

(defun appkit-media--pixels->chars-height (pixels)
  "Convert PIXELS to text lines using current frame metrics."
  (max 1
       (ceiling (/ (float (max 1 pixels))
                   (float (appkit-media--char-pixel-height))))))


(defun appkit-media-ch-height-spec (characters)
  "Return the `(HEIGHT . ch)' spec for CHARACTERS text lines."
  (cons (max 1 characters) 'ch))

(defun appkit-media--image-file-size-pixels (file)
  "Return FILE image size in pixels as (WIDTH . HEIGHT), or nil."
  (let ((probe (ignore-errors
                 (create-image file nil nil :ascent 'center))))
    (and (appkit-media-image-object-valid-p probe)
         (ignore-errors (image-size probe t)))))

(defun appkit-media-preview-height-chars
    (image-size max-width max-height)
  "Return preview row count for IMAGE-SIZE within MAX-WIDTH and MAX-HEIGHT.

MAX-WIDTH and MAX-HEIGHT are a default-scale pixel budget.  Row count
uses unscaled face metrics so `text-scale-mode' does not shrink N on a
client that rebuilds the `:height Nch' descriptor."
  (let* ((character-width (float (appkit-media--char-pixel-width)))
         (character-height (float (appkit-media--base-char-pixel-height)))
         (max-columns
          (max 1 (ceiling (/ (float (max 1 max-width)) character-width))))
         (max-rows
          (max 1 (ceiling (/ (float (max 1 max-height)) character-height))))
         (image-width (max 1.0 (float (car image-size))))
         (image-height (max 1.0 (float (cdr image-size))))
         (image-columns (/ image-width character-width))
         (image-rows (/ image-height character-height))
         (scale (min 1.0
                     (/ (float max-columns) (max 1.0 image-columns))
                     (/ (float max-rows) (max 1.0 image-rows)))))
    (max 1
         (min max-rows
              (round (* image-rows scale))))))

(defun appkit-media-preview-image-from-file
    (file &optional max-width max-height)
  "Create a cacheable inline preview from FILE.

MAX-WIDTH and MAX-HEIGHT are a default-scale pixel budget and default
to `appkit-media-preview-max-width' and `appkit-media-preview-max-height'.
The returned image uses `:height (N . ch)' and `:appkit-media-nslices
N'.  N uses unscaled face metrics so `text-scale-mode' does not change
the cached descriptor.  Do not mutate the returned image.

Display is a separate step: call `appkit-media-insert-image-slices' or
`appkit-media-image-slice-rows' so a copy is sized to N current lines."
  (let* ((safe-max-width
          (max 1 (if (numberp max-width)
                     max-width
                   appkit-media-preview-max-width)))
         (safe-max-height
          (max 1 (if (numberp max-height)
                     max-height
                   appkit-media-preview-max-height)))
         (file-size (appkit-media--image-file-size-pixels file))
         (target-height-characters
          (if (consp file-size)
              (appkit-media-preview-height-chars
               file-size safe-max-width safe-max-height)
            (appkit-media--pixels->chars-height safe-max-height)))
         (height-spec
          (appkit-media-ch-height-spec target-height-characters))
         (image
          (ignore-errors
            (create-image file nil nil
                          :height height-spec
                          :appkit-media-nslices target-height-characters
                          :scale 1.0
                          :ascent 'center))))
    (unless (appkit-media-image-object-valid-p image)
      (when (image-type-available-p 'imagemagick)
        (setq image
              (ignore-errors
                (create-image file 'imagemagick nil
                              :height height-spec
                              :appkit-media-nslices
                              target-height-characters
                              :scale 1.0
                              :ascent 'center)))))
    (and (appkit-media-image-object-valid-p image)
         (appkit-media--mark-inline-animation-image image file))))

(defun appkit-media-one-line-preview-image-from-file (file &optional max-width)
  "Create a bounded one-row thumbnail for local FILE.

MAX-WIDTH is the rendered pixel width and defaults to
`appkit-media-preview-max-width'.  A graphical display uses a fixed-size,
center-cropped SVG thumbnail like Telega's one-line previews.  Displays
without SVG support retain the ordinary image decoder with explicit
one-line height and maximum-width constraints.

Display with `appkit-media-one-line-image-display-string'."
  (let* ((safe-max-width
          (max 1 (if (numberp max-width)
                     max-width
                   appkit-media-preview-max-width)))
         (line-height (appkit-media--base-char-pixel-height)))
    (or (appkit-media--one-line-thumbnail-from-file
         file safe-max-width line-height)
        (let ((image
               (appkit-media-preview-image-from-file
                file safe-max-width line-height)))
          (when (appkit-media-image-object-valid-p image)
            (plist-put (cdr image) :max-width safe-max-width)
            image)))))

(defun appkit-media--bytes-prefix-p (bytes offset prefix-bytes)
  "Return non-nil when BYTES at OFFSET starts with PREFIX-BYTES."
  (and (stringp bytes)
       (integerp offset)
       (>= offset 0)
       (<= (+ offset (length prefix-bytes)) (length bytes))
       (cl-loop for byte in prefix-bytes
                for index from 0
                always (= (aref bytes (+ offset index)) byte))))

(defun appkit-media--webp-bytes-p-at (bytes offset)
  "Return non-nil when BYTES has a WebP signature at OFFSET."
  (and (appkit-media--bytes-prefix-p bytes offset '(82 73 70 70))
       (appkit-media--bytes-prefix-p bytes (+ offset 8) '(87 69 66 80))))

(defun appkit-media--known-image-signature-at-p (bytes offset)
  "Return non-nil when BYTES has a known image signature at OFFSET."
  (and (stringp bytes)
       (integerp offset)
       (>= offset 0)
       (<= offset (length bytes))
       (or (appkit-media--bytes-prefix-p
            bytes offset '(137 80 78 71 13 10 26 10))
           (appkit-media--bytes-prefix-p bytes offset '(255 216 255))
           (appkit-media--bytes-prefix-p bytes offset '(71 73 70 56 55 97))
           (appkit-media--bytes-prefix-p bytes offset '(71 73 70 56 57 97))
           (appkit-media--webp-bytes-p-at bytes offset))))

(defun appkit-media-normalize-image-bytes (bytes)
  "Strip a stray leading newline before recognized image BYTES."
  (cond
   ((and (stringp bytes)
         (>= (length bytes) 2)
         (eq (aref bytes 0) ?\n)
         (appkit-media--known-image-signature-at-p bytes 1))
    (substring bytes 1))
   ((and (stringp bytes)
         (>= (length bytes) 3)
         (eq (aref bytes 0) ?\r)
         (eq (aref bytes 1) ?\n)
         (appkit-media--known-image-signature-at-p bytes 2))
    (substring bytes 2))
   (t bytes)))

(defun appkit-media--buffer-uint32-be (position)
  "Read one unsigned big-endian 32-bit value at buffer POSITION."
  (+ (ash (char-after position) 24)
     (ash (char-after (1+ position)) 16)
     (ash (char-after (+ position 2)) 8)
     (char-after (+ position 3))))

(defun appkit-media--png-frame-end (start)
  "Return end position of a complete PNG frame at START, or nil."
  (let ((position (+ start 8))
        end)
    (when (and (<= (+ start 8) (point-max))
               (cl-loop for byte in '(137 80 78 71 13 10 26 10)
                        for offset from 0
                        always (= (char-after (+ start offset)) byte)))
      (catch 'done
        (while (<= (+ position 12) (point-max))
          (let* ((length (appkit-media--buffer-uint32-be position))
                 (chunk-end (+ position 12 length)))
            (when (> chunk-end (point-max))
              (throw 'done nil))
            (when (and (= (char-after (+ position 4)) ?I)
                       (= (char-after (+ position 5)) ?E)
                       (= (char-after (+ position 6)) ?N)
                       (= (char-after (+ position 7)) ?D))
              (setq end chunk-end)
              (throw 'done end))
            (setq position chunk-end))))
      end)))

(defun appkit-media-png-stream-pop-latest (&optional buffer)
  "Remove complete PNG frames from BUFFER and return only the latest.

BUFFER defaults to the current buffer and must be unibyte.  Any incomplete
trailing frame remains buffered for the next process-filter chunk.  Signal an
error when complete input does not begin with a PNG signature."
  (with-current-buffer (or buffer (current-buffer))
    (when enable-multibyte-characters
      (error "Appkit PNG stream buffer must be unibyte"))
    (let ((position (point-min))
          latest-start
          latest-end
          frame-end)
      (while (setq frame-end (appkit-media--png-frame-end position))
        (setq latest-start position
              latest-end frame-end
              position frame-end))
      (when (and (< position (point-max))
                 (>= (- (point-max) position) 8)
                 (not (appkit-media--png-frame-end position)))
        (unless
            (cl-loop for byte in '(137 80 78 71 13 10 26 10)
                     for offset from 0
                     always (= (char-after (+ position offset)) byte))
          (error "Appkit PNG stream contains invalid bytes")))
      (when latest-start
        (let ((latest
               (buffer-substring-no-properties latest-start latest-end)))
          (delete-region (point-min) position)
          latest)))))


(defun appkit-media-bytes-to-extension (bytes fallback-extension)
  "Infer an image extension from BYTES, else return FALLBACK-EXTENSION."
  (cond
   ((appkit-media--bytes-prefix-p bytes 0 '(137 80 78 71 13 10 26 10))
    "png")
   ((appkit-media--bytes-prefix-p bytes 0 '(255 216 255))
    "jpg")
   ((or (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 55 97))
        (appkit-media--bytes-prefix-p bytes 0 '(71 73 70 56 57 97)))
    "gif")
   ((appkit-media--webp-bytes-p-at bytes 0) "webp")
   (t fallback-extension)))

(provide 'appkit-media-image)

;;; appkit-media-image.el ends here
