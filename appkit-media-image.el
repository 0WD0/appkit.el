;;; appkit-media-image.el --- Shared inline image rendering -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-neutral image primitives for stateful chat applications.  This
;; module owns preview sizing, vertical image slices, bounded inline animation,
;; and image-byte format detection.  Backend payloads remain application
;; concerns; `appkit-media-resource' owns resource acquisition and caching.

;;; Code:

(require 'cl-lib)
(require 'image)
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
    (pcase (or (ignore-errors (image-type-from-file-header file))
               (ignore-errors (image-supported-file-p file)))
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

(defun appkit-media-insert-image-slices
    (image &optional action prefix-string fallback help-echo)
  "Insert IMAGE as vertical line slices with optional ACTION.

Insert PREFIX-STRING before every slice after the first.  FALLBACK is the
image's protocol alt text.  HELP-ECHO describes ACTION."
  (let* ((animated-p (appkit-media-inline-animation-image-p image))
         (render-image
          (if animated-p
              (appkit-media--make-inline-animation-occurrence image)
            image))
         (slice-count (appkit-media-image-slice-count render-image))
         ;; Match the geometry used to create `:height Nch' previews.  Like
         ;; telega's `telega-chars-xheight', this is a stable font metric for
         ;; the target buffer, never the height of an arbitrary selected line.
         (slice-height-pixels (appkit-media--char-pixel-height))
         (label (or fallback "[image]")))
    (dotimes (slice-index slice-count)
      (when (> slice-index 0)
        (appkit-media-insert-slice-newline)
        (when prefix-string
          (insert prefix-string)))
      (let ((slice-start (point))
            (slice (list 0
                         (* slice-index slice-height-pixels)
                         1.0
                         slice-height-pixels)))
        (insert-image render-image label nil slice)
        (appkit-media-add-action-properties
         slice-start (point) action (or help-echo "Open media"))))
    (when animated-p
      (appkit-media--register-inline-animation-occurrence render-image)
      (appkit-media--install-inline-animation-discovery))))

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

(defun appkit-media--em-height-ratio ()
  "Return the em-height ratio for the default face in the selected frame."
  (let* ((frame (selected-frame))
         (font-name (face-font 'default frame))
         (font-info (and font-name (font-info font-name frame)))
         (font-height (and (vectorp font-info) (aref font-info 3)))
         (font-size (and (vectorp font-info) (aref font-info 2))))
    (if (and (numberp font-height)
             (numberp font-size)
             (> font-size 0))
        (/ (float font-height) font-size)
      1.0)))

(defun appkit-media-ch-height-spec (characters)
  "Return an image height spec for CHARACTERS text lines."
  (let ((lines (max 1 characters)))
    (if (version< emacs-version "30.1")
        (cons (* lines (appkit-media--em-height-ratio)) 'em)
      (cons lines 'ch))))

(defun appkit-media--image-file-size-pixels (file)
  "Return FILE image size in pixels as (WIDTH . HEIGHT), or nil."
  (let ((probe (ignore-errors
                 (create-image file nil nil :ascent 'center))))
    (and (appkit-media-image-object-valid-p probe)
         (ignore-errors (image-size probe t)))))

(defun appkit-media-preview-height-chars
    (image-size max-width max-height)
  "Return preview height for IMAGE-SIZE within MAX-WIDTH and MAX-HEIGHT."
  (let* ((max-columns
          (appkit-media--pixels->chars-width max-width))
         (max-rows
          (appkit-media--pixels->chars-height max-height))
         (character-width (float (appkit-media--char-pixel-width)))
         (character-height (float (appkit-media--char-pixel-height)))
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
  "Create an inline preview image from FILE constrained by pixel limits.

MAX-WIDTH and MAX-HEIGHT default to `appkit-media-preview-max-width' and
`appkit-media-preview-max-height'."
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
  "Create a one-text-line-high preview for local FILE.

The image keeps its aspect ratio and tracks text scaling through a `ch' image
height on modern Emacs.  MAX-WIDTH guides aspect-ratio sizing, but the preview
never shrinks below one text line.  This is intended for compact composer
attachment tokens rather than timeline media cards."
  (appkit-media-preview-image-from-file
   file max-width (appkit-media--char-pixel-height)))

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
