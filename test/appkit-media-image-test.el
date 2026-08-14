;;; appkit-media-image-test.el --- Tests for shared image rendering -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-image)

(ert-deftest appkit-media-inline-image-rendering-detects-supported-types ()
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
            ((symbol-function 'image-type-available-p)
             (lambda (type) (eq type 'gif))))
    (should (appkit-media-inline-image-rendering-available-p)))
  (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil))
            ((symbol-function 'image-type-available-p) (lambda (_type) t)))
    (should-not (appkit-media-inline-image-rendering-available-p))))

(ert-deftest appkit-media-image-object-valid-p-catches-image-errors ()
  (cl-letf (((symbol-function 'image-size)
             (lambda (image &rest _)
               (if (eq image 'valid)
                   '(10 . 10)
                 (error "invalid image")))))
    (should (appkit-media-image-object-valid-p 'valid))
    (should-not (appkit-media-image-object-valid-p 'invalid))
    (should-not (appkit-media-image-object-valid-p nil))))

(ert-deftest appkit-media-image-mime-prefers-file-header ()
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
            ((symbol-function 'image-type-from-file-header)
             (lambda (_file) 'jpeg))
            ((symbol-function 'image-supported-file-p)
             (lambda (_file) 'png)))
    (should
     (equal "image/jpeg"
            (appkit-media-image-mime-type "/tmp/misnamed.png")))))

(ert-deftest appkit-media-circular-image-clips-and-center-crops-source ()
  (let (circle-call embed-call image-properties)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
              ((symbol-function 'image-type-available-p)
               (lambda (type) (eq type 'svg)))
              ((symbol-function 'appkit-media-image-mime-type)
               (lambda (_file) "image/jpeg"))
              ((symbol-function 'svg-create)
               (lambda (width height) (list :svg width height)))
              ((symbol-function 'svg-clip-path)
               (lambda (_svg &rest _properties) :clip))
              ((symbol-function 'svg-circle)
               (lambda (&rest arguments)
                 (setq circle-call arguments)))
              ((symbol-function 'svg-embed)
               (lambda (&rest arguments)
                 (setq embed-call arguments)))
              ((symbol-function 'svg-image)
               (lambda (_svg &rest properties)
                 (setq image-properties properties)
                 '(image :type svg :data "avatar")))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (should
       (equal '(image :type svg :data "avatar")
              (appkit-media-circular-image-from-file
               "/tmp/avatar.jpg" 21)))
      (should (equal circle-call '(:clip 10.5 10.5 10.5)))
      (should (equal (seq-take embed-call 4)
                     '((:svg 21 21) "/tmp/avatar.jpg" "image/jpeg" nil)))
      (should
       (equal (plist-get (nthcdr 4 embed-call) :preserveAspectRatio)
              "xMidYMid slice"))
      (should
       (equal (plist-get (nthcdr 4 embed-call) :clip-path)
              "url(#appkit-avatar-clip)"))
      (should (equal image-properties
                     '(:ascent center :width 21 :height 21))))))

(ert-deftest appkit-media-one-line-thumbnail-clips-into-fixed-box ()
  (let (rectangle-call embed-call image-properties)
    (cl-letf (((symbol-function 'file-readable-p) (lambda (_file) t))
              ((symbol-function 'image-type-available-p)
               (lambda (type) (eq type 'svg)))
              ((symbol-function 'appkit-media-image-mime-type)
               (lambda (_file) "image/png"))
              ((symbol-function 'svg-create)
               (lambda (width height) (list :svg width height)))
              ((symbol-function 'svg-clip-path)
               (lambda (_svg &rest _properties) :clip))
              ((symbol-function 'svg-rectangle)
               (lambda (&rest arguments)
                 (setq rectangle-call arguments)))
              ((symbol-function 'svg-embed)
               (lambda (&rest arguments)
                 (setq embed-call arguments)))
              ((symbol-function 'svg-image)
               (lambda (_svg &rest properties)
                 (setq image-properties properties)
                 '(image :type svg :data "thumbnail")))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t)))
      (let ((image
             (appkit-media--one-line-thumbnail-from-file
              "/tmp/source.png" 40 20)))
        (should (= 1 (plist-get (cdr image) :appkit-media-nslices)))
        (should
         (equal rectangle-call
                '(:clip 0 0 40 20 :rx 4.0 :ry 4.0)))
        (should
         (equal (seq-take embed-call 4)
                '((:svg 40 20) "/tmp/source.png" "image/png" nil)))
        (should
         (equal
          (plist-get (nthcdr 4 embed-call) :preserveAspectRatio)
          "xMidYMid slice"))
        (should
         (equal
          (plist-get (nthcdr 4 embed-call) :clip-path)
          "url(#appkit-one-line-thumbnail-clip)"))
        (should
         (equal image-properties
                '(:ascent center :width 40 :height 20 :scale 1.0)))))))

(ert-deftest appkit-media-marks-only-bounded-multi-frame-previews ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 10)
        (appkit-media-inline-animation-max-file-size 4096)
        (image '(image :type gif)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t))
              ((symbol-function 'appkit-media--file-size)
               (lambda (_file) 2048))
              ((symbol-function 'appkit-media--inline-animation-frame-data)
               (lambda (_image) '(20 . 2.5))))
      (should (eq image
                  (appkit-media--mark-inline-animation-image
                   image "/tmp/a.gif")))
      (should (appkit-media-inline-animation-image-p image))
      (should (= 2.5
                 (plist-get
                  (cdr image)
                  :appkit-media-inline-animation-duration))))))

(ert-deftest appkit-media-does-not-mark-unbounded-inline-animation ()
  (let ((appkit-media-inline-animation-enabled t)
        (appkit-media-inline-animation-max-duration 1)
        (appkit-media-inline-animation-max-file-size 1024)
        (duration-image '(image :type gif))
        (size-image '(image :type gif)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t))
              ((symbol-function 'appkit-media--inline-animation-frame-data)
               (lambda (_image) '(20 . 2.5)))
              ((symbol-function 'appkit-media--file-size)
               (lambda (file) (if (equal file "large") 2048 512))))
      (appkit-media--mark-inline-animation-image duration-image "small")
      (appkit-media--mark-inline-animation-image size-image "large")
      (should-not (appkit-media-inline-animation-image-p duration-image))
      (should-not (appkit-media-inline-animation-image-p size-image)))))

(ert-deftest appkit-media-start-inline-animation-is-one-bounded-cycle ()
  (let ((appkit-media-inline-animation-enabled t)
        (image '(image :type gif
                 :appkit-media-inline-animation t
                 :appkit-media-inline-animation-occurrence t
                 :appkit-media-inline-animation-duration 2.0))
        animated
        reset-delay)
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _args) :window))
                ((symbol-function 'image-animate)
                 (lambda (candidate index limit &optional _position)
                   (setq animated (list candidate index limit))))
                ((symbol-function 'run-at-time)
                 (lambda (delay _repeat function &rest args)
                   (setq reset-delay delay)
                   (list function args))))
        (should (appkit-media-start-inline-animation image))
        (should (equal animated (list image 0 nil)))
        (should (= 2.4 reset-delay))
        (should (plist-get
                 (cdr image) :appkit-media-inline-animation-played))
        (should-not (appkit-media-start-inline-animation image))))))

(ert-deftest appkit-media-stop-inline-animation-resets-state ()
  (let ((image '(image :type gif
                 :appkit-media-inline-animation t
                 :appkit-media-inline-animation-occurrence t
                 :appkit-media-inline-animation-played t))
        shown-frame)
    (cl-letf (((symbol-function 'image-animate-timer)
               (lambda (_image) nil))
              ((symbol-function 'image-show-frame)
               (lambda (_image frame &rest _)
                 (setq shown-frame frame))))
      (appkit-media-stop-inline-animation image)
      (should (= shown-frame 0))
      (should-not
       (plist-get (cdr image) :appkit-media-inline-animation-played))
      (should-not
       (plist-get
        (cdr image) :appkit-media-inline-animation-reset-timer))))
  (should-not (appkit-media-stop-inline-animation '(20)))
  (should-not (appkit-media-stop-inline-animation nil)))

(ert-deftest appkit-media-scroll-discovers-newly-visible-animation ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((window (selected-window))
             (image '(image :type gif :appkit-media-inline-animation t))
             position
             started)
        (dotimes (index 20)
          (insert (format "line %s\n" index)))
        (setq position (point))
        (insert (propertize "x" 'display image))
        (set-window-buffer window (current-buffer))
        (set-window-start window position)
        (cl-letf (((symbol-function 'appkit-media-start-inline-animation)
                   (lambda (candidate) (setq started candidate))))
          (appkit-media--start-window-inline-animations-after-scroll
           window position))
        (should (eq started image))))))

(ert-deftest appkit-media-image-slice-count-uses-only-appkit-property ()
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) nil)))
    (should (= 3
               (appkit-media-image-slice-count
                '(image :appkit-media-nslices 3))))
    (should (= 1
               (appkit-media-image-slice-count
                '(image :disco-nslices 7 :telega-nslices 8)))))
  (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
             (lambda (_image) t))
            ((symbol-function 'image-size)
             (lambda (&rest _) '(20 . 4.4))))
    (should (= 4 (appkit-media-image-slice-count '(image :type png))))))

(ert-deftest appkit-media-image-slices-use-target-character-height ()
  "Slice geometry never follows an unrelated selected window's line."
  (with-temp-buffer
    (let ((image '(image :type png :appkit-media-nslices 3))
          slices)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (_image) nil))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'line-pixel-height)
                 (lambda ()
                   (ert-fail
                    "slice insertion consulted the selected window")))
                ((symbol-function 'insert-image)
                 (lambda (_image alt &optional _area slice)
                   (push slice slices)
                   (insert alt))))
        (appkit-media-insert-image-slices image nil nil "preview"))
      (should (equal (nreverse slices)
                     '((0 0 1.0 10) (0 10 1.0 10) (0 20 1.0 10))))
      (should (equal (plist-get (cdr image) :appkit-media-nslices) 3)))))

(ert-deftest appkit-media-character-height-uses-target-buffer-window ()
  (with-temp-buffer
    (insert "render position")
    (goto-char 4)
    (let ((origin (point)))
      (cl-letf (((symbol-function 'appkit-media--render-window)
                 (lambda () 'target-window))
                ((symbol-function 'window-font-height)
                 (lambda (window face)
                   (should (eq window 'target-window))
                   (should (eq face 'default))
                   ;; Some Emacs window font queries move point internally.
                   (goto-char (point-max))
                   24))
                ((symbol-function 'line-pixel-height)
                 (lambda ()
                   (ert-fail "selected line height must not be consulted"))))
        (should (= 24 (appkit-media--char-pixel-height)))
        (should (= origin (point)))))))

(ert-deftest appkit-media-insert-slice-newline-has-no-line-gap ()
  (with-temp-buffer
    (insert "first")
    (appkit-media-insert-slice-newline)
    (let ((newline-position (1- (point))))
      (should (eq (char-after newline-position) ?\n))
      (should (eq (get-text-property newline-position 'line-height) t))
      (should (equal
               (get-text-property newline-position 'rear-nonsticky)
               '(line-height))))))

(ert-deftest appkit-media-insert-image-slices-prefixes-and-activates-slices ()
  (with-temp-buffer
    (let ((image '(image :type png :appkit-media-nslices 3))
          (called 0)
          slices)
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (_image) nil))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'insert-image)
                 (lambda (_image alt &optional _area slice)
                   (push slice slices)
                   (insert alt))))
        (appkit-media-insert-image-slices
         image
         (lambda () (setq called (1+ called)))
         "│ "
         "preview"
         "Open preview"))
      (should (equal (buffer-string)
                     "preview\n│ preview\n│ preview"))
      (should (equal (nreverse slices)
                     '((0 0 1.0 10) (0 10 1.0 10) (0 20 1.0 10))))
      (goto-char (point-min))
      (let ((command
             (lookup-key (get-text-property (point) 'keymap) (kbd "RET"))))
        (call-interactively command)
        (should (= called 1))
        (should (equal (get-text-property (point) 'help-echo)
                       "Open preview"))))))

(ert-deftest appkit-media-animation-insertion-keeps-descriptor-immutable ()
  (let* ((descriptor
          '(image :type gif
            :appkit-media-nslices 1
            :appkit-media-inline-animation t
            :appkit-media-inline-animation-duration 2.0))
         (snapshot (copy-tree descriptor))
         occurrences)
    (with-temp-buffer
      (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (_image) nil))
                ((symbol-function 'appkit-media--char-pixel-height)
                 (lambda () 10))
                ((symbol-function 'insert-image)
                 (lambda (image alt &optional _area _slice)
                   (push image occurrences)
                   (insert alt)))
                ((symbol-function
                  'appkit-media--install-inline-animation-discovery)
                 #'ignore))
        (appkit-media-insert-image-slices descriptor nil nil "first")
        (appkit-media-insert-image-slices descriptor nil nil "second"))
      (setq occurrences (nreverse occurrences))
      (let ((first (car occurrences))
            (second (cadr occurrences)))
        (should (equal descriptor snapshot))
        ;; Cache cleanup may call this API with the cached descriptor.  It is
        ;; deliberately a no-op unless handed an occurrence copy.
        (should-not (appkit-media-stop-inline-animation descriptor))
        (should (equal descriptor snapshot))
        (should-not (eq descriptor first))
        (should-not (eq descriptor second))
        (should-not (eq first second))
        (should (appkit-media--inline-animation-occurrence-p first))
        (should (appkit-media--inline-animation-occurrence-p second))
        (should (eq 'key
                    (hash-table-weakness
                     appkit-media--inline-animation-occurrences)))
        (should (= 2
                   (hash-table-count
                    appkit-media--inline-animation-occurrences)))
        (plist-put (cdr first)
                   :appkit-media-inline-animation-played t)
        (should-not
         (plist-get (cdr descriptor)
                    :appkit-media-inline-animation-played))
        (should-not
         (plist-get (cdr second)
                    :appkit-media-inline-animation-played))))))

(ert-deftest appkit-media-animation-occurrences-are-buffer-local ()
  (let ((descriptor
         '(image :type gif
           :appkit-media-nslices 1
           :appkit-media-inline-animation t
           :appkit-media-inline-animation-duration 2.0))
        (first-buffer (generate-new-buffer " *appkit-animation-first*"))
        (second-buffer (generate-new-buffer " *appkit-animation-second*"))
        first-occurrence
        second-occurrence
        first-registry
        second-registry)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                   (lambda (_image) nil))
                  ((symbol-function 'appkit-media--char-pixel-height)
                   (lambda () 10))
                  ((symbol-function 'insert-image)
                   (lambda (image alt &optional _area _slice)
                     (insert alt)
                     image))
                  ((symbol-function
                    'appkit-media--install-inline-animation-discovery)
                   #'ignore))
          (with-current-buffer first-buffer
            (appkit-media-insert-image-slices descriptor nil nil "first")
            (setq first-registry
                  appkit-media--inline-animation-occurrences)
            (maphash (lambda (image _present)
                       (setq first-occurrence image))
                     first-registry))
          (with-current-buffer second-buffer
            (appkit-media-insert-image-slices descriptor nil nil "second")
            (setq second-registry
                  appkit-media--inline-animation-occurrences)
            (maphash (lambda (image _present)
                       (setq second-occurrence image))
                     second-registry))
          (should-not (eq first-registry second-registry))
          (should-not (eq first-occurrence second-occurrence))
          (plist-put (cdr first-occurrence)
                     :appkit-media-inline-animation-played t)
          (should-not
           (plist-get (cdr second-occurrence)
                      :appkit-media-inline-animation-played))
          (should-not
           (plist-get (cdr descriptor)
                      :appkit-media-inline-animation-played)))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer)))))

(ert-deftest appkit-media-kill-buffer-stops-animation-occurrences ()
  (let* ((descriptor
          '(image :type gif
            :appkit-media-nslices 1
            :appkit-media-inline-animation t
            :appkit-media-inline-animation-duration 2.0))
         (buffer (generate-new-buffer " *appkit-animation-teardown*"))
         (animation-timer (run-at-time 3600 nil #'ignore))
         (reset-timer (run-at-time 3600 nil #'ignore))
         (original-cancel-timer (symbol-function 'cancel-timer))
         occurrence
         registry
         cancelled
         shown-frame)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
                   (lambda (_image) nil))
                  ((symbol-function 'appkit-media--char-pixel-height)
                   (lambda () 10))
                  ((symbol-function 'insert-image)
                   (lambda (image alt &optional _area _slice)
                     (setq occurrence image)
                     (insert alt)))
                  ((symbol-function
                    'appkit-media--install-inline-animation-discovery)
                   #'ignore)
                  ((symbol-function 'image-animate-timer)
                   (lambda (_image) animation-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer)
                     (push timer cancelled)
                     (funcall original-cancel-timer timer)))
                  ((symbol-function 'image-show-frame)
                   (lambda (_image frame &rest _)
                     (setq shown-frame frame))))
          (with-current-buffer buffer
            (appkit-media-insert-image-slices descriptor nil nil "image")
            (setq registry appkit-media--inline-animation-occurrences)
            (plist-put (cdr occurrence)
                       :appkit-media-inline-animation-played t)
            (plist-put (cdr occurrence)
                       :appkit-media-inline-animation-reset-timer
                       reset-timer))
          (kill-buffer buffer)
          (should-not (buffer-live-p buffer))
          (should (= 0 (hash-table-count registry)))
          (should (memq animation-timer cancelled))
          (should (memq reset-timer cancelled))
          (should (= 0 shown-frame))
          (should-not
           (plist-get (cdr occurrence)
                      :appkit-media-inline-animation-played))
          (should-not
           (plist-get (cdr occurrence)
                      :appkit-media-inline-animation-reset-timer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (timerp animation-timer)
        (funcall original-cancel-timer animation-timer))
      (when (timerp reset-timer)
        (funcall original-cancel-timer reset-timer)))))

(ert-deftest appkit-media-image-slice-rows-sizes-copy-to-current-line ()
  "Slice rows must not rewrite a cached `:height Nch' descriptor."
  (let ((image '(image :type png :height (3 . ch) :appkit-media-nslices 3)))
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) nil))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10)))
      (let ((rows (appkit-media-image-slice-rows image)))
        (should (= (length rows) 3))
        (should (equal (plist-get (cdr image) :height) '(3 . ch)))
        (cl-loop for row in rows
                 for index from 0
                 for display = (get-text-property 0 'display row)
                 do (should (equal (car display)
                                   (list 'slice 0 (* index 10) 1.0 10)))
                 do (should (= (plist-get (cdr (cadr display)) :height)
                               30)))))))

(ert-deftest appkit-media-insert-image-slices-sizes-copy-to-current-line ()
  "Insertion must not rewrite a cached `:height Nch' descriptor."
  (let* ((image '(image :type png :height (3 . ch) :appkit-media-nslices 3))
         inserted)
    (cl-letf (((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) nil))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 10))
              ((symbol-function 'insert-image)
               (lambda (img _alt &optional _area _slice)
                 (push (plist-get (cdr img) :height) inserted)
                 (insert "x"))))
      (appkit-media-insert-image-slices image nil nil "preview"))
    (should (equal (plist-get (cdr image) :height) '(3 . ch)))
    (should (equal inserted '(30 30 30)))))

(ert-deftest appkit-media-preview-height-preserves-aspect-and-bounds ()
  (cl-letf (((symbol-function 'appkit-media--char-pixel-width)
             (lambda () 10))
            ((symbol-function 'appkit-media--base-char-pixel-height)
             (lambda () 20)))
    ;; 1000x500 becomes 400x200, or ten 20-pixel text rows.
    (should (= 10
               (appkit-media-preview-height-chars
                '(1000 . 500) 400 300)))
    ;; Small images are never enlarged.
    (should (= 5
               (appkit-media-preview-height-chars
                '(100 . 100) 400 300)))))

(ert-deftest appkit-media-preview-height-ignores-remapped-line-height ()
  "A larger current line must not reduce the default-scale row count."
  (cl-letf (((symbol-function 'appkit-media--char-pixel-width)
             (lambda () 10))
            ((symbol-function 'appkit-media--base-char-pixel-height)
             (lambda () 20))
            ((symbol-function 'appkit-media--char-pixel-height)
             (lambda () 35)))
    (should (= 10
               (appkit-media-preview-height-chars
                '(1000 . 500) 400 300)))))

(ert-deftest appkit-media-preview-image-owns-slice-property ()
  (let ((appkit-media-preview-max-width 400)
        (appkit-media-preview-max-height 200)
        created-properties)
    (cl-letf (((symbol-function 'appkit-media--image-file-size-pixels)
               (lambda (_file) '(800 . 400)))
              ((symbol-function 'appkit-media--char-pixel-width)
               (lambda () 10))
              ((symbol-function 'appkit-media--base-char-pixel-height)
               (lambda () 20))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 20))
              ((symbol-function 'create-image)
               (lambda (_file &optional _type _data-p &rest properties)
                 (setq created-properties properties)
                 (cons 'image properties)))
              ((symbol-function 'appkit-media-image-object-valid-p)
               (lambda (_image) t))
              ((symbol-function 'appkit-media--mark-inline-animation-image)
               (lambda (image _file) image)))
      (let ((image (appkit-media-preview-image-from-file "/tmp/example.png")))
        (should (eq (car image) 'image))
        (should (= 10
                   (plist-get created-properties
                              :appkit-media-nslices)))
        (should (equal '(10 . ch)
                       (plist-get created-properties :height)))
        (should-not (plist-member created-properties :disco-nslices))
        (should-not (plist-member created-properties :telega-nslices))))))

(ert-deftest appkit-media-one-line-preview-uses-fixed-box-geometry ()
  (let (arguments)
    (cl-letf (((symbol-function 'appkit-media--base-char-pixel-height)
               (lambda () 21))
              ((symbol-function 'appkit-media--one-line-thumbnail-from-file)
               (lambda (&rest args)
                 (setq arguments args)
                 :image)))
      (should (eq :image
                  (appkit-media-one-line-preview-image-from-file
                   "/tmp/example.png" 300)))
      (should (equal '("/tmp/example.png" 300 21) arguments)))))

(ert-deftest appkit-media-image-display-string-keeps-fallback-and-display ()
  (let* ((image '(image :type png :data "bytes"))
         (rendered (appkit-media-image-display-string image "[image]")))
    (should (equal "[image]" (substring-no-properties rendered)))
    (should (eq image (get-text-property 0 'display rendered)))
    (should (equal "[image]"
                   (appkit-media-image-display-string nil "[image]")))))

(ert-deftest appkit-media-one-line-display-projects-current-line-slice ()
  (let* ((image '(image :type png :data "bytes"
                  :height (1 . ch) :appkit-media-nslices 1))
         (original (copy-tree image)))
    (cl-letf (((symbol-function 'appkit-media--line-slice-geometry)
               (lambda (_image)
                 '((image :type png :data "bytes" :height 19) 1 19))))
      (let* ((rendered
              (appkit-media-one-line-image-display-string image "[image]"))
             (display (get-text-property 0 'display rendered)))
        (should (equal "[image]" (substring-no-properties rendered)))
        (should (equal '(slice 0 0 1.0 19) (car display)))
        (should (= 19 (plist-get (cdr (cadr display)) :height)))
        (should (equal original image))
        (should
         (equal "[image]"
                (appkit-media-one-line-image-display-string
                 nil "[image]")))))))

(ert-deftest appkit-media-normalizes-known-image-leading-newline ()
  (let* ((png (concat (unibyte-string 137 80 78 71 13 10 26 10) "data"))
         (jpeg (concat (unibyte-string 255 216 255) "data"))
         (unknown "\nnot-an-image"))
    (should (equal png
                   (appkit-media-normalize-image-bytes
                    (concat (unibyte-string ?\n) png))))
    (should (equal jpeg
                   (appkit-media-normalize-image-bytes
                    (concat (unibyte-string ?\r ?\n) jpeg))))
    (should (equal unknown
                   (appkit-media-normalize-image-bytes unknown)))))

(ert-deftest appkit-media-detects-image-extensions-from-bytes ()
  (should (equal "png"
                 (appkit-media-bytes-to-extension
                  (unibyte-string 137 80 78 71 13 10 26 10) "img")))
  (should (equal "jpg"
                 (appkit-media-bytes-to-extension
                  (unibyte-string 255 216 255) "img")))
  (should (equal "gif"
                 (appkit-media-bytes-to-extension "GIF89a" "img")))
  (should (equal "webp"
                 (appkit-media-bytes-to-extension "RIFF1234WEBP" "img")))
  (should (equal "fallback"
                 (appkit-media-bytes-to-extension "unknown" "fallback"))))

(ert-deftest appkit-media-png-stream-keeps-latest-complete-frame ()
  (let* ((frame
          (base64-decode-string
           (concat
            "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwC"
            "AAAAC0lEQVR42mNk+A8AAQUBAScY42YAAAAASUVORK5CYII=")))
         (split 10))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert frame frame (substring frame 0 split))
      (should (equal frame (appkit-media-png-stream-pop-latest)))
      (should (equal (substring frame 0 split) (buffer-string)))
      (insert (substring frame split))
      (should (equal frame (appkit-media-png-stream-pop-latest)))
      (should (= (buffer-size) 0)))))

(provide 'appkit-media-image-test)

;;; appkit-media-image-test.el ends here
