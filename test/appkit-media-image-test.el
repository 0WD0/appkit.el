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
                ((symbol-function 'line-pixel-height) (lambda () 10))
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
                ((symbol-function 'line-pixel-height) (lambda () 10))
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
                  ((symbol-function 'line-pixel-height) (lambda () 10))
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
                  ((symbol-function 'line-pixel-height) (lambda () 10))
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

(ert-deftest appkit-media-preview-height-preserves-aspect-and-bounds ()
  (cl-letf (((symbol-function 'appkit-media--char-pixel-width)
             (lambda () 10))
            ((symbol-function 'appkit-media--char-pixel-height)
             (lambda () 20)))
    ;; 1000x500 becomes 400x200, or ten 20-pixel text rows.
    (should (= 10
               (appkit-media-preview-height-chars
                '(1000 . 500) 400 300)))
    ;; Small images are never enlarged.
    (should (= 5
               (appkit-media-preview-height-chars
                '(100 . 100) 400 300)))))

(ert-deftest appkit-media-preview-image-owns-slice-property ()
  (let ((appkit-media-preview-max-width 400)
        (appkit-media-preview-max-height 200)
        created-properties)
    (cl-letf (((symbol-function 'appkit-media--image-file-size-pixels)
               (lambda (_file) '(800 . 400)))
              ((symbol-function 'appkit-media--char-pixel-width)
               (lambda () 10))
              ((symbol-function 'appkit-media--char-pixel-height)
               (lambda () 20))
              ((symbol-function 'appkit-media-ch-height-spec)
               (lambda (characters) (cons characters 'ch)))
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
        (should-not (plist-member created-properties :disco-nslices))
        (should-not (plist-member created-properties :telega-nslices))))))

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

(provide 'appkit-media-image-test)

;;; appkit-media-image-test.el ends here
