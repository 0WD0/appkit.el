;;; appkit-view-test.el --- Tests for appkit-view helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-view)

(ert-deftest appkit-view-render-list-spec-renders-items-and-footer ()
  (with-temp-buffer
    (appkit-view-render-list-spec
     (appkit-view-list-spec-create
      :title "Threads"
      :summary "2 items"
      :items '("one" "two")
      :item-inserter (lambda (item)
                       (insert (format "- %s\n" item)))
      :footer-lines '("footer")))
    (should (string-match-p "Threads" (buffer-string)))
    (should (string-match-p "- one" (buffer-string)))
    (should (string-match-p "footer" (buffer-string)))))

(ert-deftest appkit-view-render-list-spec-preserving-position-restores-anchor ()
  (with-temp-buffer
    (let ((items '(("a" . "first")
                   ("b" . "second"))))
      (cl-labels ((render-spec (rows)
                    (appkit-view-list-spec-create
                     :items rows
                     :item-inserter
                     (lambda (item)
                       (let ((start (point)))
                         (insert (format "%s\n" (cdr item)))
                         (add-text-properties
                          start
                          (point)
                          (list 'row-id (car item))))))))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (appkit-view-render-list-spec (render-spec items)))
        (goto-char (point-min))
        (search-forward "second")
        (beginning-of-line)
        (appkit-view-render-list-spec-preserving-position
         (render-spec '(("a" . "first updated")
                        ("b" . "second updated")))
         :anchor-property 'row-id)
        (should (equal "b" (get-text-property (point) 'row-id)))
        (should (looking-at-p "second updated"))))))

(ert-deftest appkit-view-canonicalize-number-supports-ratio-and-bounds ()
  (should (= 42 (appkit-view-canonicalize-number 42 100)))
  (should (= 35 (appkit-view-canonicalize-number 0.35 100)))
  (should (= 20 (appkit-view-canonicalize-number '(0.1 20 60) 100)))
  (should (= 60 (appkit-view-canonicalize-number '(0.8 20 60) 100)))
  (should (= 90 (appkit-view-canonicalize-number '(0.9 20) 100))))

(ert-deftest appkit-view-one-line-column-widths-follow-context-ratio ()
  (let ((widths (appkit-view-one-line-column-widths 60 '(0.45 20))))
    (should (= 27 (plist-get widths :context-inner-width)))
    (should (= 30 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width))))
  (let ((widths (appkit-view-one-line-column-widths 30 '(0.45 20))))
    (should (= 20 (plist-get widths :context-inner-width)))
    (should (= 7 (plist-get widths :preview-width)))
    (should (= 1 (plist-get widths :separator-width)))))

(ert-deftest appkit-view-one-line-row-collapses-preview-newlines ()
  (with-temp-buffer
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :context "Group\nName"
      :preview "first line\nsecond line\r\nthird"
      :time "12:34")
     :width 80
     :icon-slot-width 4
     :context-width-spec '(0.32 16 30))
    (should (= (count-lines (point-min) (point-max)) 1))
    (should (string-match-p "Group Name" (buffer-string)))
    (should (string-match-p "first line second line third"
                            (buffer-string)))))

(ert-deftest appkit-view-one-line-row-does-not-infer-hover-from-help ()
  (with-temp-buffer
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :context "Group"
      :preview "preview"
      :time "12:34"
      :help-echo "Open Group")
     :width 80)
    (should (equal "Open Group" (get-text-property (point-min) 'help-echo)))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'mouse-face nil))))

(ert-deftest appkit-view-one-line-row-supports-explicit-hover-face ()
  (with-temp-buffer
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :context "Group"
      :preview "preview"
      :time "12:34"
      :mouse-face 'highlight)
     :width 80)
    (should (eq 'highlight (get-text-property (point-min) 'mouse-face)))))

(ert-deftest appkit-view-one-line-row-keeps-context-column-across-time-formats ()
  (with-temp-buffer
    (dolist (time '("" "Thu•" "29.06.26•"))
      (appkit-view-insert-one-line-row
       (appkit-view-one-line-row-create
        :context "channel"
        :preview "preview"
        :time time)
       :width 80
       :time-slot-width 9))
    (let (targets)
      (goto-char (point-min))
      (dotimes (_ 3)
        (let ((line-end (line-end-position)))
          (should (search-forward "]" line-end t))
          (push (get-text-property (- (point) 2) 'display) targets))
        (forward-line 1))
      (should (= 1 (length (delete-dups targets)))))))

(ert-deftest appkit-view-chars-xwidth-does-not-select-display-window ()
  (let ((original-window (selected-window))
        other-window
        (buffer (generate-new-buffer " *appkit-view-width*")))
    (unwind-protect
        (progn
          (setq other-window (split-window original-window))
          (set-window-buffer other-window buffer)
          (with-current-buffer buffer
            (insert "window point\nbuffer insertion point")
            (set-window-point other-window (point-min))
            (goto-char (point-max))
            (let ((expected-point (point)))
              (cl-letf (((symbol-function 'display-graphic-p)
                         (lambda (&optional _display) t))
                        ((symbol-function 'string-pixel-width)
                         (lambda (_string &optional measured-buffer)
                           (should (eq measured-buffer buffer))
                           9)))
                (should (= 9 (appkit-view--chars-xwidth 1 other-window))))
              (should (= expected-point (point)))
              (should (eq original-window (selected-window))))))
      (when (window-live-p other-window)
        (delete-window other-window))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-view-move-to-column-inserts-align-spacer-when-needed ()
  (with-temp-buffer
    (insert "abc")
    (let ((insert-pos (point)))
      (appkit-view-move-to-column 3)
      (should (= (point) (1+ insert-pos)))
      (should (>= (appkit-view-current-column) 3))
      (let ((display-prop (get-text-property insert-pos 'display)))
        (should (consp display-prop))
        (should (eq (car display-prop) 'space))
        (should (plist-member (cdr display-prop) :align-to))))))

(ert-deftest appkit-view-move-to-column-always-inserts-absolute-spacer ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((insert-pos (point)))
      (appkit-view-move-to-column 2)
      (should (= (point) (1+ insert-pos)))
      (should (equal '(space :align-to 2)
                     (get-text-property insert-pos 'display))))))

(ert-deftest appkit-view-window-fill-column-uses-remapped-width-and-margins ()
  (let* ((win (selected-window))
         (buffer (window-buffer win))
         (margins (window-margins win))
         (expected (- (+ (window-width win 'remap)
                         (or (car margins) 0)
                         (or (cdr margins) 0))
                      1)))
    (with-current-buffer buffer
      (let ((display-line-numbers-mode nil))
        (should (= (appkit-view-window-fill-column win 1) expected))))))

(ert-deftest appkit-view-elide-string-adds-display-ellipsis ()
  (let* ((text "abcdefghijklmnopqrstuvwxyz")
         (elided (appkit-view-elide-string text 8 'shadow)))
    (should (> (length elided) 8))
    (let ((display-pos (next-single-property-change 0 'display elided)))
      (should (integerp display-pos))
      (should (equal "…"
                     (get-text-property display-pos 'display elided))))))

(ert-deftest appkit-view-elide-string-noop-when-string-fits ()
  (let ((text "short"))
    (should (equal text (appkit-view-elide-string text 12 'shadow)))))

(ert-deftest appkit-view-elide-string-for-columns-uses-pixel-width ()
  (cl-labels ((pixel-width
               (text)
               (let ((total 0))
                 (dolist (character (string-to-list text) total)
                   (setq total
                         (+ total
                            (cond
                             ((= character ?🏆) 30)
                             ((= character ?…) 10)
                             (t 10))))))))
    (cl-letf (((symbol-function 'appkit-view--string-pixel-width)
               (lambda (text &optional _face) (pixel-width text)))
              ((symbol-function 'appkit-view--chars-xwidth)
               (lambda (columns &optional _window) (* columns 10))))
      (let* ((text "🏆abcdefgh")
             (elided (appkit-view-elide-string-for-columns text 5 'shadow))
             (display-position
              (next-single-property-change 0 'display elided)))
        (should (= 2 display-position))
        (should (equal "…"
                       (get-text-property display-position 'display elided)))
        (should
         (= 50
            (pixel-width
             (concat (substring elided 0 display-position) "…"))))))))

(ert-deftest appkit-view-safe-elide-boundary-preserves-emoji-clusters ()
  (should (= 0 (appkit-view--safe-elide-boundary "👍🏽ok" 1)))
  (should (= 1 (appkit-view--safe-elide-boundary "👩‍💻ok" 2)))
  (should (= 0 (appkit-view--safe-elide-boundary "🇨🇳ok" 1))))

(ert-deftest appkit-view-insert-label-row-applies-struct-fields ()
  (with-temp-buffer
    (appkit-view-insert-label-row
     (appkit-view-label-row-create
      :label "Section"
      :prefix "[+] "
      :suffix " (4)"
      :face 'font-lock-keyword-face
      :line-properties '(row-kind section)))
    (goto-char (point-min))
    (should (equal "[+] Section (4)\n" (buffer-string)))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))

(ert-deftest appkit-view-insert-label-line-supports-prefix-icon-and-suffix ()
  (with-temp-buffer
    (appkit-view-insert-label-line
     "Guild"
     :prefix "  [-] "
     :icon-inserter (lambda ()
                      (insert "*"))
     :icon-separator " "
     :suffix " (2)"
     :line-properties '(row-kind guild)
     :help-echo "toggle")
    (goto-char (point-min))
    (should (looking-at-p "  \\[-\\] \\* Guild (2)"))
    (should (equal 'guild (get-text-property (point) 'row-kind)))
    (should (equal "toggle" (get-text-property (point) 'help-echo)))))

(ert-deftest appkit-view-insert-heading-line-applies-face-and-properties ()
  (with-temp-buffer
    (appkit-view-insert-heading-line
     "Heading"
     :face 'font-lock-keyword-face
     :line-properties '(row-kind section))
    (goto-char (point-min))
    (should (equal 'section (get-text-property (point) 'row-kind)))
    (should (equal 'font-lock-keyword-face
                   (get-text-property (point) 'face)))))
