;;; appkit-media-resource-test.el --- Tests for media resources -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-media-resource)

(appkit-define-app-kind appkit-media-test)

(ert-deftest appkit-media-resource-classifies-canonical-resources ()
  (should (equal "cat%20photo.PNG"
                 (appkit-media-url-filename
                  "https://cdn.example/media/cat%20photo.PNG?token=1#preview")))
  (should (equal "explicit-name.bin"
                 (appkit-media-resource-name
                  '((name . "explicit-name.bin")
                    (url . "https://cdn.example/fallback.png")))))
  (should (equal "local.webp"
                 (appkit-media-resource-name
                  '((file . "/tmp/local.webp")))))
  (should (eq 'video
              (appkit-media-resource-kind
               '((mime-type . "VIDEO/MP4")
                 (name . "opaque.bin")))))
  (should (eq 'image
              (appkit-media-resource-kind
               '((name . "photo.JPEG?download=1")))))
  (should (eq 'video
              (appkit-media-resource-kind
               '((url . "https://cdn.example/clip.webm#fragment")))))
  (should (eq 'file
              (appkit-media-resource-kind
               '((name . "report.pdf")))))
  (should-error
   (appkit-media-resource-kind '((name . "photo.png")) 'custom)))

(ert-deftest appkit-media-resource-construction-is-strict-and-canonical ()
  (should
   (equal '((file . "/tmp/image.png")
            (name . "image.png")
            (mime-type . "image/png"))
          (appkit-media-resource-create
           :file "/tmp/image.png"
           :name "image.png"
           :mime-type "image/png")))
  (let* ((resource '((url . "https://example.invalid/image.png")))
         (normalized (appkit-media-resource-normalize resource)))
    (should (equal resource normalized))
    (should-not (eq resource normalized)))
  (dolist (legacy '(((filename . "legacy.png"))
                    ((file_name . "legacy.png"))
                    ((content_type . "image/png"))))
    (should-error (appkit-media-resource-normalize legacy)))
  (should-error (appkit-media-resource-normalize '((name . ""))))
  (should-error
   (appkit-media-resource-normalize
    '((name . "first") (name . "second")))))

(ert-deftest appkit-media-resource-sanitizes-untrusted-filenames ()
  (should (equal "folder_file_name.txt"
                 (appkit-media-sanitize-filename
                  (concat "folder/file" (string ?\n) "name.txt"))))
  (should (equal "media.bin" (appkit-media-sanitize-filename nil)))
  (should (appkit-media-image-file-name-p "photo.WeBp?token=1"))
  (should (appkit-media-video-file-name-p "clip.MP4#play"))
  (should-not (appkit-media-image-file-name-p "archive.png.zip")))

(ert-deftest appkit-media-local-resource-copy-completes-synchronously ()
  (let* ((directory (make-temp-file "appkit-media-copy" t))
         (source (expand-file-name "source.bin" directory))
         (target (expand-file-name "nested/target.bin" directory))
         success-value
         failure)
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "local media bytes"))
          (appkit-media-copy-or-download-resource-async
           `((file . ,source))
           target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (should (equal target success-value))
          (should-not failure)
          (should (file-exists-p target))
          (should (equal "local media bytes"
                         (with-temp-buffer
                           (insert-file-contents-literally target)
                           (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-open-local-resource-opens-and-reports-canonical-copy ()
  (let* ((directory (make-temp-file "appkit-media-open-local" t))
         (file (expand-file-name "report.txt" directory))
         opened
         updated
         (resource `((file . ,file) (name . "report.txt"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "report"))
          (let ((appkit-media-animate-gifs nil)
                (appkit-media-open-file-function
                 (lambda (path)
                   (setq opened path)
                   :opened)))
            (should
             (eq :opened
                 (appkit-media-open-resource
                  resource
                  :kind 'file
                  :cache-update-function
                  (lambda (local-resource)
                    (setq updated local-resource))))))
          (should (equal file opened))
          (should (equal file (alist-get 'file updated)))
          (should (equal "report.txt" (alist-get 'name updated)))
          (should (equal resource `((file . ,file) (name . "report.txt")))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-resource-dispatches-through-shared-scheduler ()
  (let* ((directory (make-temp-file "appkit-media-remote" t))
         (target (expand-file-name "nested/report.pdf" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         success-value
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory target) t)
          (with-temp-file target (insert "old contents"))
          (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     :remote-process))
                  ((symbol-function 'url-copy-file)
                   (lambda (&rest _)
                     (ert-fail
                      "generic remote files must use asynchronous plz"))))
          (should
           (appkit-media-transfer-p
            (appkit-media-copy-or-download-resource-async
             '((url . "https://example.invalid/report.pdf"))
             target
             (lambda (file) (setq success-value file))
             (lambda (reason) (setq failure reason)))))
          (should (eq 'get (nth 0 plz-arguments)))
          (should (equal "https://example.invalid/report.pdf"
                         (nth 1 plz-arguments)))
          (let ((properties (nthcdr 2 plz-arguments)))
            (let ((part (cadr (plist-get properties :as))))
              (should (string-suffix-p "/download.part" part))
              (should (file-directory-p (file-name-directory part)))
              (should-not (file-exists-p part))
              (should (equal "old contents"
                             (with-temp-buffer
                               (insert-file-contents-literally target)
                               (buffer-string))))
              (with-temp-file part (insert "new remote contents"))
              (funcall (plist-get properties :then) part)
              (should-not (file-exists-p part))
              (should-not (file-directory-p (file-name-directory part))))
            (should (eq t (plist-get properties :noquery)))
            (should (functionp (plist-get properties :then)))
            (should (functionp (plist-get properties :else)))
            (should (equal "new remote contents"
                           (with-temp-buffer
                             (insert-file-contents-literally target)
                             (buffer-string)))))
          (should (equal target success-value))
          (should-not failure)
          (should (= 0 appkit-media--active-transfer-count))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-deduplicates-and-broadcasts-success ()
  (let* ((directory (make-temp-file "appkit-media-dedupe" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         (queue-count 0)
         handles
         successes
         failures)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (cl-incf queue-count)
                     (setq plz-arguments arguments)
                     :remote-process)))
          (dolist (id '(first second))
            (push
             (appkit-media-copy-or-download-resource-async
              '((url . "https://example.invalid/shared.bin")) target
              (lambda (file) (setq successes
                                   (append successes (list (cons id file)))))
              (lambda (reason) (push (cons id reason) failures)))
             handles))
          (should (= 1 queue-count))
          (should-not (eq (car handles) (cadr handles)))
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "complete"))
            (funcall (plist-get properties :then) part))
          (should (equal `((first . ,target) (second . ,target)) successes))
          (should-not failures)
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-deduplicated-callers-cancel-independently ()
  (let* ((directory (make-temp-file "appkit-media-caller-cancel" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         first-success
         first-error
         second-success
         second-error)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     nil)))
          (let ((first
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/shared.bin")) target
                  (lambda (file) (setq first-success file))
                  (lambda (reason) (setq first-error reason))))
                (second
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/shared.bin")) target
                  (lambda (file) (setq second-success file))
                  (lambda (reason) (setq second-error reason)))))
            (should-not (eq first second))
            (should (appkit-media-cancel-transfer first))
            (should (equal "transfer canceled" first-error))
            (should-not first-success)
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as))))
              (with-temp-file part (insert "complete"))
              (funcall (plist-get properties :then) part))
            (should (equal target second-success))
            (should-not second-error)))
      (delete-directory directory t))))

(ert-deftest appkit-media-transfer-rejects-conflicting-source-for-target ()
  (let* ((directory (make-temp-file "appkit-media-conflict" t))
         (target (expand-file-name "shared.bin" directory))
         (local (expand-file-name "local.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         (plz-count 0)
         remote-error
         local-error)
    (unwind-protect
        (progn
          (with-temp-file local (insert "local"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest arguments)
                       (cl-incf plz-count)
                       (setq plz-arguments arguments)
                       nil)))
            (appkit-media-copy-or-download-resource-async
             '((url . "https://example.invalid/first.bin")) target
             #'ignore #'ignore)
            (should-not
             (appkit-media-copy-or-download-resource-async
              '((url . "https://example.invalid/second.bin")) target
              #'ignore (lambda (reason) (setq remote-error reason))))
            (should-not
             (appkit-media-copy-or-download-resource-async
              `((file . ,local)) target
              #'ignore (lambda (reason) (setq local-error reason))))
            (should (= 1 plz-count))
            (should (string-match-p "another source" remote-error))
            (should (string-match-p "another source" local-error))
            (funcall (plist-get (nthcdr 2 plz-arguments) :else)
                     "test cleanup")))
      (delete-directory directory t))))

(ert-deftest appkit-media-transfer-broadcast-survives-caller-quit ()
  (let* ((directory (make-temp-file "appkit-media-callback-quit" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         second-called)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     nil))
                  ((symbol-function 'message) #'ignore))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/shared.bin")) target
           (lambda (_file) (signal 'quit nil)) #'ignore)
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/shared.bin")) target
           (lambda (_file) (setq second-called t)) #'ignore)
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "complete"))
            (funcall (plist-get properties :then) part))
          (should second-called))
      (delete-directory directory t))))

(ert-deftest appkit-media-delayed-start-error-finishes-pending-transfer ()
  (let* ((directory (make-temp-file "appkit-media-delayed-error" t))
         (first-target (expand-file-name "first.bin" directory))
         (second-target (expand-file-name "second.bin" directory))
         (appkit-media-transfer-concurrency 1)
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         first-arguments
         (plz-count 0)
         second-error)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (cl-incf plz-count)
                     (if (= plz-count 1)
                         (progn (setq first-arguments arguments) nil)
                       (error "delayed setup exploded")))))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/first.bin")) first-target
           #'ignore #'ignore)
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/second.bin")) second-target
           #'ignore (lambda (reason) (setq second-error reason)))
          (should (= 1 (length appkit-media--pending-transfers)))
          (let* ((properties (nthcdr 2 first-arguments))
                 (part (cadr (plist-get properties :as))))
            (with-temp-file part (insert "first"))
            (funcall (plist-get properties :then) part))
          (should (string-match-p "delayed setup exploded" second-error))
          (should-not appkit-media--pending-transfers)
          (should (= 0 appkit-media--active-transfer-count))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-else-cleans-partial-and-broadcasts ()
  (let* ((directory (make-temp-file "appkit-media-else" t))
         (target (expand-file-name "shared.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         successes
         failures)
    (unwind-protect
        (progn
          (with-temp-file target (insert "previous complete value"))
          (cl-letf (((symbol-function 'plz)
                     (lambda (&rest arguments)
                       (setq plz-arguments arguments)
                       :remote-process)))
            (dotimes (id 2)
              (appkit-media-copy-or-download-resource-async
               '((url . "https://example.invalid/shared.bin")) target
               (lambda (file) (push (cons id file) successes))
               (lambda (reason) (push (cons id reason) failures))))
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as)))
                   (staging (file-name-directory part)))
              (with-temp-file part (insert "incomplete"))
              (funcall (plist-get properties :else) "network failed")
              (should-not (file-exists-p part))
              (should-not (file-directory-p staging))))
          (should-not successes)
          (should (= 2 (length failures)))
          (should (seq-every-p
                   (lambda (entry) (equal "network failed" (cdr entry)))
                   failures))
          (should (equal "previous complete value"
                         (with-temp-buffer
                           (insert-file-contents-literally target)
                           (buffer-string))))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-cleans-up-synchronous-enqueue-error ()
  (let* ((directory (make-temp-file "appkit-media-enqueue-error" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         part
         success-value
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq part (cadr (plist-get (nthcdr 2 arguments) :as)))
                     (with-temp-file part (insert "partial"))
                     (error "enqueue exploded"))))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/target.bin")) target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (should-not success-value)
          (should (string-match-p "enqueue exploded" failure))
          (should part)
          (should-not (file-exists-p part))
          (should-not (file-directory-p (file-name-directory part)))
          (should-not (file-exists-p target))
          (should (= 0 (hash-table-count appkit-media--inflight-transfers))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-rejects-missing-partial ()
  (let* ((directory (make-temp-file "appkit-media-missing-part" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         success-value
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     :remote-process)))
          (appkit-media-copy-or-download-resource-async
           '((url . "https://example.invalid/target.bin")) target
           (lambda (file) (setq success-value file))
           (lambda (reason) (setq failure reason)))
          (let* ((properties (nthcdr 2 plz-arguments))
                 (part (cadr (plist-get properties :as)))
                 (staging (file-name-directory part)))
            (funcall (plist-get properties :then) part)
            (should-not (file-directory-p staging)))
          (should-not success-value)
          (should (string-match-p "no regular partial file" failure))
          (should-not (file-exists-p target)))
      (delete-directory directory t))))

(ert-deftest appkit-media-image-cache-normalizes-sniffs-and-cleans-siblings ()
  (let* ((directory (make-temp-file "appkit-media-image-cache" t))
         (source (expand-file-name "opaque" directory))
         (cache-base (expand-file-name "cache/entry" directory))
         (stale (format "%s.jpg" cache-base))
         final
         failure)
    (unwind-protect
        (progn
          (make-directory (file-name-directory cache-base) t)
          (with-temp-file source
            (set-buffer-multibyte nil)
            (insert "\r\n\x89PNG\r\n\x1a\nimage bytes"))
          (with-temp-file stale (insert "stale"))
          (should-not
           (appkit-media-cache-image-resource-async
            `((file . ,source)) cache-base
            (lambda (file) (setq final file))
            (lambda (reason) (setq failure reason))))
          (should-not failure)
          (should (equal (format "%s.png" cache-base) final))
          (should (file-exists-p final))
          (should-not (file-exists-p stale))
          (should
           (equal "\x89PNG"
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally final nil 0 4)
                    (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-image-cache-isolates-success-callback-errors ()
  (let* ((directory (make-temp-file "appkit-media-image-callback" t))
         (source (expand-file-name "source.png" directory))
         (cache-base (expand-file-name "cache/entry" directory))
         (success-count 0)
         errors)
    (unwind-protect
        (progn
          (with-temp-file source
            (set-buffer-multibyte nil)
            (insert "\x89PNG\r\n\x1a\nimage bytes"))
          (should-not
           (appkit-media-cache-image-resource-async
            `((file . ,source)) cache-base
            (lambda (_file)
              (cl-incf success-count)
              (error "consumer callback failed"))
            (lambda (reason) (push reason errors))))
          (should (= 1 success-count))
          (should-not errors)
          (should (file-exists-p (format "%s.png" cache-base))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-transfer-cancel-removes-pending-request ()
  (let* ((directory (make-temp-file "appkit-media-cancel" t))
         (target (expand-file-name "target.bin" directory))
         (appkit-media-transfer-concurrency 1)
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 1)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         success-value
         failures)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _)
                     (ert-fail "pending transfer must not start"))))
          (let ((transfer
                 (appkit-media-copy-or-download-resource-async
                  '((url . "https://example.invalid/target.bin")) target
                  (lambda (file) (setq success-value file))
                  (lambda (reason) (push reason failures)))))
            (should (appkit-media-transfer-p transfer))
            (should (= 1 (length appkit-media--pending-transfers)))
            (should (appkit-media-cancel-transfer transfer))
            (should-not (appkit-media-cancel-transfer transfer))
            (should-not appkit-media--pending-transfers)
            (should-not success-value)
            (should (equal '("transfer canceled") failures))
            (should-not (file-exists-p target))
            (should (= 0 (hash-table-count appkit-media--inflight-transfers)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-remote-image-open-is-asynchronous-and-atomic ()
  (let* ((directory (make-temp-file "appkit-media-open-image" t))
         (appkit-media--pending-transfers nil)
         (appkit-media--active-transfer-count 0)
         (appkit-media--scheduling-transfers-p nil)
         (appkit-media--inflight-transfers (make-hash-table :test #'equal))
         plz-arguments
         opened
         updated)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest arguments)
                     (setq plz-arguments arguments)
                     :remote-process))
                  ((symbol-function 'url-copy-file)
                   (lambda (&rest _)
                     (ert-fail "remote image open must use atomic acquisition"))))
          (let ((appkit-media-open-file-function
                 (lambda (file) (setq opened file) :opened))
                (appkit-media-animate-gifs nil))
            (should-not
             (appkit-media-open-resource
              '((url . "https://example.invalid/opaque"))
              :kind 'image
              :cache-directory directory
              :cache-update-function
              (lambda (resource) (setq updated resource))
              :client-label "test"))
            (should-not opened)
            (should-not updated)
            (let* ((properties (nthcdr 2 plz-arguments))
                   (part (cadr (plist-get properties :as))))
              (with-temp-file part
                (set-buffer-multibyte nil)
                (insert "\x89PNG\r\n\x1a\nimage bytes"))
              (funcall (plist-get properties :then) part))
            (should (string-suffix-p ".png" opened))
            (should (file-exists-p opened))
            (should (equal opened (alist-get 'file updated)))))
      (delete-directory directory t))))

(ert-deftest appkit-media-video-player-builds-explicit-command ()
  (let ((appkit-media-video-player-command "mpv --no-terminal")
        process-properties)
    (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
               (lambda (command)
                 (should (equal command "mpv --no-terminal"))
                 t))
              ((symbol-function 'make-process)
               (lambda (&rest properties)
                 (setq process-properties properties)
                 :player-process))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'browse-url)
               (lambda (&rest _)
                 (ert-fail "video playback must not use a browser"))))
      (should
       (eq :player-process
           (appkit-media-play-video-source
            "https://example.invalid/movie.mp4" "test-client")))
      (should
       (equal '("mpv" "--no-terminal"
                "https://example.invalid/movie.mp4")
              (plist-get process-properties :command)))
      (should (equal "appkit-media-video-player"
                     (plist-get process-properties :name)))
      (should (eq t (plist-get process-properties :noquery))))))

(ert-deftest appkit-media-owned-video-player-stops-with-app ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'owned
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        deleted
        sentinel)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq sentinel (plist-get properties :sentinel))
                     :owned-player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :owned-player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object)
                     (and (eq object :owned-player) live-p)))
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (process)
                     (should (eq process :owned-player))
                     (setq live-p nil deleted t)))
                  ((symbol-function 'message) #'ignore))
          (should
           (eq :owned-player
               (appkit-media-play-video-source
                "https://example.invalid/owned.mp4" "test"
                :owner app)))
          (should (functionp sentinel))
          (should (= 1 (length (appkit-app-handles app))))
          (appkit-stop-app app)
          (should deleted)
          (should-not live-p)
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-owned-video-player-stops-with-view ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'view
                               :shutdown #'ignore))
        (buffer (generate-new-buffer " *appkit-media-view-owner*"))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        (delete-count 0)
        view)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq view
                  (appkit-attach-view
                   :app app :id 'video :mode major-mode)))
          (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                     (lambda (_) t))
                    ((symbol-function 'make-process)
                     (lambda (&rest _properties) :view-player))
                    ((symbol-function 'processp)
                     (lambda (object) (eq object :view-player)))
                    ((symbol-function 'process-live-p)
                     (lambda (object)
                       (and (eq object :view-player) live-p)))
                    ((symbol-function 'set-process-filter) #'ignore)
                    ((symbol-function 'set-process-sentinel) #'ignore)
                    ((symbol-function 'delete-process)
                     (lambda (process)
                       (should (eq process :view-player))
                       (setq live-p nil
                             delete-count (1+ delete-count))))
                    ((symbol-function 'message) #'ignore))
            (should
             (eq :view-player
                 (appkit-media-play-video-source
                  "https://example.invalid/view.mp4" "test" :owner view)))
            (should (= 1 (length (appkit-view-handles view))))
            (appkit-kill-view view)
            (should (= delete-count 1))
            (should-not live-p)
            (should-not (appkit-view-handles view))
            (should-not (appkit-view-live-p view))))
      (when (appkit-app-live-p app)
        (appkit-stop-app app))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest appkit-media-video-player-sync-terminal-spawn-retires-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'sync-terminal
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        deleted)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (should (= 1 (length (appkit-app-handles app))))
                     (setq live-p nil)
                     (funcall (plist-get properties :sentinel)
                              :sync-terminal-player "finished\n")
                     :sync-terminal-player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :sync-terminal-player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object)
                     (and (eq object :sync-terminal-player) live-p)))
                  ((symbol-function 'process-status)
                   (lambda (object)
                     (should (eq object :sync-terminal-player))
                     (if live-p 'run 'exit)))
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (_) (setq deleted t)))
                  ((symbol-function 'message) #'ignore))
          (should
           (eq :sync-terminal-player
               (appkit-media-play-video-source
                "https://example.invalid/sync-terminal.mp4" "test"
                :owner app)))
          (should-not (appkit-app-handles app))
          (appkit-stop-app app)
          (should-not deleted))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-constructor-error-retires-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'constructor-error
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (constructor-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _properties)
                     (setq constructor-calls (1+ constructor-calls))
                     (should (= 1 (length (appkit-app-handles app))))
                     (error "player constructor failed"))))
          (let ((condition
                 (should-error
                  (appkit-media-play-video-source
                   "https://example.invalid/constructor-error.mp4" "test"
                   :owner app))))
            (should (equal (error-message-string condition)
                           "player constructor failed")))
          (should (= constructor-calls 1))
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-constructor-throw-retires-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'constructor-throw
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (constructor-calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest _properties)
                     (setq constructor-calls (1+ constructor-calls))
                     (should (= 1 (length (appkit-app-handles app))))
                     (throw 'appkit-media-constructor-exit :escaped))))
          (should
           (eq :escaped
               (catch 'appkit-media-constructor-exit
                 (appkit-media-play-video-source
                  "https://example.invalid/constructor-throw.mp4" "test"
                  :owner app)
                 :returned)))
          (should (= constructor-calls 1))
          (should-not (appkit-app-handles app)))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-nonterminal-sentinel-keeps-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'nonterminal
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        (delete-count 0)
        sentinel)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq sentinel (plist-get properties :sentinel))
                     :nonterminal-player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :nonterminal-player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object)
                     (and (eq object :nonterminal-player) live-p)))
                  ((symbol-function 'process-status)
                   (lambda (object)
                     (should (eq object :nonterminal-player))
                     (if live-p 'stop 'exit)))
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (process)
                     (should (eq process :nonterminal-player))
                     (setq live-p nil
                           delete-count (1+ delete-count))))
                  ((symbol-function 'message) #'ignore))
          (appkit-media-play-video-source
           "https://example.invalid/nonterminal.mp4" "test" :owner app)
          (should (= 1 (length (appkit-app-handles app))))
          (funcall sentinel :nonterminal-player "stopped\n")
          (should (= 1 (length (appkit-app-handles app))))
          (should live-p)
          (appkit-stop-app app)
          (should (= delete-count 1))
          (should-not live-p))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-natural-exit-retires-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'natural
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        deleted
        sentinel)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
                   (lambda (_) t))
                  ((symbol-function 'make-process)
                   (lambda (&rest properties)
                     (setq sentinel (plist-get properties :sentinel))
                     :natural-player))
                  ((symbol-function 'processp)
                   (lambda (object) (eq object :natural-player)))
                  ((symbol-function 'process-live-p)
                   (lambda (object)
                     (and (eq object :natural-player) live-p)))
                  ((symbol-function 'set-process-filter) #'ignore)
                  ((symbol-function 'set-process-sentinel) #'ignore)
                  ((symbol-function 'delete-process)
                   (lambda (_) (setq deleted t)))
                  ((symbol-function 'message) #'ignore))
          (appkit-media-play-video-source
           "https://example.invalid/natural.mp4" "test" :owner app)
          (should (= 1 (length (appkit-app-handles app))))
          (setq live-p nil)
          (funcall sentinel :natural-player "finished\n")
          (should-not (appkit-app-handles app))
          (should-not deleted))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-stop-during-spawn-cancels-returned-process ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'reentrant
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        (live-p t)
        deleted)
    (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
               (lambda (_) t))
              ((symbol-function 'make-process)
               (lambda (&rest _)
                 (appkit-stop-app app)
                 :late-player))
              ((symbol-function 'processp)
               (lambda (object) (eq object :late-player)))
              ((symbol-function 'process-live-p)
               (lambda (object) (and (eq object :late-player) live-p)))
              ((symbol-function 'set-process-filter) #'ignore)
              ((symbol-function 'set-process-sentinel) #'ignore)
              ((symbol-function 'delete-process)
               (lambda (process)
                 (should (eq process :late-player))
                 (setq live-p nil deleted t)))
              ((symbol-function 'message) #'ignore))
      (should
       (eq :late-player
           (appkit-media-play-video-source
            "https://example.invalid/late.mp4" "test" :owner app)))
      (should deleted)
      (should-not live-p)
      (should-not (appkit-app-handles app)))))

(ert-deftest appkit-media-video-player-dead-owner-never-spawns ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'dead
                               :shutdown #'ignore))
        (appkit-media-video-player-command '("mpv"))
        spawned)
    (appkit-stop-app app)
    (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
               (lambda (_) t))
              ((symbol-function 'make-process)
               (lambda (&rest _)
                 (setq spawned t)
                 :impossible)))
      (should-error
       (appkit-media-play-video-source
        "https://example.invalid/dead.mp4" "test" :owner app))
      (should-not spawned))))

(ert-deftest appkit-media-video-entrypoints-forward-exact-owner ()
  (let ((app (appkit-start-app 'appkit-media-test :id 'forward
                               :shutdown #'ignore))
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-media-play-video-source)
                   (lambda (source &optional label &rest keys)
                     (push (list source label (plist-get keys :owner)) calls)
                     :played)))
          (should
           (eq :played
               (appkit-media-play-video-url
                "https://example.invalid/url.mp4" "url" :owner app)))
          (should
           (eq :played
               (appkit-media-open-resource
                '((url . "https://example.invalid/resource.mp4"))
                :kind 'video :client-label "resource" :owner app)))
          (with-temp-buffer
            (insert "video")
            (appkit-media-add-play-video-properties
             (point-min) (point-max) "https://example.invalid/action.mp4"
             "action" :owner app)
            (let* ((map (get-text-property (point-min) 'keymap))
                   (command (lookup-key map (kbd "RET"))))
              (should (commandp command))
              (funcall command)))
          (should
           (equal
            (nreverse calls)
            `(("https://example.invalid/url.mp4" "url" ,app)
              ("https://example.invalid/resource.mp4" "resource" ,app)
              ("https://example.invalid/action.mp4" "action" ,app)))))
      (when (appkit-app-live-p app)
        (appkit-stop-app app)))))

(ert-deftest appkit-media-video-player-errors-are-explicit ()
  (let ((appkit-media-video-player-command nil))
    (should-error
     (appkit-media-play-video-source
      "https://example.invalid/movie.mp4" "qq")
     :type 'user-error))
  (let ((appkit-media-video-player-command '("missing-player")))
    (cl-letf (((symbol-function 'appkit-media-command-runnable-p)
               (lambda (_command) nil)))
      (should-error
       (appkit-media-play-video-source
        "https://example.invalid/movie.mp4" "disco")
       :type 'user-error)))
  (should-error
   (appkit-media-play-video-source nil "client")
   :type 'user-error)
  (should-error
   (appkit-media-play-video-file
    "/definitely/missing/appkit-video.mp4" "client")
   :type 'user-error))

(provide 'appkit-media-resource-test)

;;; appkit-media-resource-test.el ends here
