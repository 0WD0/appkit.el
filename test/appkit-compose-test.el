;;; appkit-compose-test.el --- Tests for appkit-compose -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'appkit-compose)

(ert-deftest appkit-compose-refresh-separates-body-and-generated-chrome ()
  "Generated compose chrome must remain outside the editable composer."
  (with-temp-buffer
    (appkit-compose-mode)
    (let (field-action)
      (appkit-compose-setup
       :context-function (lambda () "Context")
       :status-fields-function
       (lambda ()
         (list (list :label "Audience"
                     :value "Everyone"
                     :action (lambda ()
                               (setq field-action t)))))
       :attachments-function
       (lambda ()
         (list :title "Media"
               :items
               (list (list :label "photo.png"
                           :object 'photo))))
       :footer-function (lambda () "Send / Cancel"))
      (should (string-match-p "Context" (appkit-compose-display-string)))
      (should (string-match-p "Audience: Everyone"
                              (appkit-compose-display-string)))
      (should (string-match-p "Send / Cancel"
                              (appkit-compose-display-string)))
      (goto-char (appkit-compose-body-start-position))
      (insert "hello")
      (should (equal (appkit-compose-body) "hello"))
      (should (appkit-chatbuf-point-in-input-p))
      (should-not
       (get-text-property (appkit-compose-body-start-position) 'read-only))
      (appkit-compose-refresh)
      (should (equal (appkit-compose-body) "hello"))
      (should (string-match-p "hello" (appkit-compose-display-string))))))

(ert-deftest appkit-compose-refresh-omits-empty-attachment-section ()
  "An empty attachment section should not occupy the composer frame."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-setup
     :attachments-function
     (lambda ()
       '(:title "Media"
         :items nil
         :empty-label "  No media attached.")))
    (goto-char (appkit-compose-body-start-position))
    (insert "draft")
    (appkit-compose-add-item)
    (should-not (string-match-p "Media" (appkit-compose-display-string)))
    (should-not (string-match-p "No media attached"
                                (appkit-compose-display-string)))
    (should (equal (appkit-compose-bodies) '("draft" "")))))

(ert-deftest appkit-compose-frame-keeps-header-footer-and-prompt-apart ()
  "Header, footer, and prompt must not concatenate into one line."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-setup
     :context-function (lambda () "Context")
     :attachments-function
     (lambda ()
       (list :title "Media"
             :items (list (list :label "photo.png"))))
     :footer-function (lambda () "Send / Cancel"))
    (let ((display (appkit-compose-display-string)))
      (should (string-match-p "Context\n\n" display))
      (should (string-match-p "Media\n" display))
      (should (string-match-p "Send / Cancel\n\n>>> " display))
      (should-not (string-match-p "ContextMedia" display))
      (should-not (string-match-p "Cancel>>>" display)))))

(ert-deftest appkit-compose-setup-rejects-non-callable-callbacks ()
  "Compose setup should reject malformed client callbacks immediately."
  (with-temp-buffer
    (appkit-compose-mode)
    (should-error
     (appkit-compose-setup :context-function "not a function"))))

(ert-deftest appkit-compose-submit-tracks-progress-and-cancel ()
  "Compose submit state should be presentation-only and client-canceled."
  (with-temp-buffer
    (appkit-compose-mode)
    (let (canceled)
      (should-not (appkit-compose-submitting-p))
      (should-not (appkit-compose-progress-text))
      (should-not (appkit-compose-cancel-submit))
      (appkit-compose-begin-submit
       :label "Uploading video"
       :progress 0.25
       :cancel-function (lambda () (setq canceled t)))
      (should (appkit-compose-submitting-p))
      (should (string-match-p "Uploading video" (appkit-compose-progress-text)))
      (should (string-match-p "25%" (appkit-compose-progress-text)))
      (should-error (appkit-compose-begin-submit :label "again"))
      (appkit-compose-update-submit :label "Uploading video" :progress 0.5)
      (should (string-match-p "50%" (appkit-compose-progress-text)))
      (should (appkit-compose-cancel-submit))
      (should canceled)
      (should (appkit-compose-submitting-p))
      (should-error (appkit-compose-cancel-submit) :type 'user-error)
      (appkit-compose-finish-submit)
      (should-not (appkit-compose-submitting-p))
      (should-not (appkit-compose-progress-text)))))

(ert-deftest appkit-compose-cancel-submit-refuses-without-hook ()
  "A submit without a cancel hook should refuse the cancel gesture."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-begin-submit :label "Publishing")
    (should-error (appkit-compose-cancel-submit) :type 'user-error)
    (should (appkit-compose-submitting-p))
    (appkit-compose-finish-submit)))

(ert-deftest appkit-compose-progress-bar-fills-from-ratio ()
  "The shared progress bar should stay a fixed width."
  (should (equal (appkit-compose-progress-bar 0 10) "          "))
  (should (equal (appkit-compose-progress-bar 0.5 10) "====>     "))
  (should (equal (length (appkit-compose-progress-bar 1 10)) 10)))

(ert-deftest appkit-compose-refresh-preserves-multiple-bodies ()
  "A multi-part compose surface should keep each body on refresh."
  (with-temp-buffer
    (appkit-compose-mode)
    (let ((count 2))
      (appkit-compose-setup
       :context-function (lambda () "Context")
       :parts-function
       (lambda ()
         (cl-loop for index from 1 to count
                  collect (list :title (format "Part %d" index)
                                :attachments
                                (list :title "Media"
                                      :items nil
                                      :empty-label "  None."))))
       :footer-function (lambda () "Footer"))
      (goto-char (appkit-compose-body-start-position))
      (insert "first")
      (setq count 3)
      (appkit-compose-add-item)
      (should (equal (appkit-compose-bodies) '("first" "")))
      (appkit-compose-goto-part 0)
      (should (equal (appkit-compose-body) "first"))
      (appkit-compose-add-item)
      (insert "second")
      (appkit-compose-refresh)
      (should (equal (appkit-compose-bodies) '("first" "second" "")))
      (should (eq (appkit-compose-current-part-index) 1))
      (should (equal (appkit-compose-body) "second")))))

(ert-deftest appkit-compose-undo-does-not-duplicate-after-chrome-refresh ()
  "Chrome refresh must not record undo or rewrite editable bodies."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-setup
     :status-fields-function
     (lambda ()
       (list (list :label "Length"
                   :value (format "%d" (length (appkit-compose-body))))))
     :footer-function (lambda () "Footer"))
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (goto-char (appkit-compose-body-start-position))
    (insert "hello")
    (undo-boundary)
    (appkit-compose-refresh)
    (should (equal (appkit-compose-body) "hello"))
    (should (string-match-p "Length: 5" (appkit-compose-display-string)))
    (undo)
    (should (equal (appkit-compose-body) ""))
    (should-not (string-match-p "hellohello"
                                (appkit-compose-display-string)))))

(ert-deftest appkit-compose-add-and-drop-items-keep-order ()
  "Adding or dropping a draft item should keep the surrounding items."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-setup)
    (goto-char (appkit-compose-body-start-position))
    (insert "first")
    (appkit-compose-add-item)
    (insert "third")
    (appkit-compose-goto-part 0)
    (appkit-compose-add-item)
    (insert "second")
    (should (equal (appkit-compose-bodies) '("first" "second" "third")))
    (appkit-compose-goto-part 1)
    (appkit-compose-drop-item)
    (should (equal (appkit-compose-bodies) '("first" "third")))
    (should (eq (appkit-compose-current-part-index) 1))))

(provide 'appkit-compose-test)

;;; appkit-compose-test.el ends here
