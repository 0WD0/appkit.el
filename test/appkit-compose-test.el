;;; appkit-compose-test.el --- Tests for appkit-compose -*- lexical-binding: t; -*-

(require 'button)
(require 'ert)
(require 'appkit-compose)

(ert-deftest appkit-compose-refresh-separates-body-and-generated-chrome ()
  "Generated compose chrome must remain read-only around an editable body."
  (with-temp-buffer
    (appkit-compose-mode)
    (let (field-action attachment-action)
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
                           :object 'photo
                           :action (lambda (object)
                                     (setq attachment-action object))))))
       :footer-function (lambda () "Send / Cancel"))
      (goto-char (appkit-compose-body-start-position))
      (insert "hello")
      (should (equal (appkit-compose-body) "hello"))
      (should (get-text-property (point-min) 'read-only))
      (should-not
       (get-text-property (appkit-compose-body-start-position) 'read-only))
      (goto-char (point-min))
      (search-forward "Audience: Everyone")
      (button-activate (button-at (1- (point))))
      (should field-action)
      (goto-char (point-min))
      (search-forward "photo.png")
      (button-activate (button-at (1- (point))))
      (should (eq attachment-action 'photo))
      (goto-char (+ (appkit-compose-body-start-position) 2))
      (appkit-compose-refresh)
      (should (= (point) (+ (appkit-compose-body-start-position) 2)))
      (goto-char (1- (point-max)))
      (should (get-text-property (point) 'read-only)))))

(ert-deftest appkit-compose-refresh-renders-empty-attachment-section ()
  "An attachment section should retain its empty state without client chrome."
  (with-temp-buffer
    (appkit-compose-mode)
    (appkit-compose-setup
     :attachments-function
     (lambda ()
       '(:title "Media"
         :items nil
         :empty-label "  No media attached.")))
    (should (string-match-p "Media" (buffer-string)))
    (should (string-match-p "No media attached" (buffer-string)))
    (goto-char (appkit-compose-body-start-position))
    (insert "draft")
    (should (equal (appkit-compose-body) "draft"))))

(ert-deftest appkit-compose-setup-rejects-non-callable-callbacks ()
  "Compose setup should reject malformed client callbacks immediately."
  (with-temp-buffer
    (appkit-compose-mode)
    (should-error
     (appkit-compose-setup :context-function "not a function"))))

(provide 'appkit-compose-test)

;;; appkit-compose-test.el ends here
