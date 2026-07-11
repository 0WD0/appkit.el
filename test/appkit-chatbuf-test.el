;;; appkit-chatbuf-test.el --- Tests for appkit-chatbuf -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-chatbuf)

(ert-deftest appkit-chatbuf-install-prompt-creates-tail-input-region ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (= (appkit-chatbuf-input-start-position) (point-max)))
    (insert "hello")
    (should (appkit-chatbuf-has-input-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-string-helpers-preserve-and-inspect-properties ()
  (let ((text (copy-sequence "[file] a.txt")))
    (add-text-properties 0 (length text)
                         (list appkit-chatbuf-input-object-property
                               '(:kind attachment :path "/tmp/a.txt"))
                         text)
    (should (appkit-chatbuf-string-has-objects-p text))
    (should (equal "[file] a.txt"
                   (appkit-chatbuf-string-plain-text text)))
    (let ((copy (appkit-chatbuf-copy-string text)))
      (should-not (eq copy text))
      (should (appkit-chatbuf-string-has-objects-p copy)))))

(ert-deftest appkit-chatbuf-reset-state-reinitializes-history-and-markers ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (let ((old-input-marker appkit-chatbuf--input-marker)
          (old-prompt-marker appkit-chatbuf--prompt-marker))
      (appkit-chatbuf-aux-set '(:aux-type reply))
      (appkit-chatbuf-input-options-set '(:send-on-return t))
      (appkit-chatbuf-input-history-push "hello")
      (setq-local appkit-chatbuf--prompt-button 'dummy)
      (appkit-chatbuf-reset-state 3)
      (should (markerp appkit-chatbuf--input-marker))
      (should (markerp appkit-chatbuf--prompt-marker))
      (should-not (eq old-input-marker appkit-chatbuf--input-marker))
      (should-not (eq old-prompt-marker appkit-chatbuf--prompt-marker))
      (should (ring-p appkit-chatbuf--input-ring))
      (should (= 3 (ring-size appkit-chatbuf--input-ring)))
      (should (= 0 (ring-length appkit-chatbuf--input-ring)))
      (should-not appkit-chatbuf--input-idx)
      (should-not appkit-chatbuf--input-pending)
      (should-not appkit-chatbuf--aux-plist)
      (should-not appkit-chatbuf--input-options-plist)
      (should-not appkit-chatbuf--prompt-button))))

(ert-deftest appkit-chatbuf-input-state-set-clear-and-sync-preserve-properties ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (let ((text (copy-sequence "[file] a.txt")))
      (add-text-properties 0 (length text)
                           (list appkit-chatbuf-input-object-property
                                 '(:kind attachment :path "/tmp/a.txt"))
                           text)
      (appkit-chatbuf-input-state-set text)
      (should (appkit-chatbuf-string-has-objects-p
               (appkit-chatbuf-input-state)))
      (setq-local appkit-chatbuf--input-idx 3)
      (setq-local appkit-chatbuf--input-pending "pending")
      (appkit-chatbuf-input-state-clear :reset-history-p t)
      (should (equal "" (appkit-chatbuf-input-state)))
      (should-not appkit-chatbuf--input-idx)
      (should-not appkit-chatbuf--input-pending)
      (appkit-chatbuf-install-prompt ">>> ")
      (insert text)
      (setq-local appkit-chatbuf--input-idx 1)
      (setq-local appkit-chatbuf--input-pending "later")
      (let ((result (appkit-chatbuf-input-state-sync)))
        (should (eq t (plist-get result :changed-p)))
        (should (appkit-chatbuf-string-has-objects-p
                 (plist-get result :value)))
        (should (appkit-chatbuf-string-has-objects-p
                 (appkit-chatbuf-input-state)))
        (should-not appkit-chatbuf--input-idx)
        (should-not appkit-chatbuf--input-pending))
      (setq-local appkit-chatbuf--input-idx 2)
      (setq-local appkit-chatbuf--input-pending "keep")
      (let ((result (appkit-chatbuf-input-state-sync)))
        (should-not (plist-get result :changed-p))
        (should (= 2 appkit-chatbuf--input-idx))
        (should (equal "keep" appkit-chatbuf--input-pending))))))

(ert-deftest appkit-chatbuf-input-mutations-update-canonical-state ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-input-state-set "cached")
    (should (equal "cached" (appkit-chatbuf-input-state)))
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "live")
    (should (equal "live" (appkit-chatbuf-input-state)))))

(ert-deftest appkit-chatbuf-input-replace-preserves-relative-point-offset ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "hello world")
    (goto-char (+ (appkit-chatbuf-input-start-position) 5))
    (appkit-chatbuf-input-replace "goodbye")
    (should (equal "goodbye" (appkit-chatbuf-input-string)))
    (should (= 5 (- (point) (appkit-chatbuf-input-start-position))))))

(ert-deftest appkit-chatbuf-prompt-update-preserves-input-and-point-offset ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "hello")
    (goto-char (+ (appkit-chatbuf-input-start-position) 2))
    (appkit-chatbuf-prompt-update "qq> ")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))
    (should (= 2 (- (point) (appkit-chatbuf-input-start-position))))))

(ert-deftest appkit-chatbuf-bind-input-region-hides-and-restores-tail-input ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-bind-input-region
     :visible-p t
     :prompt ">>> "
     :input-text "hello"
     :post-bind-function
     (lambda ()
       (when-let* ((bounds (appkit-chatbuf-input-region-bounds)))
         (add-text-properties (car bounds) (cdr bounds) '(demo t)))))
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "hello" (appkit-chatbuf-input-string)))
    (should (eq t (get-text-property (appkit-chatbuf-input-start-position) 'demo)))
    (appkit-chatbuf-bind-input-region :visible-p nil)
    (should-not (appkit-chatbuf-prompt-button-live-p))
    (should-not (appkit-chatbuf-input-start-position))
    (appkit-chatbuf-bind-input-region
     :visible-p t
     :prompt "qq> "
     :input-text "world")
    (should (appkit-chatbuf-prompt-button-live-p))
    (should (equal "world" (appkit-chatbuf-input-string)))))

(ert-deftest appkit-chatbuf-post-command-clamp-point-skips-prompt-glyphs ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (goto-char (appkit-chatbuf-prompt-start-position))
    (appkit-chatbuf-post-command-clamp-point)
    (should (= (point) (appkit-chatbuf-input-start-position)))))

(ert-deftest appkit-chatbuf-structured-object-insert-and-prune ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert "[file:a.txt]"
                                :object '(:type file :path "/tmp/a.txt"))
    (goto-char (appkit-chatbuf-input-start-position))
    (should (equal '(:type file :path "/tmp/a.txt")
                   (appkit-chatbuf-input-object-at-point)))
    (should (appkit-chatbuf-input-has-objects-p))
    (delete-char 1)
    (appkit-chatbuf-input-prune-broken-objects)
    (should (equal "" (or (appkit-chatbuf-input-string) "")))
    (should-not (appkit-chatbuf-input-has-objects-p))))

(ert-deftest appkit-chatbuf-input-history-restores-pending-input ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-set-text "first")
    (appkit-chatbuf-input-history-push)
    (appkit-chatbuf-input-set-text "second")
    (appkit-chatbuf-input-history-push)
    (should (equal '("second" "first")
                   (appkit-chatbuf-input-history-elements)))
    (appkit-chatbuf-input-set-text "pending")
    (appkit-chatbuf-input-history-prev)
    (should (appkit-chatbuf-input-history-active-p))
    (should (equal "second" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-prev)
    (should (equal "first" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-next)
    (should (equal "second" (appkit-chatbuf-input-string)))
    (appkit-chatbuf-input-history-next)
    (should (equal "pending" (appkit-chatbuf-input-string)))
    (should-not (appkit-chatbuf-input-history-active-p))))

(ert-deftest appkit-chatbuf-input-history-value-navigation-restores-structured-pending-input ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-input-history-push "first")
    (appkit-chatbuf-input-history-push "second")
    (let ((pending (copy-sequence "[file] a.txt")))
      (add-text-properties 0 (length pending)
                           (list appkit-chatbuf-input-object-property
                                 '(:kind attachment :path "/tmp/a.txt"))
                           pending)
      (let ((prev (appkit-chatbuf-input-history-prev-value pending)))
        (should (eq 'ok (plist-get prev :status)))
        (should (equal "second" (plist-get prev :value))))
      (let ((next (appkit-chatbuf-input-history-next-value)))
        (should (eq 'ok (plist-get next :status)))
        (should (appkit-chatbuf-string-has-objects-p (plist-get next :value)))
        (should-not appkit-chatbuf--input-idx)
        (should-not appkit-chatbuf--input-pending)))))

(ert-deftest appkit-chatbuf-input-history-value-navigation-reports-empty-and-latest ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (should (eq 'empty
                (plist-get (appkit-chatbuf-input-history-prev-value "pending")
                           :status)))
    (appkit-chatbuf-input-history-push "first")
    (should (eq 'latest
                (plist-get (appkit-chatbuf-input-history-next-value)
                           :status)))))

(ert-deftest appkit-chatbuf-input-history-push-explicit-text-ignores-live-object-buffer ()
  (with-temp-buffer
    (appkit-chatbuf-init-state 8)
    (appkit-chatbuf-install-prompt ">>> ")
    (appkit-chatbuf-input-insert "[file:a.txt]"
                                :object '(:type file :path "/tmp/a.txt"))
    (appkit-chatbuf-input-history-push "plain text")
    (should (equal '("plain text")
                   (appkit-chatbuf-input-history-elements)))))

(ert-deftest appkit-chatbuf-empty-input-remains-editable-at-point-max ()
  (save-window-excursion
    (let ((buffer (get-buffer-create " *appkit-chatbuf-input*")))
      (unwind-protect
          (progn
            (switch-to-buffer buffer)
            (erase-buffer)
            (appkit-chatbuf-init-state 8)
            (appkit-chatbuf-install-prompt ">>> ")
            (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
            (execute-kbd-macro "qs")
            (should (equal "qs" (appkit-chatbuf-input-string))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest appkit-chatbuf-aux-state-roundtrip ()
  (with-temp-buffer
    (appkit-chatbuf-init-state)
    (should-not (appkit-chatbuf-aux-active-p))
    (appkit-chatbuf-aux-set '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1"))
    (should (appkit-chatbuf-aux-active-p))
    (should (equal 'reply (appkit-chatbuf-aux-type)))
    (should (equal "m1" (appkit-chatbuf-aux-message-id)))
    (should (equal '(:aux-type reply :aux-msg ((id . "m1")) :message-id "m1")
                   (appkit-chatbuf-aux-state)))
    (appkit-chatbuf-aux-reset)
    (should-not (appkit-chatbuf-aux-active-p))))

(ert-deftest appkit-chatbuf-input-options-state-roundtrip ()
  (with-temp-buffer
    (appkit-chatbuf-init-state)
    (appkit-chatbuf-input-options-set
     '(:send-on-return t :allowed-mentions none))
    (should (equal '(:send-on-return t :allowed-mentions none)
                   (appkit-chatbuf-input-options-state)))
    (should (eq t (appkit-chatbuf-input-option :send-on-return)))
    (should (eq 'none (appkit-chatbuf-input-option :allowed-mentions)))
    (should (eq 'fallback (appkit-chatbuf-input-option :missing 'fallback)))
    (appkit-chatbuf-input-options-reset)
    (should-not (appkit-chatbuf-input-options-state))))

(provide 'appkit-chatbuf-test)

;;; appkit-chatbuf-test.el ends here
