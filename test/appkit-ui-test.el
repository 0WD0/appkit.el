;;; appkit-ui-test.el --- Shared presentation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-ui)

(ert-deftest appkit-ui-prefix-state-consumes-first-prefix-once ()
  (let ((state (appkit-ui-make-prefix-state "first> " "rest> ")))
    (should (equal "raw> "
                   (appkit-ui-prefix-string "raw> " nil "fallback> ")))
    (should (equal "first> "
                   (appkit-ui-prefix-string state t "fallback> ")))
    (should (equal "rest> "
                   (appkit-ui-prefix-string state nil "fallback> ")))))

(ert-deftest appkit-ui-prefixed-lines-apply-first-and-rest-prefixes ()
  (with-temp-buffer
    (let ((state (appkit-ui-make-prefix-state "A " "B ")))
      (appkit-ui-insert-prefixed-lines state "first\nsecond")
      (goto-char (point-min))
      (should (equal "A " (get-text-property (point) 'line-prefix)))
      (forward-line 1)
      (should (equal "B " (get-text-property (point) 'line-prefix))))))

(provide 'appkit-ui-test)

;;; appkit-ui-test.el ends here
