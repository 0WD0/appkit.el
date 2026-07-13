;;; appkit-ui-test.el --- Shared presentation tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-ui)
(require 'mouse)

(defun appkit-ui-test--primary-click (window position)
  "Return a real primary-click event pair in WINDOW at POSITION."
  (let ((posn (list window position '(0 . 0) 0 nil position)))
    (vector (list 'down-mouse-1 posn)
            (list 'mouse-1 posn))))

(ert-deftest appkit-ui-action-row-dispatches-ret-and-exact-mouse-position ()
  (with-temp-buffer
    (let* ((buffer (current-buffer))
           (first-object (list :id 'first))
           (second-object (list :id 'second))
           calls
           (first-action (lambda (object)
                           (push (list 'first object) calls)))
           (second-action (lambda (object)
                            (push (list 'second object) calls)))
           first-button
           second-button)
      (let ((start (point)))
        (insert "first\n")
        (setq first-button
              (appkit-ui-make-action-row
               start (point) first-object first-action)))
      (let ((start (point)))
        (insert "second\n")
        (setq second-button
              (appkit-ui-make-action-row
               start (point) second-object second-action)))
      (should first-button)
      (should second-button)
      (should (eq #'push-button
                  (lookup-key appkit-ui-action-row-map (kbd "RET"))))
      (should (eq #'push-button
                  (lookup-key appkit-ui-action-row-map [mouse-1])))
      (should-not (keymap-parent appkit-ui-action-row-map))
      (should (eq first-object
                  (button-get first-button 'appkit-ui-action-row-object)))
      (should (eq first-action
                  (button-get first-button 'appkit-ui-action-row-action)))
      (should (eq second-object
                  (button-get second-button 'appkit-ui-action-row-object)))
      (should (eq second-action
                  (button-get second-button 'appkit-ui-action-row-action)))
      (save-window-excursion
        (switch-to-buffer buffer)
        (goto-char first-button)
        (execute-kbd-macro (kbd "RET"))
        (should (equal (pop calls) (list 'first first-object)))
        ;; Point deliberately remains on the first object.  Mouse activation
        ;; must use the exact event position on the adjacent second object.
        (goto-char first-button)
        (let ((mouse-1-click-follows-link 450))
          (execute-kbd-macro
           (appkit-ui-test--primary-click (selected-window) second-button)))
        (should (= (point) first-button)))
      (should (equal (pop calls) (list 'second second-object)))
      (should-not calls))))

(ert-deftest appkit-ui-action-row-excludes-eol-and-skips-empty-lines ()
  (with-temp-buffer
    (let ((buffer (current-buffer))
          (called 0)
          button
          newline-position)
      (let ((start (point)))
        (insert "body\n")
        (setq newline-position (1- (point)))
        (setq button
              (appkit-ui-make-action-row
               start (point) 'body (lambda (_object) (setq called (1+ called))))))
      (should button)
      (should-not (button-at newline-position))
      (save-window-excursion
        (switch-to-buffer buffer)
        (goto-char button)
        (let ((mouse-1-click-follows-link 450))
          (execute-kbd-macro
           (appkit-ui-test--primary-click (selected-window) newline-position))))
      (should (= called 0))
      (let ((empty-start (point)))
        (insert "\n")
        (should-not
         (appkit-ui-make-action-row
          empty-start (point) 'empty
          (lambda (_object) (setq called (1+ called)))))
        (should-not (button-at empty-start)))
      (should (= called 0)))))

(ert-deftest appkit-ui-action-row-hover-is-explicit-and-never-a-follow-link ()
  (with-temp-buffer
    (let (plain-button highlighted-button)
      (let ((start (point)))
        (insert "plain\n")
        (setq plain-button
              (appkit-ui-make-action-row
               start (point) 'plain #'ignore :help-echo "Open plain")))
      (let ((start (point)))
        (insert (propertize "highlighted" 'face 'bold) "\n")
        (setq highlighted-button
              (appkit-ui-make-action-row
               start (point) 'highlighted #'ignore
               :help-echo "Open highlighted"
               :mouse-face 'highlight)))
      (should (equal "Open plain" (button-get plain-button 'help-echo)))
      (should-not (button-get plain-button 'mouse-face))
      (should-not (button-type-get 'appkit-ui-action-row-button 'face))
      (should (eq 'bold (button-get highlighted-button 'face)))
      (should (eq 'highlight (button-get highlighted-button 'mouse-face)))
      (should-not (button-get highlighted-button 'follow-link))
      (should-not (lookup-key (button-get highlighted-button 'keymap)
                              [follow-link]))
      (should-not (mouse-on-link-p highlighted-button)))))

(ert-deftest appkit-ui-action-row-rejects-invalid-action-and-empty-span ()
  (with-temp-buffer
    (insert "invalid\n")
    (should-not
     (appkit-ui-make-action-row (point-min) (point) 'object 'not-a-function))
    (should-not (button-at (point-min)))
    (goto-char (point-max))
    (should-not
     (appkit-ui-make-action-row (point) (point) 'object #'ignore))
    (should-not (button-at (point)))))

(ert-deftest appkit-ui-action-row-rejects-multiline-span ()
  (with-temp-buffer
    (let ((called nil)
          (start (point)))
      (insert "first\nsecond\n")
      (should-not
       (appkit-ui-make-action-row
        start (point) 'object (lambda (_object) (setq called t))))
      (should-not (button-at start))
      (should-not (button-at (line-beginning-position 0)))
      (should-not called))))

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
