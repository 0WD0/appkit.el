;;; appkit-discussion-test.el --- Tests for discussion rows -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-discussion)

(ert-deftest appkit-discussion-entry-owns-thread-layout-and-properties ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _arguments)
                 '(:header "H " :first-body "B " :rest-body "R "))))
      (appkit-discussion-insert-entry
       (appkit-discussion-entry-create
        :key "reply-2"
        :parent-key "comment-1"
        :depth 1
        :heading "Author replies"
        :time "12:34"
        :body-inserter
        (lambda (prefix properties)
          (appkit-ui-insert-prefixed-lines
           prefix "first\nsecond" :properties properties))
        :footer "3/5 replies"
        :properties '(client-entry "reply-2"
                      rear-nonsticky (client-entry)))
       :width 50
       :indent-width 3))
    (should (string-match-p "Author replies" (buffer-string)))
    (should (string-match-p "first\nsecond\n3/5 replies" (buffer-string)))
    (goto-char (point-min))
    (should (equal "reply-2" (appkit-discussion-key-at-point)))
    (should (equal "comment-1"
                   (get-text-property
                    (point) appkit-discussion-parent-key-property)))
    (should (= 1 (get-text-property
                  (point) appkit-discussion-depth-property)))
    (should (equal "reply-2" (get-text-property (point) 'client-entry)))
    (should
     (equal
      (sort (copy-sequence (get-text-property (point) 'rear-nonsticky))
            (lambda (left right)
              (string-lessp (symbol-name left) (symbol-name right))))
      (sort (list 'client-entry
                  appkit-discussion-key-property
                  appkit-discussion-parent-key-property
                  appkit-discussion-depth-property)
            (lambda (left right)
              (string-lessp (symbol-name left) (symbol-name right))))))
    (should (string-prefix-p
             "   H " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p
             "   B " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (string-prefix-p
             "   R " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-entry-can-omit-avatar-prefix ()
  "An entry without a shared avatar reserves only nesting indentation."
  (with-temp-buffer
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key "plain"
      :parent-key "root"
      :depth 1
      :heading "Plain"
      :body-inserter
      (lambda (prefix properties)
        (appkit-ui-insert-prefixed-lines
         prefix "body" :properties properties)))
     :avatar-p nil
     :indent-width 3)
    (goto-char (point-min))
    (should (equal "Plain" (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))))
    (should (equal "   " (get-text-property (point) 'line-prefix)))
    (forward-line 1)
    (should (equal "body" (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
    (should (equal "   " (get-text-property (point) 'line-prefix)))))

(ert-deftest appkit-discussion-entry-requires-parent-for-nesting ()
  (with-temp-buffer
    (should-error
     (appkit-discussion-insert-entry
      (appkit-discussion-entry-create :key "orphan" :depth 1))
     :type 'error)))

(ert-deftest appkit-discussion-avatar-adapts-a-plain-callback-to-a-command ()
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
                 (lambda (&rest _arguments)
                   '(:header "A " :first-body "B " :rest-body "  "))))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create
          :key "action"
          :heading "Action"
          :avatar-action (lambda () (setq called t))))
        (let* ((prefix (get-text-property (point-min) 'line-prefix))
               (map (get-text-property 0 'keymap prefix))
               (command (lookup-key map (kbd "RET"))))
          (should (commandp command))
          (call-interactively command)
          (should called))))))

(ert-deftest appkit-discussion-navigation-follows-stable-entry-spans ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (&rest _arguments)
                 '(:header "@ " :first-body "  " :rest-body "  "))))
      (dolist (key '("one" "two" "three"))
        (appkit-discussion-insert-entry
         (appkit-discussion-entry-create :key key :heading key))))
    (goto-char (point-min))
    (should (equal "one" (appkit-discussion-key-at-point)))
    (goto-char (appkit-discussion-next-position))
    (should (equal "two" (appkit-discussion-key-at-point)))
    (forward-line 1)
    (should (equal "one"
                   (save-excursion
                     (goto-char (appkit-discussion-previous-position))
                     (appkit-discussion-key-at-point))))
    (forward-line -1)
    (goto-char (appkit-discussion-next-position))
    (should (equal "three" (appkit-discussion-key-at-point)))
    (goto-char (appkit-discussion-previous-position))
    (should (equal "two" (appkit-discussion-key-at-point)))))

(provide 'appkit-discussion-test)

;;; appkit-discussion-test.el ends here
