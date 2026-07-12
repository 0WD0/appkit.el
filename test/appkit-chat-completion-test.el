;;; appkit-chat-completion-test.el --- Tests for chat completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit-chat-completion)

(ert-deftest appkit-chat-completion-token-bounds-supports-unicode ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "你好 @徐天")
    (should (equal (list :start (- (point) 3)
                         :end (point)
                         :trigger ?@
                         :raw "@徐天"
                         :query "徐天")
                   (appkit-chat-completion-token-bounds ?@)))))

(ert-deftest appkit-chat-completion-token-bounds-rejects-email-address ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "mail@example")
    (should-not (appkit-chat-completion-token-bounds ?@))))

(ert-deftest appkit-chat-completion-token-bounds-keeps-repeated-trigger ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@@admin")
    (let ((token (appkit-chat-completion-token-bounds ?@)))
      (should (equal "@@admin" (plist-get token :raw)))
      (should (equal "@admin" (plist-get token :query))))))

(ert-deftest appkit-chat-completion-capf-affixes-and-replaces ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@gre")
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@GreenKite" :insert "<@1356835185>"
             :prefix "[G] " :annotation " QQ 1356835185"))
           (capf (appkit-chat-completion-capf
                  (- (point) 4) (point) (list candidate) :suffix " "))
           (table (nth 2 capf))
           (affix (plist-get (nthcdr 3 capf) :affixation-function))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (should (equal '("@GreenKite")
                     (all-completions "@g" table)))
      (should (equal '(("@GreenKite" "[G] " " QQ 1356835185"))
                     (funcall affix '("@GreenKite"))))
      (delete-region (- (point) 4) (point))
      (insert "@GreenKite")
      (funcall exit "@GreenKite" 'finished)
      (should (equal "<@1356835185> " (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-only-commits-finished-candidate ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@alice")
    (let* ((candidate (appkit-chat-completion-candidate-create
                       :label "@alice" :insert "<@1>"))
           (capf (appkit-chat-completion-capf
                  (- (point) 6) (point) (list candidate)))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (funcall exit "@alice" 'exact)
      (should (equal "@alice" (appkit-chatbuf-input-string)))
      (funcall exit "@alice" 'finished)
      (should (equal "<@1>" (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-default-commit-syncs-canonical-state ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@old")
    (appkit-chatbuf-input-state-sync)
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@new" :insert "@new")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate :suffix " ")
      (should (equal "@new " (appkit-chatbuf-input-string)))
      (should (equal "@new " (appkit-chatbuf-input-state))))))

(ert-deftest appkit-chat-completion-rolls-back-failed-rich-insertion ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@chosen")
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@chosen")))
      (should-error
       (appkit-chat-completion-apply-candidate
        "@chosen" candidate
        :insert-function (lambda (_candidate)
                           (insert "PART")
                           (error "broken insert"))))
      (should (equal "@chosen" (appkit-chatbuf-input-string)))
      (should (equal "@chosen" (appkit-chatbuf-input-state))))))

(ert-deftest appkit-chat-completion-suffix-does-not-duplicate-following-space ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@old rest")
    (goto-char (+ (appkit-chatbuf-input-start-position) 4))
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "@new" :insert "<@1>")))
      (delete-region (- (point) 4) (point))
      (insert "@new")
      (appkit-chat-completion-apply-candidate
       "@new" candidate :suffix " ")
      (should (equal "<@1> rest" (appkit-chatbuf-input-string))))))

(ert-deftest appkit-chat-completion-decorations-are-lazy ()
  (let* ((calls 0)
        (candidate
         (appkit-chat-completion-candidate-create
          :label "@user"
          :annotation (lambda (_candidate)
                        (cl-incf calls)
                        " details"))))
    (let ((map (appkit-chat-completion--candidate-map (list candidate))))
      (should (= calls 0))
      (should (equal '(("@user" "" " details"))
                     (appkit-chat-completion-affixation '("@user") map)))
      (should (= calls 1)))))

(ert-deftest appkit-chat-completion-searches-candidate-aliases ()
  (let* ((candidate
          (appkit-chat-completion-candidate-create
           :label "@徐天天"
           :search-terms '("GreenKite" "1356835185")))
         (capf (appkit-chat-completion-capf 1 1 (list candidate)))
         (table (nth 2 capf)))
    (should (equal '("@徐天天") (all-completions "@green" table)))
    (should (equal '("@徐天天") (all-completions "@135683" table)))))

(ert-deftest appkit-chat-completion-alias-matching-is-syntax-independent ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@徐天天" :search-terms '("GreenKite")))
           (capf (appkit-chat-completion-capf 1 1 (list candidate)))
           (table (nth 2 capf)))
      (should (equal '("@徐天天") (all-completions "@@green" table))))))

(ert-deftest appkit-chat-completion-alias-respects-case-option ()
  (let* ((appkit-chat-completion-ignore-case nil)
         (candidate
          (appkit-chat-completion-candidate-create
           :label "@user" :search-terms '("GreenKite")))
         (capf (appkit-chat-completion-capf 1 1 (list candidate)))
         (table (nth 2 capf)))
    (should-not (all-completions "@green" table))
    (should (equal '("@user") (all-completions "@Green" table)))))

(ert-deftest appkit-chat-completion-never-deletes-before-input-marker ()
  (with-temp-buffer
    (insert "timeline\n")
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "x")
    (let ((candidate (appkit-chat-completion-candidate-create
                      :label "very-long-label" :insert "bad")))
      (should-not
       (appkit-chat-completion-apply-candidate "very-long-label" candidate))
      (should (string-prefix-p "timeline\n>>> " (buffer-string))))))

(ert-deftest appkit-chat-completion-capf-supports-structured-insertion ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@徐天天")
    (let* ((candidate
            (appkit-chat-completion-candidate-create
             :label "@徐天天" :value '((user-id . "1356835185"))))
           (capf
            (appkit-chat-completion-capf
             (- (point) 4) (point) (list candidate)
             :insert-function
             (lambda (selected)
               (appkit-chatbuf-input-insert
                "@徐天天"
                :object (appkit-chat-completion-candidate-value selected)))))
           (exit (plist-get (nthcdr 3 capf) :exit-function)))
      (funcall exit "@徐天天" 'finished)
      (goto-char (appkit-chatbuf-input-start-position))
      (should (equal '((user-id . "1356835185"))
                     (appkit-chatbuf-input-object-at-point))))))

(ert-deftest appkit-chat-completion-dispatch-stops-after-handler ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert ":wave:")
    (let (calls)
      (setq-local appkit-chat-completion-functions
                  (list (lambda () (push 'first calls) nil)
                        (lambda () (push 'second calls) t)
                        (lambda () (push 'third calls) t)))
      (should (appkit-chat-completion-complete))
      (should (equal '(second first) calls)))))

(provide 'appkit-chat-completion-test)

;;; appkit-chat-completion-test.el ends here
