;;; appkit-directory-test.el --- Tests for directory surfaces -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-directory)

(defmacro appkit-directory-test--with-surface (&rest body)
  "Run BODY in one initialized temporary directory surface."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (appkit-directory-mode)
     (let ((surface (appkit-directory-surface)))
       ,@body)))

(defun appkit-directory-test--item-inserter (_surface entry)
  "Insert synthetic item ENTRY."
  (insert (appkit-directory-entry-label entry) "\n"))

(defun appkit-directory-test--entries (&optional first-expanded-p)
  "Return a section, group, two items, note, and spacer projection."
  (list
   (appkit-directory-entry-create
    :key '(section . "s") :role 'section :label "Scope"
    :foldable-p t :fold-key '(section . "s")
    :fold-default-expanded-p t :expanded-p t)
   (appkit-directory-entry-create
    :key '(group . "g") :role 'group :section-key '(section . "s")
    :label "Group" :indent 2
    :foldable-p t :fold-key '(group . "g")
    :fold-default-expanded-p t :expanded-p (and first-expanded-p t))
   (appkit-directory-entry-create
    :key '(item . "one") :role 'item :section-key '(section . "s")
    :group-key '(group . "g") :label "One" :indent 4
    :item-p t :unread-p t :payload 'one)
   (appkit-directory-entry-create
    :key '(item . "two") :role 'item :section-key '(section . "s")
    :group-key '(group . "g") :label "Two" :indent 4
    :item-p t :payload 'two)
   (appkit-directory-entry-create
    :key 'note :role 'note :label "Note" :indent 2 :face 'shadow)
   (appkit-directory-entry-create :key 'gap :role 'spacer)))

(ert-deftest appkit-directory-renders-flat-roles-with-stable-properties ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :item-inserter #'appkit-directory-test--item-inserter)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (should (equal "▾ Scope\n  ▾ Group\n    One\n    Two\n  Note\n\n"
                   (buffer-string)))
    (goto-char (point-min))
    (search-forward "One")
    (should-not (button-at (1- (point))))
    (should (equal '(item . "one")
                   (appkit-directory-key-at-point (1- (point)))))
    (should (get-text-property
             (1- (point)) appkit-directory-item-property))
    (should (get-text-property
             (1- (point)) appkit-directory-unread-property))))

(ert-deftest appkit-directory-action-row-excludes-the-terminating-newline ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :item-inserter #'appkit-directory-test--item-inserter
     :action-rows-p t)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (goto-char (point-min))
    (search-forward "One")
    (let* ((button (button-at (1- (point))))
           (end (button-end button)))
      (should button)
      (should (equal '(item . "one")
                     (appkit-directory-entry-key
                      (button-get button 'appkit-ui-action-row-object))))
      (should (eq (char-after end) ?\n))
      (should-not (button-at end)))))

(ert-deftest appkit-directory-fold-state-keeps-defaults-and-forced-expansion-distinct ()
  (appkit-directory-test--with-surface
    (let ((key '(category . "stable")))
      (should (appkit-directory-fold-expanded-p surface key t))
      (should-not (appkit-directory-fold-expanded-p surface key nil))
      (appkit-directory-set-fold-expanded surface key nil)
      (should-not (appkit-directory-fold-expanded-p surface key t))
      (should (appkit-directory-fold-expanded-p surface key t t))
      (should-not (appkit-directory-fold-expanded-p surface key t))
      (appkit-directory-clear-fold-state surface key)
      (should (appkit-directory-fold-expanded-p surface key t)))))

(ert-deftest appkit-directory-reconcile-retains-and-targets-ewoc-nodes ()
  (appkit-directory-test--with-surface
    (let ((prints (make-hash-table :test #'equal)))
      (appkit-directory-configure
       surface
       :item-inserter
       (lambda (_surface entry)
         (cl-incf (gethash (appkit-directory-entry-key entry) prints 0))
         (insert (appkit-directory-entry-label entry) "\n")))
      (let* ((entries (appkit-directory-test--entries t))
             (_ (appkit-directory-reconcile surface entries))
             (key '(item . "one"))
             (node (gethash key
                            (appkit-directory-surface-node-table surface))))
        (appkit-directory-reconcile surface (copy-tree entries))
        (should (eq node
                    (gethash key
                             (appkit-directory-surface-node-table surface))))
        (should (= 1 (gethash key prints)))
        (appkit-directory-reconcile
         surface entries :force-keys (list key))
        (should (= 2 (gethash key prints)))))))

(ert-deftest appkit-directory-navigation-and-activation-ignore-passive-rows ()
  (appkit-directory-test--with-surface
    (let (activated folded)
      (appkit-directory-configure
       surface
       :item-inserter #'appkit-directory-test--item-inserter
       :activate-function
       (lambda (_surface entry)
         (setq activated (appkit-directory-entry-payload entry)))
       :fold-function
       (lambda (_surface entry expanded-p)
         (setq folded (list (appkit-directory-entry-fold-key entry)
                            expanded-p))))
      (appkit-directory-reconcile
       surface (appkit-directory-test--entries t))
      (goto-char (point-min))
      (appkit-directory-next-item)
      (should (equal '(item . "one")
                     (appkit-directory-key-at-point)))
      (appkit-directory-activate)
      (should (eq activated 'one))
      (appkit-directory-next-item)
      (should (equal '(item . "two")
                     (appkit-directory-key-at-point)))
      (appkit-directory-previous-item)
      (should (equal '(item . "one")
                     (appkit-directory-key-at-point)))
      (goto-char (point-min))
      (appkit-directory-activate)
      (should (equal folded '((section . "s") nil))))))

(ert-deftest appkit-directory-next-unread-wraps-over-items-only ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface :item-inserter #'appkit-directory-test--item-inserter)
    (appkit-directory-reconcile
     surface (appkit-directory-test--entries t))
    (goto-char (point-max))
    (appkit-directory-next-unread)
    (should (equal '(item . "one")
                   (appkit-directory-key-at-point)))))

(ert-deftest appkit-directory-rolls-back-fold-state-when-adapter-fails ()
  (appkit-directory-test--with-surface
    (appkit-directory-configure
     surface
     :fold-function
     (lambda (&rest _arguments)
       (error "synthetic fold failure")))
    (let ((entry (car (appkit-directory-test--entries t)))
          (key '(section . "s")))
      (should (appkit-directory-fold-expanded-p surface key t))
      (should-error (appkit-directory-activate-entry surface entry))
      (should (appkit-directory-fold-expanded-p surface key t)))))

(ert-deftest appkit-directory-rejects-invalid-or-duplicate-projections ()
  (appkit-directory-test--with-surface
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create :key "x" :role 'tree))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create :key "x" :role 'note)
            (appkit-directory-entry-create :key "x" :role 'note))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create
             :key "group" :role 'group :label "orphan"))))
    (should-error
     (appkit-directory-reconcile
      surface
      (list (appkit-directory-entry-create
             :key "item" :role 'item :item-p t))))))

(provide 'appkit-directory-test)

;;; appkit-directory-test.el ends here
