;;; appkit-projection-test.el --- Tests for read-only projections -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-projection)
(require 'appkit-test-helper)

(defun appkit-projection-test--printer (prints)
  "Return a row printer recording render counts in PRINTS."
  (lambda (row)
    (let* ((key (appkit-projection-row-key row))
           (payload (appkit-projection-row-payload row))
           (context (appkit-projection-row-context row))
           (start (point)))
      (puthash key (1+ (gethash key prints 0)) prints)
      (insert (format "%s:%s:%s\n"
                      key payload (or (plist-get context :layout) "plain")))
      (add-text-properties start (point) (list 'test-projection-key key)))))

(defun appkit-projection-test--row
    (key payload &optional context dependencies)
  "Create one test row from KEY, PAYLOAD, CONTEXT, and DEPENDENCIES."
  (appkit-projection-row-create
   :key key
   :payload payload
   :context context
   :dependencies dependencies))

(ert-deftest appkit-projection-projects-context-and-dependencies ()
  (let ((rows
         (appkit-projection-project
          '((a . "one") (b . "two"))
          #'car
          :context-function
          (lambda (previous current)
            (list :previous (car-safe previous)
                  :current (car current)))
          :dependencies-function
          (lambda (entry)
            (list (list :source (cdr entry)))))))
    (should (equal '(a b) (mapcar #'appkit-projection-row-key rows)))
    (should (equal '(:previous a :current b)
                   (appkit-projection-row-context (cadr rows))))
    (should (equal '((:source "two"))
                   (appkit-projection-row-dependencies (cadr rows))))))

(ert-deftest appkit-projection-sync-is-keyed-and-context-sensitive ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          (prints (make-hash-table :test #'equal)))
      (appkit-projection-ensure
       view
       :printer (appkit-projection-test--printer prints)
       :anchor-property 'test-projection-key)
      (appkit-projection-sync
       view
       (list (appkit-projection-test--row 'a "A")
             (appkit-projection-test--row 'b "B")))
      (let ((a-node (appkit-projection-node view 'a))
            (b-node (appkit-projection-node view 'b)))
        (appkit-projection-sync
         view
         (list (appkit-projection-test--row 'a "A")
               (appkit-projection-test--row 'b "B" '(:layout compact))))
        (should (eq a-node (appkit-projection-node view 'a)))
        (should (eq b-node (appkit-projection-node view 'b)))
        (should (= 1 (gethash 'a prints)))
        (should (= 2 (gethash 'b prints)))
        (should (equal '(a b) (appkit-projection-keys view)))))))

(ert-deftest appkit-projection-redraws-changed-dependency-rows ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          (prints (make-hash-table :test #'equal))
          (resource '(:image "avatar")))
      (appkit-projection-ensure
       view
       :printer (appkit-projection-test--printer prints)
       :anchor-property 'test-projection-key)
      (let ((rows
             (list
              (appkit-projection-test--row 'a "A")
              (appkit-projection-test--row 'b "B" nil (list resource)))))
        (appkit-projection-sync view rows)
        (let ((a-node (appkit-projection-node view 'a))
              (b-node (appkit-projection-node view 'b)))
          (appkit-projection-sync
           view rows :changed-dependencies (list resource))
          (should (eq a-node (appkit-projection-node view 'a)))
          (should (eq b-node (appkit-projection-node view 'b)))
          (should (= 1 (gethash 'a prints)))
          (should (= 2 (gethash 'b prints)))
          (should (equal '(b)
                         (appkit-projection-dependent-keys
                          view (list resource)))))))))

(ert-deftest appkit-projection-updates-frame-and-moves-to-first-row ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          (prints (make-hash-table :test #'equal)))
      (appkit-projection-ensure
       view
       :printer (appkit-projection-test--printer prints)
       :anchor-property 'test-projection-key
       :header "Loading\n")
      (goto-char (point-min))
      (appkit-projection-sync
       view
       (list (appkit-projection-test--row 'a "A"))
       :header "Ready\n"
       :position 'first)
      (should (eq 'a (get-text-property (point) 'test-projection-key)))
      (should (string-prefix-p "Ready\n" (buffer-string)))
      (appkit-projection-sync
       view nil :header "Settled\n" :reconcile-p nil)
      (should (= 1 (gethash 'a prints)))
      (should (equal '(a) (appkit-projection-keys view)))
      (should (string-prefix-p "Settled\n" (buffer-string))))))

(ert-deftest appkit-projection-preserves-each-window-position ()
  (save-window-excursion
    (appkit-test-with-view
      (delete-other-windows)
      (let* ((view (appkit-current-view))
             (buffer (current-buffer))
             (prints (make-hash-table :test #'equal))
             (first-window (selected-window))
             (second-window (split-window first-window nil 'right)))
        (set-window-buffer first-window buffer)
        (set-window-buffer second-window buffer)
        (appkit-projection-ensure
         view
         :printer (appkit-projection-test--printer prints)
         :anchor-property 'test-projection-key)
        (appkit-projection-sync
         view
         (list (appkit-projection-test--row 'a "A")
               (appkit-projection-test--row 'b "B")
               (appkit-projection-test--row 'c "C")
               (appkit-projection-test--row 'd "D")))
        (let ((b-position
               (appkit-position-find-property-value
                (point-min) (point-max) 'test-projection-key 'b))
              (c-position
               (appkit-position-find-property-value
                (point-min) (point-max) 'test-projection-key 'c))
              (d-position
               (appkit-position-find-property-value
                (point-min) (point-max) 'test-projection-key 'd)))
          (goto-char c-position)
          (set-window-start first-window b-position 'noforce)
          (set-window-point second-window d-position)
          (set-window-start second-window c-position 'noforce))
        (appkit-projection-sync
         view
         (list (appkit-projection-test--row 'prefix "New")
               (appkit-projection-test--row 'a "A")
               (appkit-projection-test--row 'b "B")
               (appkit-projection-test--row 'c "C")
               (appkit-projection-test--row 'd "D")))
        (should (eq 'c (get-text-property
                        (window-point first-window) 'test-projection-key)))
        (should (eq 'b (get-text-property
                        (window-start first-window) 'test-projection-key)))
        (should (eq 'd (get-text-property
                        (window-point second-window) 'test-projection-key)))
        (should (eq 'c (get-text-property
                        (window-start second-window) 'test-projection-key)))))))

(ert-deftest appkit-projection-rejects-duplicate-row-keys ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          (prints (make-hash-table :test #'equal)))
      (appkit-projection-ensure
       view
       :printer (appkit-projection-test--printer prints))
      (should-error
       (appkit-projection-sync
        view
        (list (appkit-projection-test--row 'same "A")
              (appkit-projection-test--row 'same "B")))))))

(provide 'appkit-projection-test)

;;; appkit-projection-test.el ends here
