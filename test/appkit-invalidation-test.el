;;; appkit-invalidation-test.el --- Tests for appkit invalidation -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-invalidation)
(require 'appkit-test-helper)

(ert-deftest appkit-invalidations-coalesce-before-sync ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           snapshots)
      (setf (appkit-view-parts view) '(footer status)
            (appkit-view-sync-function view)
            (lambda (_view snapshot) (push snapshot snapshots)))
      (appkit-invalidate view :part 'footer :entry "m1")
      (appkit-invalidate view :part 'footer :entry "m2" :resource '(:avatar "u"))
      (appkit-sync-invalidations view)
      (should (= (length snapshots) 1))
      (let ((snapshot (car snapshots)))
        (should (equal (appkit-invalidations-parts snapshot) '(footer)))
        (should (equal (sort (copy-sequence
                              (appkit-invalidations-entry-keys snapshot))
                             #'string<)
                       '("m1" "m2")))
        (should (equal (appkit-invalidations-resource-keys snapshot)
                       '((:avatar "u"))))))))

(ert-deftest appkit-invalidations-produced-during-sync-run-next-cycle ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (runs 0))
      (setf (appkit-view-sync-function view)
            (lambda (candidate _snapshot)
              (setq runs (1+ runs))
              (when (= runs 1)
                (appkit-invalidate candidate :entry "second"))))
      (appkit-invalidate view :entry "first")
      (appkit-sync-invalidations view)
      (should (= runs 1))
      (appkit-sync-invalidations view)
      (should (= runs 2)))))

(ert-deftest appkit-dead-view-discards-invalidations ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (appkit-kill-view view)
      (should-not (appkit-invalidate view :structure t)))))

(ert-deftest appkit-scheduled-sync-is-coalesced-and-forgets-timer-handle ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           (runs 0))
      (setf (appkit-view-sync-function view)
            (lambda (_view _snapshot) (setq runs (1+ runs))))
      (appkit-invalidate view :entry "m1")
      (let ((first (appkit-schedule-sync view :delay 60))
            (second (appkit-schedule-sync view :delay 60)))
        (should (eq first second))
        (should (= 1 (length (appkit-view-handles view))))
        (appkit-sync-invalidations view)
        (should (= runs 1))
        (should-not (appkit-view-handles view))))))

(ert-deftest appkit-request-sync-invalidates-and-coalesces-atomically ()
  (appkit-test-with-view
    (let* ((view (appkit-current-view))
           snapshots)
      (setf (appkit-view-parts view) '(entries header)
            (appkit-view-sync-function view)
            (lambda (_view snapshot) (push snapshot snapshots)))
      (let ((first
             (appkit-request-sync
              view :part 'entries :entry "m1" :delay 60))
            (second
             (appkit-request-sync
              view :part 'header :entry "m2" :position t :delay 60)))
        (should (eq first second))
        (appkit-sync-invalidations view)
        (should (= 1 (length snapshots)))
        (let ((snapshot (car snapshots)))
          (should (equal (sort (copy-sequence
                                (appkit-invalidations-parts snapshot))
                               (lambda (left right)
                                 (string-lessp (symbol-name left)
                                               (symbol-name right))))
                         '(entries header)))
          (should (equal (sort (copy-sequence
                                (appkit-invalidations-entry-keys snapshot))
                               #'string<)
                         '("m1" "m2")))
          (should (appkit-invalidations-position-p snapshot)))))))

(ert-deftest appkit-request-sync-is-inert-for-dead-view ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (appkit-kill-view view)
      (should-not (appkit-request-sync view :structure t)))))

(ert-deftest appkit-request-sync-does-not-schedule-empty-work ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (should-not (appkit-request-sync view))
      (should-not (appkit-view-handles view)))))

(ert-deftest appkit-failed-sync-restores-invalidation-snapshot ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (setf (appkit-view-sync-function view)
            (lambda (_view _snapshot) (error "broken projection")))
      (appkit-invalidate view :entry "m1" :resource '(:avatar "u"))
      (should-error (appkit-sync-invalidations view))
      (let ((pending (appkit-view-invalidations view)))
        (should (equal (appkit-invalidations-entry-keys pending) '("m1")))
        (should (equal (appkit-invalidations-resource-keys pending)
                       '((:avatar "u"))))))))

(provide 'appkit-invalidation-test)

;;; appkit-invalidation-test.el ends here
