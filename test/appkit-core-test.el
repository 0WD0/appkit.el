;;; appkit-core-test.el --- Tests for appkit lifecycle -*- lexical-binding: t; -*-

(require 'ert)

(require 'appkit-core)
(require 'appkit-test-helper)

(define-derived-mode appkit-test-mode special-mode "Appkit-Test")

(ert-deftest appkit-open-view-initializes-major-mode-once ()
  (let ((app (appkit-start-app 'appkit-test :id 'app))
        (mode-runs 0)
        first second)
    (unwind-protect
        (cl-letf (((symbol-function 'appkit-test-mode)
                   (lambda ()
                     (setq mode-runs (1+ mode-runs))
                     (special-mode)
                     (setq-local major-mode 'appkit-test-mode))))
          (setq first
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name " *appkit-open-view*"))
          (setq second
                (appkit-open-view
                 :app app :id 'chat :mode 'appkit-test-mode
                 :buffer-name " *appkit-open-view*"))
          (should (eq first second))
          (should (= mode-runs 1)))
      (when (appkit-view-p first) (appkit-kill-view first t))
      (appkit-stop-app app))))

(ert-deftest appkit-view-kill-cancels-owned-handles ()
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          cancelled)
      (appkit-register-handle
       view 'test 'owned (lambda (object) (setq cancelled object)))
      (appkit-kill-view view)
      (should (eq cancelled 'owned))
      (should-not (appkit-view-live-p view)))))

(ert-deftest appkit-major-mode-change-detaches-view ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (fundamental-mode)
      (should-not (appkit-view-live-p view))
      (should-not (appkit-current-view)))))

(ert-deftest appkit-view-event-acknowledgement-preserves-new-tail-events ()
  (appkit-test-with-view
    (let ((view (appkit-current-view)))
      (appkit-view-enqueue-event view 'first)
      (appkit-view-enqueue-event view 'second)
      (let ((snapshot (appkit-view-pending-events-snapshot view)))
        (appkit-view-enqueue-event view 'third)
        (appkit-view-acknowledge-events view (length snapshot)))
      (should (equal (appkit-view-pending-events view) '(third))))))

(ert-deftest appkit-app-events-unsubscribe-on-stop ()
  (let ((app (appkit-start-app 'appkit-test :id 'events))
        seen)
    (appkit-app-on app 'change (lambda (value) (push value seen)))
    (appkit-app-emit app 'change 1)
    (should (equal seen '(1)))
    (appkit-stop-app app)
    (appkit-app-emit app 'change 2)
    (should (equal seen '(1)))))

(provide 'appkit-core-test)

;;; appkit-core-test.el ends here
