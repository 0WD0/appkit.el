;;; appkit-compose-edit-test.el --- Child compose editor tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-compose-edit)
(require 'appkit-test-helper)

(ert-deftest appkit-compose-edit-buffer-is-owner-bound-and-ephemeral ()
  "A child editor should return validated text and retire its owner handle."
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          editor
          validation-value
          exited)
      (cl-letf
          (((symbol-function 'recursive-edit)
            (lambda ()
              (setq editor (current-buffer))
              (erase-buffer)
              (insert "accepted")
              (cl-letf (((symbol-function 'exit-recursive-edit)
                         (lambda () (setq exited t))))
                (appkit-compose-edit-finish)))))
        (should
         (equal
          "accepted"
          (appkit-compose-edit-buffer
           view "initial"
           :mode #'emacs-lisp-mode
           :validation-function
           (lambda (value) (setq validation-value value))))))
      (should exited)
      (should (equal "accepted" validation-value))
      (should-not (buffer-live-p editor))
      (should-not (appkit-view-handles view)))))

(ert-deftest appkit-compose-edit-buffer-cancel-returns-no-content ()
  "Explicit editor cancellation should leave no buffer or owner handle."
  (appkit-test-with-view
    (let ((view (appkit-current-view))
          editor)
      (cl-letf
          (((symbol-function 'recursive-edit)
            (lambda ()
              (setq editor (current-buffer))
              (cl-letf (((symbol-function 'exit-recursive-edit) #'ignore))
                (appkit-compose-edit-cancel)))))
        (should-not
         (appkit-compose-edit-buffer view "discard me" :mode #'text-mode)))
      (should-not (buffer-live-p editor))
      (should-not (appkit-view-handles view)))))

(provide 'appkit-compose-edit-test)

;;; appkit-compose-edit-test.el ends here
