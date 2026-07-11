;;; appkit-transaction.el --- Buffer mutation boundaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Separate generated content mutation, display-property mutation, editable
;; user mutation, and explicitly audited raw mutation.

;;; Code:

(require 'appkit-core)

(defmacro appkit-with-content-update (view &rest body)
  "Run BODY as an undo-free generated content update for VIEW."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-view ,view))
     (unless (appkit-view-live-p appkit-transaction-view)
       (error "Cannot mutate a dead appkit view"))
     (with-current-buffer (appkit-view-buffer appkit-transaction-view)
       (let ((inhibit-read-only t)
             (buffer-undo-list t)
             (appkit-old-modified-p (buffer-modified-p)))
         (unwind-protect
             (progn ,@body)
           (set-buffer-modified-p appkit-old-modified-p))))))

(defmacro appkit-with-property-update (view &rest body)
  "Run BODY as a property-only update for VIEW."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-view ,view))
     (unless (appkit-view-live-p appkit-transaction-view)
       (error "Cannot patch properties in a dead appkit view"))
     (with-current-buffer (appkit-view-buffer appkit-transaction-view)
       (let ((appkit-old-size (buffer-size))
             (appkit-old-tick (buffer-chars-modified-tick)))
         (prog1
             (with-silent-modifications ,@body)
           (when (and appkit-strict-boundaries
                      (or (/= appkit-old-size (buffer-size))
                          (/= appkit-old-tick
                              (buffer-chars-modified-tick))))
             (error "Appkit property transaction changed buffer text")))))))

(defmacro appkit-with-edit-transaction (view &rest body)
  "Run BODY as an ordinary undoable edit in VIEW."
  (declare (indent 1) (debug t))
  `(let ((appkit-transaction-view ,view))
     (unless (appkit-view-live-p appkit-transaction-view)
       (error "Cannot edit a dead appkit view"))
     (with-current-buffer (appkit-view-buffer appkit-transaction-view)
       (atomic-change-group ,@body))))

(defmacro appkit-with-raw-buffer-mutation (view reason &rest body)
  "Run BODY as an audited raw mutation for VIEW, recording REASON in debug."
  (declare (indent 2) (debug t))
  `(let ((appkit-transaction-view ,view)
         (appkit-raw-reason ,reason))
     (unless (appkit-view-live-p appkit-transaction-view)
       (error "Cannot raw-mutate a dead appkit view"))
     (when appkit-debug
       (message "appkit: raw buffer mutation (%s)" appkit-raw-reason))
     (with-current-buffer (appkit-view-buffer appkit-transaction-view)
       (let ((inhibit-read-only t)) ,@body))))

(provide 'appkit-transaction)

;;; appkit-transaction.el ends here
