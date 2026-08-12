;;; appkit-projection.el --- Read-only stable-key projections -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-independent projection mechanics for generated read-only views.
;; Clients own source state, query semantics, and row rendering.  Appkit owns
;; stable-key reconciliation, presentation dependencies, frame updates, and
;; semantic point and viewport preservation.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-position)
(require 'appkit-transaction)

(cl-defstruct (appkit-projection-row
               (:constructor appkit-projection-row-create))
  "One protocol-independent projected row."
  key
  payload
  context
  dependencies)

(cl-defstruct (appkit-projection--state
               (:constructor appkit-projection--state-create))
  "Projection state owned by one Appkit view."
  ewoc
  node-table
  row-table
  keys
  dependency-index
  anchor-property
  printer
  header
  footer)

(defun appkit-projection-view-p (view)
  "Return non-nil when VIEW owns a read-only projection."
  (and (appkit-view-live-p view)
       (appkit-projection--state-p (appkit-view-engine view))))

(defun appkit-projection--state (view)
  "Return VIEW's initialized projection state."
  (let ((state (appkit-view-engine view)))
    (unless (appkit-projection--state-p state)
      (error "Appkit view has no read-only projection"))
    state))

(defun appkit-projection--current-state ()
  "Return the current buffer's initialized projection state."
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "Appkit projection requires a live view"))
    (appkit-projection--state view)))

(defun appkit-projection--print-row (row)
  "Render projected ROW through the current client printer."
  (let ((printer
         (appkit-projection--state-printer
          (appkit-projection--current-state))))
    (unless (functionp printer)
      (error "Appkit projection has no row printer"))
    (funcall printer row)))

(cl-defun appkit-projection-ensure
    (view &key printer anchor-property header footer)
  "Ensure VIEW owns one read-only projection.

PRINTER renders one `appkit-projection-row'.  ANCHOR-PROPERTY carries stable
row identity in generated text.  HEADER and FOOTER are the initial EWOC frame."
  (unless (appkit-view-live-p view)
    (error "Cannot initialize a dead Appkit view projection"))
  (let ((current (appkit-view-engine view)))
    (if (appkit-projection--state-p current)
        (progn
          (when printer
            (setf (appkit-projection--state-printer current) printer))
          (when anchor-property
            (setf (appkit-projection--state-anchor-property current)
                  anchor-property)))
      (when current
        (error "Appkit view already owns another projection engine"))
      (unless (functionp printer)
        (error "Appkit projection requires a row printer"))
      (let ((state
             (appkit-projection--state-create
              :node-table (make-hash-table :test #'equal)
              :row-table (make-hash-table :test #'equal)
              :keys nil
              :dependency-index (make-hash-table :test #'equal)
              :anchor-property anchor-property
              :printer printer
              :header (or header "")
              :footer (or footer ""))))
        (setf (appkit-view-engine view) state)
        (appkit-with-content-update view
          (erase-buffer)
          (setf (appkit-projection--state-ewoc state)
                (ewoc-create #'appkit-projection--print-row
                             (appkit-projection--state-header state)
                             (appkit-projection--state-footer state)))))))
  (appkit-projection--state-ewoc (appkit-projection--state view)))

(cl-defun appkit-projection-project
    (entries key-function &key context-function dependencies-function)
  "Project ENTRIES into protocol-independent rows using KEY-FUNCTION.

CONTEXT-FUNCTION receives the previous and current entry.  DEPENDENCIES-FUNCTION
receives the current entry and returns opaque presentation dependency keys."
  (let (previous rows)
    (dolist (entry entries (nreverse rows))
      (push
       (appkit-projection-row-create
        :key (funcall key-function entry)
        :payload entry
        :context (and context-function
                      (funcall context-function previous entry))
        :dependencies
        (and dependencies-function
             (delete-dups
              (delq nil
                    (copy-sequence
                     (or (funcall dependencies-function entry) '()))))))
       rows)
      (setq previous entry))))

(defun appkit-projection--validate-rows (rows)
  "Require unique, stable keys on projected ROWS."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (row rows)
      (unless (appkit-projection-row-p row)
        (error "Appkit projection contains an invalid row: %S" row))
      (let ((key (appkit-projection-row-key row)))
        (unless key
          (error "Appkit projection row has no stable key"))
        (when (gethash key seen)
          (error "Appkit projection rows duplicate key %S" key))
        (puthash key t seen)))))

(defun appkit-projection--row-table (rows)
  "Return an equal-tested key table for ROWS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows table)
      (puthash (appkit-projection-row-key row) row table))))

(defun appkit-projection--dependency-index (rows)
  "Return a presentation dependency index for ROWS."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row rows index)
      (let ((row-key (appkit-projection-row-key row)))
        (dolist (dependency (appkit-projection-row-dependencies row))
          (puthash dependency
                   (cons row-key
                         (delete row-key (gethash dependency index)))
                   index))))))

(defun appkit-projection--dependent-keys (index dependencies)
  "Return row keys in INDEX affected by DEPENDENCIES."
  (let (keys)
    (dolist (dependency dependencies)
      (setq keys
            (nconc (copy-sequence (gethash dependency index)) keys)))
    (delete-dups (delq nil keys))))

(defun appkit-projection-dependent-keys (view dependencies)
  "Return VIEW row keys affected by presentation DEPENDENCIES."
  (appkit-projection--dependent-keys
   (appkit-projection--state-dependency-index
    (appkit-projection--state view))
   dependencies))

(defun appkit-projection-keys (view)
  "Return the ordered projected row keys in VIEW."
  (copy-sequence
   (appkit-projection--state-keys (appkit-projection--state view))))

(defun appkit-projection-row (view key)
  "Return VIEW's projected row identified by KEY."
  (gethash key
           (appkit-projection--state-row-table
            (appkit-projection--state view))))

(defun appkit-projection-node (view key)
  "Return VIEW's EWOC node identified by KEY."
  (gethash key
           (appkit-projection--state-node-table
            (appkit-projection--state view))))

(defun appkit-projection--capture-position (state position)
  "Capture position for STATE unless POSITION supplies an explicit target."
  (unless (or (eq position 'first)
              (appkit-position-snapshot-p position))
    (appkit-position-capture
     :anchor-property (appkit-projection--state-anchor-property state)
     :preserve-window-start t)))

(defun appkit-projection--first-row-position (state)
  "Return the first projected row position in STATE, or `point-min'."
  (let ((property (appkit-projection--state-anchor-property state)))
    (or (and property
             (text-property-not-all
              (point-min) (point-max) property nil))
        (point-min))))

(defun appkit-projection--restore-position (state position snapshot)
  "Restore POSITION in STATE, using SNAPSHOT for preservation."
  (pcase position
    ('first
     (goto-char (appkit-projection--first-row-position state)))
    ((pred appkit-position-snapshot-p)
     (appkit-position-restore position))
    ((or 'preserve 'nil)
     (when snapshot
       (appkit-position-restore snapshot)))
    (key
     (when-let* ((property
                  (appkit-projection--state-anchor-property state))
                 (target
                  (appkit-position-find-property-value
                   (point-min) (point-max) property key)))
       (goto-char target)))))

(defun appkit-projection--set-frame (state header footer)
  "Update STATE's EWOC frame to HEADER and FOOTER when changed."
  (let ((new-header (or header ""))
        (new-footer (or footer "")))
    (unless (and (equal new-header
                        (appkit-projection--state-header state))
                 (equal new-footer
                        (appkit-projection--state-footer state)))
      (setf (appkit-projection--state-header state) new-header
            (appkit-projection--state-footer state) new-footer)
      (ewoc-set-hf (appkit-projection--state-ewoc state)
                   new-header new-footer))))

(cl-defun appkit-projection-sync
    (view rows &key header footer force-keys changed-dependencies
          (position 'preserve) (reconcile-p t))
  "Synchronize VIEW with projected ROWS.

HEADER and FOOTER update the generated frame.  FORCE-KEYS redraws retained
rows.  CHANGED-DEPENDENCIES redraws rows that named any changed presentation
dependency before or after this synchronization.  POSITION is `preserve',
`first', a stable row key, or an `appkit-position-snapshot'.  When RECONCILE-P
is nil, update only the frame and position without inspecting ROWS."
  (when reconcile-p
    (appkit-projection--validate-rows rows))
  (let* ((state (appkit-projection--state view))
         (row-table
          (and reconcile-p (appkit-projection--row-table rows)))
         (new-index
          (and reconcile-p (appkit-projection--dependency-index rows)))
         (dependency-keys
          (and
           reconcile-p
           (delete-dups
            (append
             (appkit-projection--dependent-keys
              (appkit-projection--state-dependency-index state)
              changed-dependencies)
             (appkit-projection--dependent-keys
              new-index changed-dependencies)))))
         (effective-force-keys
          (and
           reconcile-p
           (delete-dups
            (delq nil (append (copy-sequence force-keys)
                              dependency-keys)))))
         (snapshot
          (with-current-buffer (appkit-view-buffer view)
            (appkit-projection--capture-position state position))))
    (appkit-with-content-update view
      (appkit-projection--set-frame state header footer)
      (when reconcile-p
        (setf (appkit-projection--state-node-table state)
              (appkit-ewoc-reconcile
               (appkit-projection--state-ewoc state)
               rows #'appkit-projection-row-key
               :force-keys effective-force-keys)
              (appkit-projection--state-row-table state) row-table
              (appkit-projection--state-keys state)
              (mapcar #'appkit-projection-row-key rows)
              (appkit-projection--state-dependency-index state) new-index))
      (appkit-projection--restore-position state position snapshot))
    (appkit-projection-keys view)))

(provide 'appkit-projection)

;;; appkit-projection.el ends here
