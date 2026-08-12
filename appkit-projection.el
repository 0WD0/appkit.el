;;; appkit-projection.el --- Stable-key projection mechanics -*- lexical-binding: t; -*-

;;; Commentary:

;; Protocol-independent projection mechanics shared by generated views.
;; Clients own source state, query semantics, and row rendering.  Appkit owns
;; stable-key reconciliation and presentation dependency indexing.  The
;; direct read-only view adapter additionally owns frame updates and semantic
;; point and viewport preservation.

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

(cl-defstruct (appkit-projection--engine
               (:constructor appkit-projection--engine-create))
  "Stable-key projection state embedded by one presentation surface."
  ewoc
  node-table
  row-table
  keys
  dependency-index
  anchor-property
  printer)

(cl-defstruct (appkit-projection--view-state
               (:constructor appkit-projection--view-state-create))
  "Direct read-only projection state owned by one Appkit view."
  projection
  header
  footer
  no-separator-p)

(defun appkit-projection--print-row (projection row)
  "Render projected ROW through PROJECTION's client printer."
  (let ((printer (appkit-projection--engine-printer projection)))
    (unless (functionp printer)
      (error "Appkit projection has no row printer"))
    (funcall printer row)))

(cl-defun appkit-projection--create
    (printer anchor-property &key header footer no-separator-p)
  "Create an empty projection at point using PRINTER and ANCHOR-PROPERTY.

HEADER and FOOTER seed the projection's EWOC frame.  NO-SEPARATOR-P is passed
to `ewoc-create'.  The caller owns the surrounding buffer mutation transaction."
  (unless (functionp printer)
    (error "Appkit projection requires a row printer"))
  (let ((projection
         (appkit-projection--engine-create
          :node-table (make-hash-table :test #'equal)
          :row-table (make-hash-table :test #'equal)
          :keys nil
          :dependency-index (make-hash-table :test #'equal)
          :anchor-property anchor-property
          :printer printer)))
    (setf (appkit-projection--engine-ewoc projection)
          (ewoc-create
           (apply-partially #'appkit-projection--print-row projection)
           (or header "") (or footer "") no-separator-p))
    projection))

(defun appkit-projection-view-p (view)
  "Return non-nil when VIEW owns a direct read-only projection."
  (and (appkit-view-live-p view)
       (appkit-projection--view-state-p (appkit-view-engine view))))

(defun appkit-projection--view-state (view)
  "Return VIEW's initialized direct projection state."
  (let ((state (appkit-view-engine view)))
    (unless (appkit-projection--view-state-p state)
      (error "Appkit view has no direct read-only projection"))
    state))

(defun appkit-projection--view-engine (view)
  "Return the projection engine installed directly in VIEW."
  (appkit-projection--view-state-projection
   (appkit-projection--view-state view)))

(cl-defun appkit-projection-ensure
    (view &key printer anchor-property header footer
          (no-separator-p nil no-separator-p-supplied-p))
  "Ensure VIEW owns one direct read-only projection.

PRINTER renders one `appkit-projection-row'.  ANCHOR-PROPERTY carries stable
row identity in generated text.  HEADER and FOOTER are the initial EWOC frame.
NO-SEPARATOR-P suppresses EWOC's automatic newlines between rendered regions."
  (unless (appkit-view-live-p view)
    (error "Cannot initialize a dead Appkit view projection"))
  (unless (memq no-separator-p '(nil t))
    (error "Appkit projection no-separator flag must be boolean"))
  (let ((current (appkit-view-engine view)))
    (if (appkit-projection--view-state-p current)
        (let ((projection
               (appkit-projection--view-state-projection current)))
          (when (and no-separator-p-supplied-p
                     (not (eq no-separator-p
                              (appkit-projection--view-state-no-separator-p
                               current))))
            (error "Cannot change an initialized Appkit projection separator"))
          (when printer
            (setf (appkit-projection--engine-printer projection) printer))
          (when anchor-property
            (setf (appkit-projection--engine-anchor-property projection)
                  anchor-property)))
      (when current
        (error "Appkit view already owns another projection engine"))
      (unless (functionp printer)
        (error "Appkit projection requires a row printer"))
      (let ((state
             (appkit-projection--view-state-create
              :header (or header "")
              :footer (or footer "")
              :no-separator-p no-separator-p)))
        (appkit-with-content-update view
          (erase-buffer)
          (setf (appkit-projection--view-state-projection state)
                (appkit-projection--create
                 printer anchor-property
                 :header header :footer footer
                 :no-separator-p no-separator-p)))
        (setf (appkit-view-engine view) state))))
  (appkit-projection--engine-ewoc (appkit-projection--view-engine view)))

(cl-defun appkit-projection-project
    (entries key-function &key context-function dependencies-function
             (row-function #'appkit-projection-row-create))
  "Project ENTRIES into protocol-independent rows using KEY-FUNCTION.

CONTEXT-FUNCTION receives the previous and current entry.  DEPENDENCIES-FUNCTION
receives the current entry and returns opaque presentation dependency keys.
ROW-FUNCTION constructs each row from `:key', `:payload', `:context', and
`:dependencies' keyword arguments."
  (unless (functionp row-function)
    (error "Appkit projection row constructor is not callable"))
  (let (previous rows)
    (dolist (entry entries (nreverse rows))
      (push
       (funcall
        row-function
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

(defun appkit-projection--dependent-keys-in-index (index dependencies)
  "Return row keys in INDEX affected by DEPENDENCIES."
  (let (keys)
    (dolist (dependency dependencies)
      (setq keys
            (nconc (copy-sequence (gethash dependency index)) keys)))
    (delete-dups (delq nil keys))))

(defun appkit-projection--dependent-keys (projection dependencies)
  "Return PROJECTION row keys affected by DEPENDENCIES."
  (appkit-projection--dependent-keys-in-index
   (appkit-projection--engine-dependency-index projection)
   dependencies))

(defun appkit-projection--keys (projection)
  "Return the ordered row keys in PROJECTION."
  (copy-sequence (appkit-projection--engine-keys projection)))

(defun appkit-projection--row (projection key)
  "Return the row identified by KEY in PROJECTION."
  (gethash key (appkit-projection--engine-row-table projection)))

(defun appkit-projection--node (projection key)
  "Return the EWOC node identified by KEY in PROJECTION."
  (gethash key (appkit-projection--engine-node-table projection)))

(defun appkit-projection--invalidate (projection keys)
  "Redraw existing PROJECTION rows identified by KEYS."
  (dolist (key (delete-dups (delq nil (copy-sequence keys))))
    (appkit-ewoc-invalidate-key
     (appkit-projection--engine-ewoc projection)
     (appkit-projection--engine-node-table projection)
     key)))

(defun appkit-projection-dependent-keys (view dependencies)
  "Return VIEW row keys affected by presentation DEPENDENCIES."
  (appkit-projection--dependent-keys
   (appkit-projection--view-engine view) dependencies))

(defun appkit-projection-keys (view)
  "Return the ordered projected row keys in VIEW."
  (appkit-projection--keys (appkit-projection--view-engine view)))

(defun appkit-projection-row (view key)
  "Return VIEW's projected row identified by KEY."
  (appkit-projection--row (appkit-projection--view-engine view) key))

(defun appkit-projection-node (view key)
  "Return VIEW's EWOC node identified by KEY."
  (appkit-projection--node (appkit-projection--view-engine view) key))

(defun appkit-projection--validate-rekeys (projection row-table rekeys)
  "Validate REKEYS against PROJECTION and projected ROW-TABLE."
  (let ((nodes (appkit-projection--engine-node-table projection))
        (targets (make-hash-table :test #'equal)))
    (dolist (mapping rekeys)
      (let ((old-key (car mapping))
            (new-key (cdr mapping)))
        (unless (and old-key new-key (not (equal old-key new-key)))
          (error "Appkit projection has invalid rekey %S" mapping))
        (unless (gethash new-key row-table)
          (error "Appkit projection rekey target %S is not projected" new-key))
        (when (gethash new-key targets)
          (error "Appkit projection has duplicate rekey target %S" new-key))
        (puthash new-key t targets)
        (when (and (gethash old-key nodes)
                   (gethash new-key nodes)
                   (not (eq (gethash old-key nodes)
                            (gethash new-key nodes))))
          (error "Appkit projection rekey target %S already exists" new-key))))))

(defun appkit-projection--apply-rekeys (projection row-table rekeys)
  "Apply validated REKEYS to PROJECTION using projected ROW-TABLE."
  (let ((nodes (appkit-projection--engine-node-table projection)))
    (dolist (mapping rekeys)
      (when-let* ((node (gethash (car mapping) nodes)))
        (ewoc-set-data node (gethash (cdr mapping) row-table))))))

(cl-defun appkit-projection--reconcile
    (projection rows &key force-keys changed-dependencies rekeys)
  "Reconcile PROJECTION with ROWS inside the caller's mutation transaction.

FORCE-KEYS redraws retained rows.  CHANGED-DEPENDENCIES redraws rows that named
those opaque keys before or after reconciliation.  REKEYS promotes old stable
keys to projected replacement keys while preserving EWOC node identity."
  (appkit-projection--validate-rows rows)
  (let* ((row-table (appkit-projection--row-table rows))
         (new-index (appkit-projection--dependency-index rows))
         (dependency-keys
          (delete-dups
           (append
            (appkit-projection--dependent-keys-in-index
             (appkit-projection--engine-dependency-index projection)
             changed-dependencies)
            (appkit-projection--dependent-keys-in-index
             new-index changed-dependencies))))
         (effective-force-keys
          (delete-dups
           (delq nil
                 (append (copy-sequence force-keys)
                         dependency-keys
                         (mapcar #'cdr rekeys))))))
    (appkit-projection--validate-rekeys projection row-table rekeys)
    (appkit-projection--apply-rekeys projection row-table rekeys)
    (setf (appkit-projection--engine-node-table projection)
          (appkit-ewoc-reconcile
           (appkit-projection--engine-ewoc projection)
           rows #'appkit-projection-row-key
           :force-keys effective-force-keys)
          (appkit-projection--engine-row-table projection) row-table
          (appkit-projection--engine-keys projection)
          (mapcar #'appkit-projection-row-key rows)
          (appkit-projection--engine-dependency-index projection) new-index)
    (appkit-projection--keys projection)))

(defun appkit-projection--capture-position (projection position)
  "Capture a position in PROJECTION when POSITION requests preservation."
  (when (or (null position) (eq position 'preserve))
    (appkit-position-capture
     :anchor-property (appkit-projection--engine-anchor-property projection)
     :preserve-window-start t)))

(defun appkit-projection--first-row-position (projection)
  "Return the first projected row position in PROJECTION, or `point-min'."
  (let ((property (appkit-projection--engine-anchor-property projection)))
    (or (and property
             (text-property-not-all
              (point-min) (point-max) property nil))
        (point-min))))

(defun appkit-projection--restore-position (projection position snapshot)
  "Restore POSITION in PROJECTION, using SNAPSHOT for preservation."
  (pcase position
    ('first
     (goto-char (appkit-projection--first-row-position projection)))
    ((pred appkit-position-snapshot-p)
     (appkit-position-restore position))
    ((or 'preserve 'nil)
     (when snapshot
       (appkit-position-restore snapshot)))
    (key
     (when-let* ((property
                  (appkit-projection--engine-anchor-property projection))
                 (target
                  (appkit-position-find-property-value
                   (point-min) (point-max) property key)))
       (goto-char target)))))

(defun appkit-projection--set-frame (state header footer)
  "Update direct projection STATE's frame to HEADER and FOOTER when changed."
  (let ((new-header (or header ""))
        (new-footer (or footer "")))
    (unless (and (equal new-header
                        (appkit-projection--view-state-header state))
                 (equal new-footer
                        (appkit-projection--view-state-footer state)))
      (setf (appkit-projection--view-state-header state) new-header
            (appkit-projection--view-state-footer state) new-footer)
      (ewoc-set-hf
       (appkit-projection--engine-ewoc
        (appkit-projection--view-state-projection state))
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
  (let* ((state (appkit-projection--view-state view))
         (projection (appkit-projection--view-state-projection state))
         (snapshot
          (with-current-buffer (appkit-view-buffer view)
            (appkit-projection--capture-position projection position))))
    (appkit-with-content-update view
      (appkit-projection--set-frame state header footer)
      (when reconcile-p
        (appkit-projection--reconcile
         projection rows
         :force-keys force-keys
         :changed-dependencies changed-dependencies))
      (appkit-projection--restore-position projection position snapshot))
    (appkit-projection--keys projection)))

(provide 'appkit-projection)

;;; appkit-projection.el ends here
