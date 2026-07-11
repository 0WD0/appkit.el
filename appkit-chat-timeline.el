;;; appkit-chat-timeline.el --- Shared projected chat timeline -*- lexical-binding: t; -*-

;; Author: appkit.el contributors

;;; Commentary:

;; Persistent chat timeline controller shared by disco-room and other clients.
;; Clients project protocol messages into `appkit-chat-timeline-row' values.
;; Every state change uses one keyed reconciliation path: the projection may be
;; rebuilt in full, while EWOC only redraws rows whose payload, render context,
;; or dependency state changed.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-chatbuf)
(require 'appkit-ewoc)
(require 'appkit-position)
(require 'appkit-core)

(cl-defstruct (appkit-chat-timeline-row
               (:constructor appkit-chat-timeline-row-create))
  "One protocol-independent projected chat timeline row."
  key
  payload
  context
  dependencies)

(cl-defstruct (appkit-chat-timeline--state
               (:constructor appkit-chat-timeline--state-create))
  ewoc
  node-table
  row-table
  keys
  dependency-index
  anchor-property
  printer
  after-mutation-function
  mutation-depth
  deferred-keys)

(defun appkit-chat-timeline--view ()
  "Return the live appkit view owning the current timeline."
  (let ((view (appkit-current-view)))
    (or (and (appkit-view-live-p view) view)
        (error "Appkit chat timeline requires a live view"))))

(defun appkit-chat-timeline--current-state ()
  "Return the timeline state owned by the current appkit view, or nil."
  (let ((view (appkit-current-view)))
    (and (appkit-view-live-p view)
         (appkit-chat-timeline--state-p (appkit-view-engine view))
         (appkit-view-engine view))))

(defun appkit-chat-timeline-reset ()
  "Discard projected timeline state in the current buffer."
  (setf (appkit-view-engine (appkit-chat-timeline--view)) nil))

(defun appkit-chat-timeline-live-p ()
  "Return non-nil when the current buffer owns a live timeline."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-chat-timeline--state-ewoc state)))

(defun appkit-chat-timeline-ewoc ()
  "Return the current shared EWOC, or nil before timeline initialization."
  (when-let* ((state (appkit-chat-timeline--current-state)))
    (appkit-chat-timeline--state-ewoc state)))

(defun appkit-chat-timeline-keys ()
  "Return projected row keys in display order."
  (copy-sequence
   (or (when-let* ((state (appkit-chat-timeline--current-state)))
         (appkit-chat-timeline--state-keys state))
       '())))

(defun appkit-chat-timeline-node (key)
  "Return current EWOC node identified by KEY, or nil."
  (when-let* ((state (appkit-chat-timeline--current-state))
              (nodes (appkit-chat-timeline--state-node-table state)))
    (and (hash-table-p nodes) (gethash key nodes))))

(defun appkit-chat-timeline-row (key)
  "Return current projected row identified by KEY, or nil."
  (when-let* ((state (appkit-chat-timeline--current-state))
              (rows (appkit-chat-timeline--state-row-table state)))
    (and (hash-table-p rows) (gethash key rows))))

(defun appkit-chat-timeline-context (key)
  "Return render context belonging to projected row KEY."
  (when-let* ((row (appkit-chat-timeline-row key)))
    (appkit-chat-timeline-row-context row)))

(defun appkit-chat-timeline--require-state ()
  "Return current projected timeline state or signal an invariant error."
  (or (appkit-chat-timeline--current-state)
      (error "Appkit chat timeline has not been initialized")))

(defun appkit-chat-timeline--print-row (row)
  "Render projected ROW through the current client printer."
  (let* ((state (appkit-chat-timeline--require-state))
         (printer (appkit-chat-timeline--state-printer state)))
    (unless (functionp printer)
      (error "Appkit chat timeline has no row printer"))
    (funcall printer row)))

(cl-defun appkit-chat-timeline-ensure
    (&key printer anchor-property header footer after-mutation-function)
  "Ensure the current buffer owns one projected timeline.

PRINTER renders one `appkit-chat-timeline-row'.  ANCHOR-PROPERTY is the text
property used to restore semantic message position.  HEADER and FOOTER seed a
new EWOC.  AFTER-MUTATION-FUNCTION runs after outer structural transactions."
  (let ((current (appkit-chat-timeline--current-state)))
    (if current
      (progn
        (when printer
          (setf (appkit-chat-timeline--state-printer
                 current)
                printer))
        (when anchor-property
          (setf (appkit-chat-timeline--state-anchor-property
                 current)
                anchor-property))
        (setf (appkit-chat-timeline--state-after-mutation-function
               current)
              after-mutation-function))
      (progn
        (unless (functionp printer)
          (error "Appkit chat timeline requires a row printer"))
        (let ((state
               (appkit-chat-timeline--state-create
                :node-table (make-hash-table :test #'equal)
                :row-table (make-hash-table :test #'equal)
                :keys nil
                :dependency-index (make-hash-table :test #'equal)
                :anchor-property anchor-property
                :printer printer
                :after-mutation-function after-mutation-function
                :mutation-depth 0
                :deferred-keys nil)))
          (setf (appkit-view-engine (appkit-chat-timeline--view)) state)
          (appkit-chatbuf-with-generated-update
            (erase-buffer)
            (setf (appkit-chat-timeline--state-ewoc state)
                  (ewoc-create #'appkit-chat-timeline--print-row
                               header footer t))))))
    (appkit-chat-timeline--state-ewoc
     (appkit-chat-timeline--require-state))))

(cl-defun appkit-chat-timeline-project
    (entries key-function &key context-function dependencies-function)
  "Project ENTRIES into protocol-independent timeline rows.

KEY-FUNCTION receives one entry.  CONTEXT-FUNCTION, when non-nil, receives the
previous entry and current entry.  DEPENDENCIES-FUNCTION receives the current
entry and returns opaque resource keys whose changes should redraw the row."
  (let ((previous nil)
        rows)
    (dolist (entry entries (nreverse rows))
      (push (appkit-chat-timeline-row-create
             :key (funcall key-function entry)
             :payload entry
             :context (and context-function
                           (funcall context-function previous entry))
             :dependencies (and dependencies-function
                                (delete-dups
                                 (delq nil
                                       (copy-sequence
                                        (or (funcall dependencies-function entry)
                                            '()))))))
            rows)
      (setq previous entry))))

(defun appkit-chat-timeline--validate-rows (rows)
  "Require ROWS to have unique, non-nil stable keys."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (row rows)
      (unless (appkit-chat-timeline-row-p row)
        (error "Appkit chat timeline projection contains a non-row: %S" row))
      (let ((key (appkit-chat-timeline-row-key row)))
        (unless key
          (error "Appkit chat timeline row has no stable key"))
        (when (gethash key seen)
          (error "Appkit chat timeline has duplicate row key %S" key))
        (puthash key t seen)))))

(defun appkit-chat-timeline--row-table (rows)
  "Return equal-tested key to row table for validated ROWS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (row rows table)
      (puthash (appkit-chat-timeline-row-key row) row table))))

(defun appkit-chat-timeline--dependency-index (rows)
  "Build resource key to projected row key index for ROWS."
  (let ((index (make-hash-table :test #'equal)))
    (dolist (row rows index)
      (let ((row-key (appkit-chat-timeline-row-key row)))
        (dolist (resource-key (appkit-chat-timeline-row-dependencies row))
          (puthash resource-key
                   (cons row-key
                         (delete row-key (gethash resource-key index)))
                   index))))))

(defun appkit-chat-timeline--dependent-keys-in-index (index resource-keys)
  "Return row keys in INDEX depending on RESOURCE-KEYS."
  (let (keys)
    (when (hash-table-p index)
      (dolist (resource-key resource-keys)
        (setq keys (nconc (copy-sequence (gethash resource-key index)) keys))))
    (delete-dups (delq nil keys))))

(defun appkit-chat-timeline-dependent-keys (resource-keys)
  "Return current row keys depending on any of RESOURCE-KEYS."
  (let ((state (appkit-chat-timeline--require-state)))
    (appkit-chat-timeline--dependent-keys-in-index
     (appkit-chat-timeline--state-dependency-index state)
     resource-keys)))

(defun appkit-chat-timeline--footer-region-bounds ()
  "Return current EWOC footer bounds before the prompt, or nil."
  (when-let* ((ewoc (appkit-chat-timeline-ewoc))
              (start (ewoc-location (ewoc--footer ewoc))))
    (let ((end (or (appkit-chatbuf-prompt-start-position)
                   (appkit-chatbuf-input-start-position)
                   (point-max))))
      (when (<= start end)
        (cons start end)))))

(defun appkit-chat-timeline-footer-start-position ()
  "Return the current EWOC footer start position, or nil."
  (car-safe (appkit-chat-timeline--footer-region-bounds)))

(defun appkit-chat-timeline--position-zone-state (position preserve-window-start)
  "Capture semantic state for POSITION in the current timeline.

PRESERVE-WINDOW-START is forwarded for message-zone snapshots."
  (let ((position (min (point-max) (max (point-min) position))))
    (cond
     ((appkit-chatbuf-point-in-input-p position)
      (list :zone 'input
            :offset (- position (appkit-chatbuf-input-start-position))))
     ((appkit-chatbuf-point-in-prompt-p position)
      (list :zone 'prompt
            :offset (- position (appkit-chatbuf-prompt-start-position))))
     ((when-let* ((bounds (appkit-chat-timeline--footer-region-bounds)))
        (and (<= (car bounds) position)
             (<= position (cdr bounds))))
      (list :zone 'footer
            :offset (- position
                       (car (appkit-chat-timeline--footer-region-bounds)))))
     (t
      (save-excursion
        (goto-char position)
        (list :zone 'message
              :snapshot
              (appkit-position-capture
               :anchor-property
               (appkit-chat-timeline--state-anchor-property
                (appkit-chat-timeline--require-state))
               :preserve-window-start preserve-window-start)))))))

(defun appkit-chat-timeline--restore-zone-state (position-state rekeys)
  "Restore POSITION-STATE, remapping semantic anchors through REKEYS."
  (pcase (plist-get position-state :zone)
    ('input
     (when-let* ((start (appkit-chatbuf-input-start-position))
                 (end (appkit-chatbuf-input-logical-end-position)))
       (goto-char (min end
                       (max start
                            (+ start (or (plist-get position-state :offset) 0)))))))
    ('prompt
     (when-let* ((start (appkit-chatbuf-prompt-start-position))
                 (end (appkit-chatbuf-input-start-position)))
       (goto-char (min (max start (1- end))
                       (+ start (or (plist-get position-state :offset) 0))))))
    ('footer
     (when-let* ((bounds (appkit-chat-timeline--footer-region-bounds)))
       (goto-char (min (cdr bounds)
                       (+ (car bounds)
                          (or (plist-get position-state :offset) 0))))))
    (_
     (when-let* ((snapshot (plist-get position-state :snapshot)))
       (appkit-position-restore snapshot rekeys))))
  (point))

(cl-defun appkit-chat-timeline-run-preserving-position (mutator &key rekeys)
  "Run MUTATOR as one undo-free chat timeline transaction.

Point, active mark, viewport, footer position, composer position, and window
points inside the composer are restored afterwards.  REKEYS is an alist mapping
old semantic row keys to new keys."
  (let ((state (appkit-chat-timeline--require-state)))
    (if (> (or (appkit-chat-timeline--state-mutation-depth state) 0) 0)
        (funcall mutator)
      (let* ((window-input-offsets
              (appkit-chatbuf-capture-window-input-offsets))
             (point-state
              (appkit-chat-timeline--position-zone-state (point) t))
             (mark-state
              (and mark-active
                   (appkit-chat-timeline--position-zone-state (mark t) nil))))
        (setf (appkit-chat-timeline--state-mutation-depth state) 1)
        (unwind-protect
            (appkit-chatbuf-with-generated-update
              (funcall mutator))
          (setf (appkit-chat-timeline--state-mutation-depth state) 0)
          (appkit-chat-timeline--restore-zone-state point-state rekeys)
          (appkit-chatbuf-restore-window-input-offsets window-input-offsets)
          (if mark-state
              (let ((mark-position
                     (save-excursion
                       (appkit-chat-timeline--restore-zone-state mark-state rekeys)
                       (point))))
                (set-marker (mark-marker) mark-position)
                (setq mark-active t
                      deactivate-mark nil))
            (setq mark-active nil
                  deactivate-mark t))
          (when-let* ((after-mutation
                       (appkit-chat-timeline--state-after-mutation-function state)))
            (funcall after-mutation)))))))

(cl-defun appkit-chat-timeline-set-frame
    (header footer &key bind-input-function)
  "Set timeline HEADER and FOOTER and then call BIND-INPUT-FUNCTION."
  (let* ((state (appkit-chat-timeline--require-state))
         (ewoc (appkit-chat-timeline--state-ewoc state)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (appkit-chatbuf-clear-prompt-and-input)
       (ewoc-set-hf ewoc header footer)
       (when (functionp bind-input-function)
         (funcall bind-input-function))))))

(defun appkit-chat-timeline--validate-rekeys (state row-table rekeys)
  "Validate REKEYS against STATE and projected ROW-TABLE."
  (let ((nodes (appkit-chat-timeline--state-node-table state))
        (targets (make-hash-table :test #'equal)))
    (dolist (mapping rekeys)
      (let ((old-key (car mapping))
            (new-key (cdr mapping)))
        (unless (and old-key new-key (not (equal old-key new-key)))
          (error "Appkit chat timeline has invalid rekey %S" mapping))
        (unless (gethash new-key row-table)
          (error "Appkit chat timeline rekey target %S is not projected" new-key))
        (when (gethash new-key targets)
          (error "Appkit chat timeline has duplicate rekey target %S" new-key))
        (puthash new-key t targets)
        (when (and (gethash old-key nodes)
                   (gethash new-key nodes)
                   (not (eq (gethash old-key nodes) (gethash new-key nodes))))
          (error "Appkit chat timeline rekey target %S already exists" new-key))))))

(defun appkit-chat-timeline--apply-rekeys (state row-table rekeys)
  "Apply validated REKEYS to live nodes in STATE using projected ROW-TABLE."
  (let ((nodes (appkit-chat-timeline--state-node-table state)))
    (dolist (mapping rekeys)
      (let* ((old-key (car mapping))
             (new-key (cdr mapping))
             (node (gethash old-key nodes))
             (row (gethash new-key row-table)))
        (when node
          (ewoc-set-data node row))))))

(cl-defun appkit-chat-timeline-sync
    (rows &key force-keys changed-resources rekeys)
  "Synchronize the live timeline with projected ROWS.

FORCE-KEYS redraws presentation-only changes.  CHANGED-RESOURCES redraws rows
whose dependency lists mention those opaque resource keys.  REKEYS maps old
row keys to newly projected keys while preserving node and cursor identity."
  (appkit-chat-timeline--validate-rows rows)
  (let* ((state (appkit-chat-timeline--require-state))
         (ewoc (appkit-chat-timeline--state-ewoc state))
         (row-table (appkit-chat-timeline--row-table rows))
         (new-dependency-index
          (appkit-chat-timeline--dependency-index rows))
         (dependency-force-keys
          (delete-dups
           (append
            (appkit-chat-timeline--dependent-keys-in-index
             (appkit-chat-timeline--state-dependency-index state)
             changed-resources)
            (appkit-chat-timeline--dependent-keys-in-index
             new-dependency-index changed-resources))))
         (rekey-targets (mapcar #'cdr rekeys))
         (effective-force-keys
          (delete-dups
           (delq nil
                 (append (copy-sequence force-keys)
                         dependency-force-keys
                         rekey-targets)))))
    (appkit-chat-timeline--validate-rekeys state row-table rekeys)
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (appkit-chat-timeline--apply-rekeys state row-table rekeys)
       (setf (appkit-chat-timeline--state-node-table state)
             (appkit-ewoc-reconcile
              ewoc rows #'appkit-chat-timeline-row-key
              :force-keys effective-force-keys)
             (appkit-chat-timeline--state-row-table state) row-table
             (appkit-chat-timeline--state-keys state)
             (mapcar #'appkit-chat-timeline-row-key rows)
             (appkit-chat-timeline--state-dependency-index state)
             new-dependency-index))
     :rekeys rekeys)
    (appkit-chat-timeline-keys)))

(cl-defun appkit-chat-timeline-invalidate (keys &key defer-while-mark-active)
  "Redraw existing rows identified by KEYS.

When DEFER-WHILE-MARK-ACTIVE is non-nil, queue keys until
`appkit-chat-timeline-flush-deferred' is called with no active region."
  (let* ((state (appkit-chat-timeline--require-state))
         (keys (delete-dups (delq nil (copy-sequence keys)))))
    (cond
     ((null keys) nil)
     ((and defer-while-mark-active mark-active)
      (setf (appkit-chat-timeline--state-deferred-keys state)
            (delete-dups
             (append keys
                     (appkit-chat-timeline--state-deferred-keys state))))
      'deferred)
     (t
      (appkit-chat-timeline-run-preserving-position
       (lambda ()
         (dolist (key keys)
           (appkit-ewoc-invalidate-key
            (appkit-chat-timeline--state-ewoc state)
            (appkit-chat-timeline--state-node-table state)
            key))))
      t))))

(defun appkit-chat-timeline-flush-deferred ()
  "Redraw deferred keys when no region is active."
  (let ((state (appkit-chat-timeline--require-state)))
    (when (and (not mark-active)
               (appkit-chat-timeline--state-deferred-keys state))
      (let ((keys (prog1 (appkit-chat-timeline--state-deferred-keys state)
                    (setf (appkit-chat-timeline--state-deferred-keys state) nil))))
        (appkit-chat-timeline-invalidate keys)))))

(defun appkit-chat-timeline-refresh ()
  "Refresh all projected rows while preserving chat position."
  (let ((state (appkit-chat-timeline--require-state)))
    (appkit-chat-timeline-run-preserving-position
     (lambda ()
       (ewoc-refresh (appkit-chat-timeline--state-ewoc state))))))

(defun appkit-chat-timeline-key-at-point (&optional position)
  "Return semantic timeline key at POSITION or point."
  (let* ((position (or position (point)))
         (property
          (appkit-chat-timeline--state-anchor-property
           (appkit-chat-timeline--require-state))))
    (and property
         (or (get-text-property position property)
             (save-excursion
               (goto-char position)
               (get-text-property (line-beginning-position) property))))))

(defun appkit-chat-timeline-key-position (key)
  "Return first buffer position carrying semantic row KEY, or nil."
  (let ((property
         (appkit-chat-timeline--state-anchor-property
          (appkit-chat-timeline--require-state))))
    (and property
         (appkit-position-find-property-value
          (point-min) (point-max) property key))))

(provide 'appkit-chat-timeline)

;;; appkit-chat-timeline.el ends here
