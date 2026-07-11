;;; appkit-invalidation.el --- View-owned invalidation scheduling -*- lexical-binding: t; -*-

;;; Commentary:

;; External events mark stale projections.  Only a view sync function turns a
;; coalesced invalidation snapshot into buffer mutation.

;;; Code:

(require 'cl-lib)
(require 'appkit-core)

(cl-defstruct (appkit-invalidations
               (:constructor appkit-invalidations--create))
  structure-p
  parts
  entry-keys
  resource-keys
  position-p
  syncing-p
  scheduled-handle)

(defun appkit-invalidations-create ()
  "Return a new empty invalidation state."
  (appkit-invalidations--create
   :parts nil :entry-keys nil :resource-keys nil))

(defun appkit-view-invalidations-ensure (view)
  "Return VIEW's invalidation state, creating it when needed."
  (or (appkit-view-invalidations view)
      (setf (appkit-view-invalidations view)
            (appkit-invalidations-create))))

(defun appkit-invalidations-any-p (invalidations)
  "Return non-nil when INVALIDATIONS contains stale projections."
  (and (appkit-invalidations-p invalidations)
       (or (appkit-invalidations-structure-p invalidations)
           (appkit-invalidations-parts invalidations)
           (appkit-invalidations-entry-keys invalidations)
           (appkit-invalidations-resource-keys invalidations)
           (appkit-invalidations-position-p invalidations))))

(defun appkit--invalidation-values (singular plural)
  "Normalize SINGULAR and PLURAL invalidation values into a list."
  (delete-dups
   (delq nil
         (append (and singular (list singular))
                 (cond ((null plural) nil)
                       ((listp plural) plural)
                       (t (list plural)))))))

(cl-defun appkit-invalidate
    (view &key structure part parts entry entries resource resources position)
  "Mark stale projections in VIEW without mutating its buffer."
  (unless (appkit-view-live-p view)
    (cl-return-from appkit-invalidate nil))
  (let* ((state (appkit-view-invalidations-ensure view))
         (part-values (appkit--invalidation-values part parts))
         (entry-values (appkit--invalidation-values entry entries))
         (resource-values (appkit--invalidation-values resource resources)))
    (dolist (value part-values)
      (when (and appkit-strict-boundaries
                 (not (memq value (appkit-view-parts view))))
        (error "Appkit view %S does not declare part %S"
               (appkit-view-id view) value)))
    (when structure (setf (appkit-invalidations-structure-p state) t))
    (when position (setf (appkit-invalidations-position-p state) t))
    (setf (appkit-invalidations-parts state)
          (delete-dups (append part-values
                               (appkit-invalidations-parts state)))
          (appkit-invalidations-entry-keys state)
          (delete-dups (append entry-values
                               (appkit-invalidations-entry-keys state)))
          (appkit-invalidations-resource-keys state)
          (delete-dups (append resource-values
                               (appkit-invalidations-resource-keys state))))
    state))

(defun appkit-invalidations-take (state)
  "Take a snapshot from STATE and clear its pending projections."
  (let ((snapshot
         (appkit-invalidations--create
          :structure-p (appkit-invalidations-structure-p state)
          :parts (appkit-invalidations-parts state)
          :entry-keys (appkit-invalidations-entry-keys state)
          :resource-keys (appkit-invalidations-resource-keys state)
          :position-p (appkit-invalidations-position-p state))))
    (setf (appkit-invalidations-structure-p state) nil
          (appkit-invalidations-parts state) nil
          (appkit-invalidations-entry-keys state) nil
          (appkit-invalidations-resource-keys state) nil
          (appkit-invalidations-position-p state) nil)
    snapshot))

(defun appkit-invalidations-merge (state snapshot)
  "Merge pending invalidation SNAPSHOT back into STATE."
  (setf (appkit-invalidations-structure-p state)
        (or (appkit-invalidations-structure-p state)
            (appkit-invalidations-structure-p snapshot))
        (appkit-invalidations-position-p state)
        (or (appkit-invalidations-position-p state)
            (appkit-invalidations-position-p snapshot))
        (appkit-invalidations-parts state)
        (delete-dups
         (append (appkit-invalidations-parts snapshot)
                 (appkit-invalidations-parts state)))
        (appkit-invalidations-entry-keys state)
        (delete-dups
         (append (appkit-invalidations-entry-keys snapshot)
                 (appkit-invalidations-entry-keys state)))
        (appkit-invalidations-resource-keys state)
        (delete-dups
         (append (appkit-invalidations-resource-keys snapshot)
                 (appkit-invalidations-resource-keys state))))
  state)

(cl-defun appkit-schedule-sync (view &key (delay 0))
  "Schedule one coalesced invalidation sync for VIEW after DELAY seconds."
  (when (appkit-view-live-p view)
    (let ((state (appkit-view-invalidations-ensure view)))
      (unless (and (appkit-handle-p
                    (appkit-invalidations-scheduled-handle state))
                   (appkit-handle-alive-p
                    (appkit-invalidations-scheduled-handle state)))
        (let* ((timer (run-at-time delay nil #'appkit-sync-invalidations view))
               (handle (appkit-register-handle view 'timer timer)))
          (setf (appkit-invalidations-scheduled-handle state) handle)))
      (appkit-handle-object
       (appkit-invalidations-scheduled-handle state)))))

(defun appkit-sync-invalidations (view)
  "Synchronize VIEW from one coalesced invalidation snapshot."
  (when (appkit-view-live-p view)
    (let* ((state (appkit-view-invalidations-ensure view))
           (handle (appkit-invalidations-scheduled-handle state)))
      (setf (appkit-invalidations-scheduled-handle state) nil)
      (when (appkit-handle-p handle) (appkit-cancel-handle handle))
      (if (appkit-invalidations-syncing-p state)
          (appkit-schedule-sync view)
        (when (appkit-invalidations-any-p state)
          (let ((sync (appkit-view-sync-function view)))
            (unless (functionp sync)
              (error "Appkit view %S has no invalidation sync function"
                     (appkit-view-id view)))
            (let ((snapshot (appkit-invalidations-take state))
                  completed-p)
              (setf (appkit-invalidations-syncing-p state) t)
              (unwind-protect
                  (appkit-with-live-view view
                    (funcall sync view snapshot)
                    (setq completed-p t))
                (unless completed-p
                  (appkit-invalidations-merge state snapshot))
                (setf (appkit-invalidations-syncing-p state) nil)
                (when (and completed-p
                           (appkit-invalidations-any-p state))
                  (appkit-schedule-sync view))))))))))

(provide 'appkit-invalidation)

;;; appkit-invalidation.el ends here
