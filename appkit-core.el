;;; appkit-core.el --- App and view lifecycle runtime -*- lexical-binding: t; -*-

;;; Commentary:

;; Runtime ownership for app sessions, buffer views, lifecycle handles, and a
;; small app-local event bus.  Major modes remain initialization functions;
;; attaching or reusing a view never invokes a live buffer's mode again.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup appkit nil
  "Runtime primitives for stateful Emacs buffer applications."
  :group 'applications)

(defcustom appkit-debug nil
  "When non-nil, validate runtime ownership and mutation invariants."
  :type 'boolean
  :group 'appkit)

(defcustom appkit-strict-boundaries nil
  "When non-nil, signal errors for invalid runtime boundaries."
  :type 'boolean
  :group 'appkit)

(cl-defstruct (appkit-handle
               (:constructor appkit-handle--create))
  type
  object
  cancel
  owner
  alive-p)

(cl-defstruct (appkit-app
               (:constructor appkit-app--create))
  id
  kind
  state
  transport
  event-bus
  resource-store
  request-table
  view-registry
  handles
  shutdown
  alive-p)

(cl-defstruct (appkit-view
               (:constructor appkit-view--create))
  id
  app
  buffer
  mode
  state
  engine
  entry-index
  resource-index
  request-table
  invalidations
  pending-events
  handles
  position-policy
  sync-function
  parts
  update-depth
  alive-p)

(defvar appkit--app-kinds (make-hash-table :test #'eq)
  "Registered app kind definitions keyed by symbol.")

(defvar-local appkit--current-view nil
  "Appkit view attached to the current buffer.")

(defun appkit-register-app-kind (kind options)
  "Register app KIND with evaluated OPTIONS and return KIND."
  (unless (symbolp kind)
    (error "Appkit app kind must be a symbol: %S" kind))
  (puthash kind (copy-sequence options) appkit--app-kinds)
  kind)

(defmacro appkit-define-app-kind (kind &rest options)
  "Define app KIND using evaluated OPTIONS."
  (declare (indent 1))
  `(appkit-register-app-kind ',kind (list ,@options)))

(defun appkit-app-kind-options (kind)
  "Return registered options for app KIND, or nil."
  (gethash kind appkit--app-kinds))

(defun appkit-app-kind-registered-p (kind)
  "Return non-nil when app KIND has been registered."
  (let ((missing (make-symbol "missing")))
    (not (eq (gethash kind appkit--app-kinds missing) missing))))

(cl-defun appkit-start-app (kind &key id state transport shutdown)
  "Create and return a live app session of KIND.

ID identifies one concrete account or backend session.  STATE and TRANSPORT
  remain application-owned values.  SHUTDOWN overrides the app-kind callback."
  (let ((kind-options (appkit-app-kind-options kind)))
    (unless (appkit-app-kind-registered-p kind)
      (error "Appkit app kind is not registered: %S" kind))
    (appkit-app--create
     :id id
     :kind kind
     :state state
     :transport transport
     :event-bus (make-hash-table :test #'equal)
     :resource-store (make-hash-table :test #'equal)
     :request-table (make-hash-table :test #'equal)
     :view-registry (make-hash-table :test #'equal)
     :handles nil
     :shutdown (or shutdown (plist-get kind-options :shutdown))
     :alive-p t)))

(defun appkit-app-live-p (app)
  "Return non-nil when APP is a live app session."
  (and (appkit-app-p app) (appkit-app-alive-p app)))

(defun appkit-current-view ()
  "Return the appkit view attached to the current buffer, or nil."
  appkit--current-view)

(defun appkit-view-live-p (view)
  "Return non-nil when VIEW and its owner buffer/app are live."
  (and (appkit-view-p view)
       (appkit-view-alive-p view)
       (buffer-live-p (appkit-view-buffer view))
       (appkit-app-live-p (appkit-view-app view))))

(defun appkit--owner-handles (owner)
  "Return lifecycle handle list belonging to OWNER."
  (cond
   ((appkit-app-p owner) (appkit-app-handles owner))
   ((appkit-view-p owner) (appkit-view-handles owner))
   (t (error "Appkit handle owner is invalid: %S" owner))))

(defun appkit--set-owner-handles (owner handles)
  "Set OWNER lifecycle HANDLES."
  (cond
   ((appkit-app-p owner) (setf (appkit-app-handles owner) handles))
   ((appkit-view-p owner) (setf (appkit-view-handles owner) handles))
   (t (error "Appkit handle owner is invalid: %S" owner))))

(defun appkit--default-handle-cancel (type object)
  "Cancel lifecycle OBJECT according to TYPE."
  (pcase type
    ('timer
     (when (timerp object) (cancel-timer object)))
    ('process
     (when (processp object)
       (set-process-filter object nil)
       (set-process-sentinel object nil)
       (when (process-live-p object) (delete-process object))))
    ('hook
     (pcase-let ((`(,hook ,function ,local ,buffer) object))
       (if (buffer-live-p buffer)
           (with-current-buffer buffer
             (remove-hook hook function local))
         (remove-hook hook function local))))
    ('event-subscription
     (when (functionp object) (funcall object)))
    ('function
     (when (functionp object) (funcall object)))))

(cl-defun appkit-register-handle (owner type object &optional cancel-function)
  "Register lifecycle OBJECT of TYPE under OWNER.

CANCEL-FUNCTION, when non-nil, receives OBJECT."
  (unless (or (appkit-app-live-p owner) (appkit-view-live-p owner))
    (error "Cannot register handle under dead appkit owner"))
  (let ((handle (appkit-handle--create
                 :type type
                 :object object
                 :cancel cancel-function
                 :owner owner
                 :alive-p t)))
    (appkit--set-owner-handles owner
                               (cons handle (appkit--owner-handles owner)))
    handle))

(defun appkit-cancel-handle (handle)
  "Cancel HANDLE exactly once."
  (when (and (appkit-handle-p handle) (appkit-handle-alive-p handle))
    (setf (appkit-handle-alive-p handle) nil)
    (unwind-protect
        (let ((cancel (appkit-handle-cancel handle))
              (object (appkit-handle-object handle)))
          (if (functionp cancel)
              (funcall cancel object)
            (appkit--default-handle-cancel (appkit-handle-type handle) object)))
      (let ((owner (appkit-handle-owner handle)))
        (when (or (appkit-app-p owner) (appkit-view-p owner))
          (appkit--set-owner-handles
           owner (delq handle (appkit--owner-handles owner))))))
    t))

(defun appkit-cancel-handles (owner)
  "Cancel and forget all lifecycle handles owned by OWNER."
  (let ((handles (appkit--owner-handles owner)))
    (appkit--set-owner-handles owner nil)
    (dolist (handle handles)
      (condition-case err
          (appkit-cancel-handle handle)
        (error
         (message "appkit: handle cleanup failed: %s"
                  (error-message-string err)))))))

(defun appkit-view-for-id (app id)
  "Return APP's live view identified by ID, or nil."
  (when (appkit-app-live-p app)
    (let ((view (gethash id (appkit-app-view-registry app))))
      (and (appkit-view-live-p view) view))))

(defun appkit--detach-current-view ()
  "Detach the appkit view belonging to the current buffer."
  (when (appkit-view-p appkit--current-view)
    (appkit-kill-view appkit--current-view)))

(cl-defun appkit-attach-view
    (&key app id state mode sync-function parts position-policy)
  "Attach a new appkit view for APP and ID to the current buffer."
  (unless (appkit-app-live-p app)
    (error "Cannot attach a view to a dead appkit app"))
  (when (and (appkit-view-p appkit--current-view)
             (appkit-view-live-p appkit--current-view))
    (unless (and (eq app (appkit-view-app appkit--current-view))
                 (equal id (appkit-view-id appkit--current-view)))
      (error "Buffer already belongs to another live appkit view"))
    (setf (appkit-view-state appkit--current-view) state
          (appkit-view-sync-function appkit--current-view) sync-function
          (appkit-view-parts appkit--current-view) (copy-sequence parts)
          (appkit-view-position-policy appkit--current-view) position-policy)
    (cl-return-from appkit-attach-view appkit--current-view))
  (let ((existing (appkit-view-for-id app id)))
    (when (and existing
               (not (eq (appkit-view-buffer existing) (current-buffer))))
      (error "Appkit view id %S is already attached to another buffer" id)))
  (let ((view (appkit-view--create
               :id id
               :app app
               :buffer (current-buffer)
               :mode (or mode major-mode)
               :state state
               :engine nil
               :entry-index (make-hash-table :test #'equal)
               :resource-index (make-hash-table :test #'equal)
               :request-table (make-hash-table :test #'equal)
               :invalidations nil
               :pending-events nil
               :handles nil
               :position-policy position-policy
               :sync-function sync-function
               :parts (copy-sequence parts)
               :update-depth 0
               :alive-p t)))
    (setq-local appkit--current-view view)
    (puthash id view (appkit-app-view-registry app))
    (add-hook 'change-major-mode-hook #'appkit--detach-current-view nil t)
    (add-hook 'kill-buffer-hook #'appkit--detach-current-view nil t)
    view))

(cl-defun appkit-open-view
    (&key app id mode buffer-name state sync-function parts position-policy
          setup select)
  "Open or reuse one appkit view.

MODE initializes a newly created buffer exactly once.  SETUP runs only after a
new view is attached.  SELECT non-nil displays the resulting buffer."
  (unless (and (symbolp mode) (functionp mode))
    (error "Appkit view MODE must name a major mode function"))
  (let* ((existing (appkit-view-for-id app id))
         (buffer (or (and existing (appkit-view-buffer existing))
                     (get-buffer-create buffer-name)))
         view)
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        (funcall mode))
      (setq view
            (appkit-attach-view
             :app app :id id :state state :mode mode
             :sync-function sync-function :parts parts
             :position-policy position-policy))
      (setf (appkit-view-state view) state
            (appkit-view-sync-function view) sync-function
            (appkit-view-parts view) (copy-sequence parts))
      (when (and (null existing) (functionp setup))
        (funcall setup view)))
    (when select (pop-to-buffer buffer))
    view))

(defun appkit-kill-view (view &optional kill-buffer)
  "Detach VIEW and optionally KILL-BUFFER."
  (when (appkit-view-p view)
    (let ((buffer (appkit-view-buffer view))
          (app (appkit-view-app view))
          (id (appkit-view-id view)))
      (when (appkit-view-alive-p view)
        (setf (appkit-view-alive-p view) nil)
        (appkit-cancel-handles view)
        (when (and (appkit-app-p app)
                   (eq view (gethash id (appkit-app-view-registry app))))
          (remhash id (appkit-app-view-registry app)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (eq appkit--current-view view)
              (setq-local appkit--current-view nil)))))
      (when (and kill-buffer (buffer-live-p buffer))
        (kill-buffer buffer))
      t)))

(defun appkit-stop-app (app)
  "Stop APP, detach its views, and cancel owned handles."
  (when (appkit-app-live-p app)
    (setf (appkit-app-alive-p app) nil)
    (let (views)
      (maphash (lambda (_id view) (push view views))
               (appkit-app-view-registry app))
      (dolist (view views) (appkit-kill-view view))
      (clrhash (appkit-app-view-registry app)))
    (appkit-cancel-handles app)
    (when-let* ((shutdown (appkit-app-shutdown app)))
      (funcall shutdown app))
    t))

(defmacro appkit-with-live-view (view &rest body)
  "Evaluate BODY when VIEW is live, with its buffer current."
  (declare (indent 1) (debug t))
  `(let ((appkit-view-value ,view))
     (when (appkit-view-live-p appkit-view-value)
       (with-current-buffer (appkit-view-buffer appkit-view-value)
         ,@body))))

(defun appkit-view-enqueue-event (view event)
  "Append EVENT to live VIEW's synchronization queue."
  (when (appkit-view-live-p view)
    (setf (appkit-view-pending-events view)
          (append (appkit-view-pending-events view) (list event)))
    event))

(defun appkit-view-pending-events-snapshot (view)
  "Return a shallow snapshot of synchronization events queued for VIEW."
  (when (appkit-view-live-p view)
    (copy-sequence (appkit-view-pending-events view))))

(defun appkit-view-acknowledge-events (view count)
  "Forget the first COUNT successfully synchronized events from VIEW."
  (unless (and (integerp count) (>= count 0))
    (error "Appkit event acknowledgement count is invalid: %S" count))
  (when (appkit-view-live-p view)
    (setf (appkit-view-pending-events view)
          (nthcdr count (appkit-view-pending-events view)))))

(defun appkit-app-on (app event function)
  "Subscribe FUNCTION to APP EVENT and return a lifecycle handle."
  (unless (and (appkit-app-live-p app) (functionp function))
    (error "Cannot subscribe a dead app or non-function"))
  (let* ((bus (appkit-app-event-bus app))
         (functions (gethash event bus)))
    (puthash event (cons function (delq function functions)) bus)
    (appkit-register-handle
     app 'event-subscription
     (lambda ()
       (puthash event (delq function (gethash event bus)) bus)))))

(defun appkit-app-emit (app event &rest arguments)
  "Emit APP EVENT with ARGUMENTS."
  (when (appkit-app-live-p app)
    (dolist (function (copy-sequence
                       (gethash event (appkit-app-event-bus app))))
      (apply function arguments))))

(provide 'appkit-core)

;;; appkit-core.el ends here
