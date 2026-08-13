;;; appkit-compose.el --- Shared standalone compose surfaces -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Provide a protocol-neutral compose surface on top of chatbuf.  Committed
;; draft items are generated timeline rows.  The trailing composer holds the
;; current uncommitted or in-edit part.  Clients supply context, status
;; fields, attachment records, and transport actions.  Appkit owns the
;; render/edit split, prompt/input boundary, and in-flight submit
;; presentation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-timeline)
(require 'appkit-chatbuf)
(require 'appkit-core)

(appkit-define-app-kind appkit-compose)

(defvar-local appkit-compose--items nil
  "Committed compose items as plists with `:id' and `:text'.")

(defvar-local appkit-compose--input-item nil
  "Item plist for the current composer input, or nil.")

(defvar-local appkit-compose--editing nil
  "Index of the committed item loaded into the composer, or nil.")

(defvar-local appkit-compose--serial 0
  "Serial used to assign stable compose item identifiers.")

(defvar-local appkit-compose--owned-app nil
  "Appkit app started for this compose buffer, or nil when the client owns it.")

(defvar-local appkit-compose--pending-bodies nil
  "Body strings consumed by the next `appkit-compose-refresh', or nil.")

(defvar-local appkit-compose-context-function nil
  "Function returning generated compose context text, or nil.")

(defvar-local appkit-compose-status-fields-function nil
  "Function returning generated compose status field plists, or nil.")

(defvar-local appkit-compose-attachments-function nil
  "Function returning a generated compose attachment section, or nil.")

(defvar-local appkit-compose-parts-function nil
  "Function returning a list of compose part plists, or nil.")

(defvar-local appkit-compose-footer-function nil
  "Function returning generated compose footer text, or nil.")

(defvar-local appkit-compose--submit nil
  "In-flight compose submit plist, or nil when idle.

Keys are `:state' (`submitting' or `cancelling'), optional `:label',
optional `:progress' as a 0-1 float, and optional `:cancel-function'.")

(defun appkit-compose--normalize-progress (progress)
  "Return PROGRESS as a 0-1 float, or nil."
  (cond
   ((null progress) nil)
   ((and (numberp progress) (<= 0 progress) (<= progress 1))
    (float progress))
   (t
    (error "Appkit compose submit progress must be nil or 0-1: %S"
           progress))))

(defun appkit-compose--normalize-label (label)
  "Return LABEL when it is a non-empty string, otherwise nil."
  (cond
   ((null label) nil)
   ((and (stringp label) (not (string-empty-p label))) label)
   (t (error "Appkit compose submit label must be a string: %S" label))))

(defun appkit-compose-submitting-p ()
  "Return non-nil when the current compose buffer has an in-flight submit."
  (and appkit-compose--submit t))

(defun appkit-compose-progress-bar (progress &optional width)
  "Return a WIDTH-column bar for PROGRESS, a 0-1 float.

WIDTH defaults to 10.  Nil or non-positive PROGRESS yields an empty
track.  The filled portion uses `=' characters with a trailing `>'."
  (let* ((width (max 1 (or width 10)))
         (ratio (if (and (numberp progress) (> progress 0))
                    (min 1.0 (max 0.0 (float progress)))
                  0.0))
         (filled (min width (round (* width ratio)))))
    (cond
     ((= filled 0) (make-string width ?\s))
     ((= filled 1) (concat ">" (make-string (1- width) ?\s)))
     (t (concat (make-string (1- filled) ?=)
                ">"
                (make-string (- width filled) ?\s))))))

(defun appkit-compose-progress-text ()
  "Return the current submit status string, or nil when idle.

When progress is known the string includes a bar and a percent.  Clients
own protocol meaning; this is only presentation."
  (when-let* ((submit appkit-compose--submit)
              (label (or (plist-get submit :label) "Submitting...")))
    (if-let* ((progress (plist-get submit :progress)))
        (format "%s  [%s]  %d%%"
                label
                (appkit-compose-progress-bar progress 10)
                (round (* 100 progress)))
      label)))

(cl-defun appkit-compose-begin-submit
    (&key label progress cancel-function)
  "Start a compose submit session in the current buffer.

LABEL is optional status text.  PROGRESS is an optional 0-1 float.
CANCEL-FUNCTION is an optional zero-argument hook the client uses to
abort transport.  Signal an error when a submit is already in flight."
  (when appkit-compose--submit
    (error "Appkit compose submit is already in progress"))
  (when (and cancel-function (not (functionp cancel-function)))
    (error "Appkit compose submit cancel function is not callable: %S"
           cancel-function))
  (setq-local appkit-compose--submit
              (list :state 'submitting
                    :label (or (appkit-compose--normalize-label label)
                               "Submitting...")
                    :progress (appkit-compose--normalize-progress progress)
                    :cancel-function cancel-function))
  (force-mode-line-update)
  appkit-compose--submit)

(cl-defun appkit-compose-update-submit
    (&key (label nil label-p)
          (progress nil progress-p)
          (cancel-function nil cancel-p))
  "Update the current compose submit session.

LABEL, PROGRESS, and CANCEL-FUNCTION replace the current values when
those keywords are supplied.  Omitted keyword arguments keep their
current values.  Signal an error when the surface is idle."
  (unless appkit-compose--submit
    (error "Appkit compose submit is not in progress"))
  (when label-p
    (setf (plist-get appkit-compose--submit :label)
          (or (appkit-compose--normalize-label label) "Submitting...")))
  (when progress-p
    (setf (plist-get appkit-compose--submit :progress)
          (appkit-compose--normalize-progress progress)))
  (when cancel-p
    (when (and cancel-function (not (functionp cancel-function)))
      (error "Appkit compose submit cancel function is not callable: %S"
             cancel-function))
    (setf (plist-get appkit-compose--submit :cancel-function)
          cancel-function))
  (force-mode-line-update)
  appkit-compose--submit)

(defun appkit-compose-finish-submit ()
  "Clear the current compose submit session.

Return the previous session plist, or nil when the surface was idle."
  (let ((submit appkit-compose--submit))
    (setq-local appkit-compose--submit nil)
    (force-mode-line-update)
    submit))

(defun appkit-compose-cancel-submit ()
  "Cancel the current compose submit when a cancel function is installed.

Return non-nil when cancellation was requested.  Return nil when the
surface is idle so the client can abandon the draft.  Signal
`user-error' when a submit is already cancelling, or is in flight
without a cancel function."
  (let ((submit appkit-compose--submit))
    (cond
     ((null submit) nil)
     ((eq (plist-get submit :state) 'cancelling)
      (user-error "The current submit is already being canceled"))
     ((not (functionp (plist-get submit :cancel-function)))
      (user-error "Wait for the current submit to finish"))
     (t
      (setf (plist-get submit :state) 'cancelling)
      (force-mode-line-update)
      (funcall (plist-get submit :cancel-function))
      t))))

(defun appkit-compose--callback-value (function)
  "Call FUNCTION when callable, returning nil when it is nil."
  (when function
    (unless (functionp function)
      (error "Appkit compose callback is not callable: %S" function))
    (funcall function)))

(defun appkit-compose--field-text (field)
  "Return display text for status FIELD."
  (let ((label (plist-get field :label))
        (value (plist-get field :value)))
    (unless (stringp label)
      (error "Appkit compose status field needs a string label: %S" field))
    (if (null value)
        label
      (format "%s: %s" label value))))

(defun appkit-compose--status-field-string (field)
  "Return the generated string for one status FIELD."
  (unless (listp field)
    (error "Appkit compose status field must be a plist: %S" field))
  (let ((text (appkit-compose--field-text field))
        (action (plist-get field :action))
        (face (plist-get field :face))
        (help-echo (plist-get field :help-echo)))
    (when (and action (not (functionp action)))
      (error "Appkit compose status field action is not callable: %S" field))
    (let ((map (and action (make-sparse-keymap))))
      (when action
        (define-key map [header-line down-mouse-1] #'ignore)
        (define-key map [header-line mouse-1]
                    (lambda () (interactive) (funcall action))))
      (apply #'propertize text
             (append (and face (list 'face face))
                     (and help-echo (list 'help-echo help-echo))
                     (and map (list 'local-map map
                                    'mouse-face 'mode-line-highlight)))))))

(defun appkit-compose--status-fields-string (fields)
  "Return generated status FIELDS as one line."
  (when fields
    (let ((pieces '())
          (index 0))
      (dolist (field fields)
        (when (> index 0)
          (push "   " pieces))
        (push (appkit-compose--status-field-string field) pieces)
        (setq index (1+ index)))
      (apply #'concat (nreverse pieces)))))

(defun appkit-compose--attachment-string (attachment)
  "Return the generated string for one ATTACHMENT row."
  (unless (listp attachment)
    (error "Appkit compose attachment must be a plist: %S" attachment))
  (let* ((preview (plist-get attachment :preview))
         (label (or (plist-get attachment :label) "[attachment]"))
         (description (plist-get attachment :description))
         (description-label (or (plist-get attachment :description-label)
                                "Description"))
         (state (plist-get attachment :state))
         (text "  "))
    (unless (stringp label)
      (error "Appkit compose attachment label must be a string: %S"
             attachment))
    (when preview
      (setq text (concat text (propertize " " 'display preview
                                          'rear-nonsticky '(display)))))
    (setq text (concat text label))
    (when (and (stringp description)
               (not (string-empty-p description)))
      (setq text (concat text (format "  %s: %s"
                                      description-label description))))
    (when (and (stringp state)
               (not (string-empty-p state)))
      (setq text (concat text (format "  [%s]" state))))
    (concat text "\n")))

(defun appkit-compose--attachments-string (section)
  "Return generated attachment SECTION text, or an empty string.

SECTION is a plist with `:title' and `:items'.  An empty item list
occupies no frame text."
  (unless (listp section)
    (error "Appkit compose attachment section must be a plist: %S" section))
  (let ((title (or (plist-get section :title) "Attachments"))
        (items (plist-get section :items)))
    (unless (stringp title)
      (error "Appkit compose attachment title must be a string: %S" section))
    (unless (listp items)
      (error "Appkit compose attachment items must be a list: %S" section))
    (if items
        (concat title "\n"
                (mapconcat #'appkit-compose--attachment-string items ""))
      "")))

(defun appkit-compose--ensure-newline (text)
  "Return TEXT ending with a newline, or an empty string."
  (cond
   ((or (null text) (string-empty-p text)) "")
   ((string-suffix-p "\n" text) text)
   (t (concat text "\n"))))

(defun appkit-compose--copy-item (item)
  "Return a shallow copy of ITEM."
  (copy-sequence (or item (list :attachments nil))))

(defun appkit-compose--ensure-item-id (item)
  "Return ITEM with a stable `:id' assigned when missing."
  (if (plist-get item :id)
      item
    (setq-local appkit-compose--serial (1+ appkit-compose--serial))
    (plist-put (appkit-compose--copy-item item)
               :id appkit-compose--serial)))

(defun appkit-compose--item-has-consumer-metadata-p (item)
  "Return non-nil when ITEM carries meaningful client-owned metadata."
  (cl-loop for (key value) on item by #'cddr
           thereis (and (not (memq key '(:id :text)))
                        value)))

(defun appkit-compose--input-text ()
  "Return the current composer input without text properties."
  (if (appkit-chatbuf-input-start-position)
      (substring-no-properties (or (appkit-chatbuf-input-string) ""))
    ""))

(defun appkit-compose--merge-input (item)
  "Return ITEM with composer input text and current input metadata."
  (let ((merged (appkit-compose--ensure-item-id
                 (appkit-compose--copy-item
                  (or appkit-compose--input-item item)))))
    (plist-put merged :text (appkit-compose--input-text))))

(defun appkit-compose-body ()
  "Return the current editable compose body without text properties."
  (appkit-compose--input-text))

(defun appkit-compose-body-region-bounds ()
  "Return the editable compose body bounds, or nil."
  (appkit-chatbuf-input-region-bounds))

(defun appkit-compose-body-start-position ()
  "Return the start position of the editable compose body, or nil."
  (appkit-chatbuf-input-start-position))

(defun appkit-compose-body-end-position ()
  "Return the end position of the editable compose body, or nil."
  (cdr (appkit-chatbuf-input-region-bounds)))

(defun appkit-compose-bodies ()
  "Return every compose body, including the live composer input."
  (if appkit-compose--pending-bodies
      (copy-sequence appkit-compose--pending-bodies)
    (let ((texts (mapcar (lambda (item)
                           (or (plist-get item :text) ""))
                         appkit-compose--items))
          (input (appkit-compose-body)))
      (cond
       ((integerp appkit-compose--editing)
        (setf (nth appkit-compose--editing texts) input)
        texts)
       ((or (not (string-empty-p input))
            (appkit-compose--item-has-consumer-metadata-p
             appkit-compose--input-item)
            (null texts))
        (append texts (list input)))
       (t texts)))))

(defun appkit-compose-set-items (items)
  "Replace compose items with ITEMS and load the last into the composer.

ITEMS is a list of plists.  Each item may include `:text' and other
client metadata such as `:attachments'."
  (unless (listp items)
    (error "Appkit compose items must be a list: %S" items))
  (let* ((copies (mapcar (lambda (item)
                           (appkit-compose--ensure-item-id
                            (appkit-compose--copy-item item)))
                         (or items (list (list :text "" :attachments nil)))))
         (last (car (last copies)))
         (committed (butlast copies)))
    (setq-local appkit-compose--items committed
                appkit-compose--input-item last
                appkit-compose--editing nil
                appkit-compose--pending-bodies nil)
    (appkit-chatbuf-init-state)
    (appkit-chatbuf-input-state-set (or (plist-get last :text) ""))
    (when (appkit-chatbuf-input-start-position)
      (appkit-chatbuf-input-set-text (or (plist-get last :text) "")))
    (appkit-compose-refresh)
    (appkit-compose-items)))

(defun appkit-compose-set-bodies (bodies)
  "Use BODIES as committed items plus the trailing composer input.

BODIES must be a list of strings.  Every body except the last becomes a
rendered draft row.  The last body is loaded into the composer."
  (unless (and (listp bodies)
               (cl-every #'stringp bodies))
    (error "Appkit compose bodies must be a list of strings: %S" bodies))
  (setq-local appkit-compose--pending-bodies (copy-sequence bodies)))

(defun appkit-compose-items ()
  "Return compose items aligned with `appkit-compose-bodies'.

Committed rows keep their stored metadata.  The live composer input is
merged into the edited item, or appended as a new item."
  (let ((texts (appkit-compose-bodies))
        (index 0)
        items)
    (dolist (text texts)
      (let ((item
             (cond
              ((eq index appkit-compose--editing)
               (appkit-compose--merge-input
                (nth index appkit-compose--items)))
              ((< index (length appkit-compose--items))
               (plist-put
                (appkit-compose--copy-item
                 (nth index appkit-compose--items))
                :text text))
              (t
               (appkit-compose--merge-input
                appkit-compose--input-item)))))
        (push item items))
      (setq index (1+ index)))
    (nreverse items)))

(defun appkit-compose--apply-pending-bodies ()
  "Apply `appkit-compose--pending-bodies' when it is set."
  (when appkit-compose--pending-bodies
    (let* ((bodies (copy-sequence appkit-compose--pending-bodies))
           (last (if bodies (car (last bodies)) ""))
           (committed (if bodies (butlast bodies) '())))
      (setq-local appkit-compose--pending-bodies nil
                  appkit-compose--editing nil
                  appkit-compose--input-item (list :attachments nil)
                  appkit-compose--items
                  (mapcar (lambda (text)
                            (appkit-compose--ensure-item-id
                             (list :text text :attachments nil)))
                          committed))
      (appkit-chatbuf-init-state)
      (appkit-chatbuf-input-state-set last)
      (when (appkit-chatbuf-input-start-position)
        (appkit-chatbuf-input-set-text last)))))

(defun appkit-compose-item-index-at-point ()
  "Return the committed item index at point, or nil."
  (get-text-property (point) 'appkit-compose-item-index))

(defun appkit-compose-current-part-index ()
  "Return the 0-based compose part that contains point, or nil."
  (cond
   ((appkit-chatbuf-point-in-input-p)
    (or appkit-compose--editing (length appkit-compose--items)))
   ((appkit-compose-item-index-at-point))
   (appkit-compose--items
    (1- (length appkit-compose--items)))
   (t 0)))

(defun appkit-compose-current-item ()
  "Return the compose item plist for the current part."
  (let ((index (or (appkit-compose-current-part-index) 0)))
    (cond
     ((and (integerp appkit-compose--editing)
           (or (appkit-chatbuf-point-in-input-p)
               (eq index appkit-compose--editing)))
      (appkit-compose--merge-input
       (nth appkit-compose--editing appkit-compose--items)))
     ((and (integerp index)
           (< index (length appkit-compose--items)))
      (nth index appkit-compose--items))
     (t
      (appkit-compose--merge-input appkit-compose--input-item)))))

(defun appkit-compose-update-current-item (item)
  "Replace the current compose item with ITEM."
  (let ((index (or (appkit-compose-current-part-index) 0))
        (updated (appkit-compose--copy-item item)))
    (cond
     ((and (integerp appkit-compose--editing)
           (or (appkit-chatbuf-point-in-input-p)
               (eq index appkit-compose--editing)))
      (setq-local appkit-compose--input-item updated)
      (setf (nth appkit-compose--editing appkit-compose--items)
            (appkit-compose--merge-input updated)))
     ((and (integerp index)
           (< index (length appkit-compose--items)))
      (setf (nth index appkit-compose--items)
            (appkit-compose--ensure-item-id updated)))
     (t
      (setq-local appkit-compose--input-item updated)))
    (appkit-compose-refresh)
    updated))

(defun appkit-compose--resolved-parts ()
  "Return the client part list aligned with committed items."
  (if appkit-compose-parts-function
      (let ((parts (appkit-compose--callback-value
                    appkit-compose-parts-function)))
        (unless (listp parts)
          (error "Appkit compose parts must be a list: %S" parts))
        (or parts (list nil)))
    (list (list :attachments
                (appkit-compose--callback-value
                 appkit-compose-attachments-function)))))

(defun appkit-compose--print-row (row)
  "Insert one generated compose ROW."
  (let* ((item (appkit-chat-timeline-row-payload row))
         (index (plist-get item :index))
         (part (plist-get item :part))
         (editing (eq index appkit-compose--editing))
         (title (plist-get part :title))
         (text (or (plist-get item :text) ""))
         (start (point)))
    (when (and (stringp title) (not (string-empty-p title)))
      (insert (appkit-compose--ensure-newline title)))
    (when editing
      (insert (propertize "(editing in composer)\n" 'face 'shadow)))
    (insert text)
    (unless (or (string-empty-p text)
                (string-suffix-p "\n" text))
      (insert "\n"))
    (when-let* ((attachments (plist-get part :attachments)))
      (insert (appkit-compose--attachments-string attachments)))
    (add-text-properties
     start (point)
     (list 'appkit-compose-item-index index
           'appkit-compose-item-id (plist-get item :id)))))

(defun appkit-compose--timeline-entries ()
  "Return timeline entries for committed compose items."
  (let ((parts (appkit-compose--resolved-parts))
        (index 0)
        entries)
    (dolist (item appkit-compose--items)
      (let ((part (or (nth index parts) (car (last parts)))))
        (push (list :id (plist-get item :id)
                    :index index
                    :text (if (eq index appkit-compose--editing)
                              (appkit-compose--input-text)
                            (or (plist-get item :text) ""))
                    :part part)
              entries))
      (setq index (1+ index)))
    (nreverse entries)))

(defun appkit-compose--header-line ()
  "Return the current compose header-line string."
  (let* ((fields (appkit-compose--callback-value
                  appkit-compose-status-fields-function))
         (text (appkit-compose--status-fields-string fields)))
    (when (and fields (not (listp fields)))
      (error "Appkit compose status fields must be a list: %S" fields))
    (or text "")))

(defun appkit-compose--current-attachments ()
  "Return the attachment section for the current composer item, or nil."
  (let* ((parts (appkit-compose--resolved-parts))
         (index (or (appkit-compose-current-part-index) 0))
         (part (or (nth index parts) (car (last parts)))))
    (or (plist-get part :attachments)
        (appkit-compose--callback-value
         appkit-compose-attachments-function))))

(defun appkit-compose--frame-header ()
  "Return generated header text placed above committed draft rows.

A non-empty header ends with a blank line so it does not run into
the first draft row, attachment footer, or prompt."
  (let ((text (appkit-compose--ensure-newline
               (appkit-compose--callback-value
                appkit-compose-context-function))))
    (if (string-empty-p text)
        ""
      (concat text "\n"))))

(defun appkit-compose--frame-footer ()
  "Return generated footer text placed above the composer.

A non-empty footer ends with a blank line so the prompt stays on its
own line.  Chat timelines use EWOC `nosep', so clients cannot rely on
automatic separators between header, footer, and prompt."
  (let ((text (appkit-compose--ensure-newline
               (concat
                (when-let* ((section (appkit-compose--current-attachments)))
                  (appkit-compose--attachments-string section))
                (or (appkit-compose--callback-value
                     appkit-compose-footer-function)
                    "")))))
    (if (string-empty-p text)
        ""
      (concat text "\n"))))

(defun appkit-compose--bind-composer ()
  "Install the trailing compose prompt and input when missing."
  (appkit-chatbuf-bind-input-region
   :visible-p t
   :prompt ">>> "
   :input-text (if appkit-compose--pending-bodies
                   (or (car (last appkit-compose--pending-bodies)) "")
                 (or (appkit-chatbuf-input-state)
                     (appkit-compose--input-text)
                     ""))))

(defun appkit-compose--ensure-view (app)
  "Attach a compose view to the current buffer and return it.

APP is an optional live appkit app.  When it is nil, start a buffer-owned
compose app."
  (or (and (appkit-view-live-p (appkit-current-view))
           (appkit-current-view))
      (let* ((owned (null app))
             (target (or app
                         (appkit-start-app
                          'appkit-compose
                          :id (intern (format "compose-%x"
                                              (sxhash-eq (current-buffer))))))))
        (when owned
          (setq-local appkit-compose--owned-app target))
        (appkit-attach-view
         :app target
         :id (list 'compose (intern (format "b%x" (sxhash-eq (current-buffer)))))
         :mode major-mode
         :sync-function #'appkit-compose-refresh))))

(defun appkit-compose--stop-owned-app ()
  "Stop the compose app started for the current buffer, if any."
  (when (appkit-app-live-p appkit-compose--owned-app)
    (appkit-stop-app appkit-compose--owned-app))
  (setq-local appkit-compose--owned-app nil))

(defun appkit-compose-refresh ()
  "Refresh generated compose presentation without rewriting composer input."
  (appkit-compose--ensure-view nil)
  (when (and (not appkit-compose--pending-bodies)
             (not appkit-compose--items)
             (not (appkit-chat-timeline-live-p))
             (> (buffer-size) 0))
    (setq-local appkit-compose--pending-bodies
                (list (buffer-substring-no-properties (point-min)
                                                      (point-max)))))
  (appkit-compose--apply-pending-bodies)
  (unless (appkit-chat-timeline-live-p)
    (appkit-chat-timeline-ensure
     :printer #'appkit-compose--print-row
     :anchor-property 'appkit-compose-item-id))
  (appkit-chat-timeline-sync
   (appkit-chat-timeline-project
    (appkit-compose--timeline-entries)
    (lambda (entry)
      (plist-get entry :id))))
  (condition-case _
      (appkit-chat-timeline-set-frame
       (appkit-compose--frame-header)
       (appkit-compose--frame-footer)
       :bind-input-function #'appkit-compose--bind-composer
       :composer-visible-p t)
    (error
     (appkit-compose--bind-composer)))
  (setq-local header-line-format '(:eval (appkit-compose--header-line)))
  (force-mode-line-update)
  (appkit-compose-body-region-bounds))

(defun appkit-compose-display-string ()
  "Return visible compose text, including header-line chrome."
  (concat (appkit-compose--header-line)
          (unless (string-empty-p (appkit-compose--header-line))
            "\n")
          (buffer-substring-no-properties (point-min) (point-max))))

(defun appkit-compose-goto-part (index)
  "Load compose part INDEX into the composer, or focus new input."
  (when (and (integerp appkit-compose--editing)
             (not (eq appkit-compose--editing index)))
    (appkit-compose--write-back-edit))
  (cond
   ((and (integerp index)
         (< index (length appkit-compose--items)))
    (let ((item (nth index appkit-compose--items)))
      (setq-local appkit-compose--editing index
                  appkit-compose--input-item (appkit-compose--copy-item item))
      (appkit-chatbuf-input-set-text (or (plist-get item :text) ""))
      (appkit-compose-refresh)
      (goto-char (appkit-chatbuf-input-start-position))))
   (t
    (setq-local appkit-compose--editing nil
                appkit-compose--input-item
                (or appkit-compose--input-item (list :attachments nil)))
    (appkit-compose-refresh)
    (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))))

(defun appkit-compose--write-back-edit ()
  "Store the current composer input back into the edited item."
  (when (integerp appkit-compose--editing)
    (setf (nth appkit-compose--editing appkit-compose--items)
          (appkit-compose--merge-input
           (nth appkit-compose--editing appkit-compose--items)))
    (setq-local appkit-compose--editing nil
                appkit-compose--input-item (list :attachments nil))))

(defun appkit-compose--flush-input ()
  "Write back an edit or commit composer input with content or metadata."
  (cond
   ((integerp appkit-compose--editing)
    (appkit-compose--write-back-edit))
   ((or (not (string-empty-p (appkit-compose--input-text)))
        (appkit-compose--item-has-consumer-metadata-p
         appkit-compose--input-item))
    (setq-local appkit-compose--items
                (append appkit-compose--items
                        (list (appkit-compose--merge-input
                               appkit-compose--input-item))))
    (setq-local appkit-compose--input-item (list :attachments nil))
    (appkit-chatbuf-input-set-text ""))))

(defun appkit-compose-add-item ()
  "Insert an empty draft item after the current part and edit it."
  (interactive)
  (let ((index (1+ (or (appkit-compose-current-part-index) 0))))
    (appkit-compose--flush-input)
    (setq index (min index (length appkit-compose--items)))
    (let ((item (appkit-compose--ensure-item-id
                 (list :text "" :attachments nil))))
      (setq-local appkit-compose--items
                  (append (cl-subseq appkit-compose--items 0 index)
                          (list item)
                          (cl-subseq appkit-compose--items index)))
      (appkit-compose-goto-part index)
      (set-buffer-modified-p t)
      index)))

(defun appkit-compose-drop-item (&optional index)
  "Remove compose item INDEX, or the current part when INDEX is nil."
  (interactive)
  (let ((target (or index (appkit-compose-current-part-index))))
    (unless (and (integerp target)
                 (< target (length appkit-compose--items)))
      (user-error "No committed compose item to remove"))
    (when (eq target appkit-compose--editing)
      (setq-local appkit-compose--editing nil
                  appkit-compose--input-item (list :attachments nil))
      (appkit-chatbuf-input-set-text ""))
    (when (and (integerp appkit-compose--editing)
               (> appkit-compose--editing target))
      (setq-local appkit-compose--editing (1- appkit-compose--editing)))
    (setq-local appkit-compose--items
                (append (cl-subseq appkit-compose--items 0 target)
                        (cl-subseq appkit-compose--items (1+ target))))
    (appkit-compose-refresh)
    (appkit-compose-goto-part (min target (max 0 (1- (length appkit-compose--items)))))
    (set-buffer-modified-p t)
    target))

(defun appkit-compose-edit-at-point ()
  "Load the committed compose item at point into the composer."
  (interactive)
  (if-let* ((index (appkit-compose-item-index-at-point)))
      (appkit-compose-goto-part index)
    (user-error "No compose item at point")))

(defvar appkit-compose-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'appkit-compose-edit-at-point)
    (define-key map (kbd "e") #'appkit-compose-edit-at-point)
    (define-key map (kbd "d") #'appkit-compose-drop-item)
    map)
  "Keymap for generated compose draft rows.")

(define-minor-mode appkit-compose-timeline-mode
  "Enable keys on generated compose draft rows."
  :init-value nil
  :lighter nil
  :keymap appkit-compose-timeline-mode-map)

(cl-defun appkit-compose-setup
    (&key app context-function status-fields-function attachments-function
          parts-function footer-function)
  "Configure generated compose callbacks and refresh the current buffer.

APP is an optional live appkit app that should own the compose view.
CONTEXT-FUNCTION returns a context string.  STATUS-FIELDS-FUNCTION returns a
list of field plists with `:label', `:value', and optional `:action'.
ATTACHMENTS-FUNCTION returns an attachment-section plist for a single part.
PARTS-FUNCTION returns a list of part plists with optional `:title' and
`:attachments'; when it is set, it replaces ATTACHMENTS-FUNCTION.
FOOTER-FUNCTION returns the generated footer string.  Client callbacks own
all protocol semantics and may be nil when a section is not needed."
  (dolist (entry `((:context . ,context-function)
                   (:status-fields . ,status-fields-function)
                   (:attachments . ,attachments-function)
                   (:parts . ,parts-function)
                   (:footer . ,footer-function)))
    (let ((function (cdr entry)))
      (when (and function (not (functionp function)))
        (error "Appkit compose %s callback is not callable: %S"
               (car entry) function))))
  (setq-local appkit-compose-context-function context-function
              appkit-compose-status-fields-function status-fields-function
              appkit-compose-attachments-function attachments-function
              appkit-compose-parts-function parts-function
              appkit-compose-footer-function footer-function)
  (appkit-compose--ensure-view app)
  (appkit-chatbuf-use-timeline-mode #'appkit-compose-timeline-mode)
  (appkit-compose-refresh))

(define-derived-mode appkit-compose-mode appkit-chatbuf-mode "Appkit-Compose"
  "Major mode for a standalone compose surface on a chatbuf.

Committed draft items are generated timeline rows.  The trailing
composer holds the current uncommitted or in-edit body."
  (setq-local header-line-format '(:eval (appkit-compose--header-line)))
  (setq-local require-final-newline nil)
  (buffer-enable-undo)
  (setq-local appkit-compose--submit nil)
  (setq-local appkit-compose--items nil)
  (setq-local appkit-compose--input-item (list :attachments nil))
  (setq-local appkit-compose--editing nil)
  (setq-local appkit-compose--serial 0)
  (setq-local appkit-compose--pending-bodies nil)
  (add-hook 'kill-buffer-hook #'appkit-compose--stop-owned-app nil t))

(provide 'appkit-compose)

;;; appkit-compose.el ends here
