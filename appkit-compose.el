;;; appkit-compose.el --- Shared standalone compose surfaces -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Provide a protocol-neutral standalone compose buffer.  Clients supply
;; context, status fields, attachment records, and transport actions; Appkit
;; owns the editable-body boundary and generated presentation invariants.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'appkit-ui)

(defvar-local appkit-compose--body-start-marker nil
  "Marker at the start of the editable compose body.")

(defvar-local appkit-compose--body-end-marker nil
  "Marker at the end of the editable compose body.")

(defvar-local appkit-compose-context-function nil
  "Function returning generated compose context text, or nil.")

(defvar-local appkit-compose-status-fields-function nil
  "Function returning generated compose status field plists, or nil.")

(defvar-local appkit-compose-attachments-function nil
  "Function returning a generated compose attachment section, or nil.")

(defvar-local appkit-compose-footer-function nil
  "Function returning generated compose footer text, or nil.")

(defun appkit-compose--init-state ()
  "Initialize compose markers without changing buffer content."
  (unless (markerp appkit-compose--body-start-marker)
    (setq-local appkit-compose--body-start-marker
                (copy-marker (point-min))))
  (unless (markerp appkit-compose--body-end-marker)
    (setq-local appkit-compose--body-end-marker
                (copy-marker (point-max) t))))

(defun appkit-compose--clear-marker (marker)
  "Detach MARKER when it is a marker."
  (when (markerp marker)
    (set-marker marker nil)))

(defun appkit-compose--generated-properties ()
  "Return text properties used by generated compose presentation."
  '(read-only t
    rear-nonsticky (read-only field)
    field appkit-compose-generated))

(defun appkit-compose--insert-generated (text)
  "Insert generated TEXT and make it read-only."
  (when (and (stringp text)
             (not (string-empty-p text)))
    (let ((start (point)))
      (insert text)
      (add-text-properties start (point)
                           (appkit-compose--generated-properties)))))

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

(defun appkit-compose--insert-status-field (field)
  "Insert one generated status FIELD."
  (unless (listp field)
    (error "Appkit compose status field must be a plist: %S" field))
  (let* ((start (point))
         (text (appkit-compose--field-text field))
         (action (plist-get field :action))
         (face (plist-get field :face))
         (help-echo (plist-get field :help-echo)))
    (when (and action (not (functionp action)))
      (error "Appkit compose status field action is not callable: %S" field))
    (if action
        (insert-text-button
         text
         'action (lambda (_button) (funcall action))
         'follow-link t
         'help-echo help-echo
         'face face)
      (insert (if face (propertize text 'face face) text)))
    (add-text-properties start (point)
                         (appkit-compose--generated-properties))))

(defun appkit-compose--insert-status-fields (fields)
  "Insert generated status FIELDS as one action-capable line."
  (when fields
    (let ((index 0))
      (dolist (field fields)
        (when (> index 0)
          (insert "   "))
        (appkit-compose--insert-status-field field)
        (setq index (1+ index)))
      (insert "\n"))))

(defun appkit-compose--insert-attachment (attachment)
  "Insert one generated ATTACHMENT row."
  (unless (listp attachment)
    (error "Appkit compose attachment must be a plist: %S" attachment))
  (let* ((start (point))
         (preview (plist-get attachment :preview))
         (label (or (plist-get attachment :label) "[attachment]"))
         (description (plist-get attachment :description))
         (description-label (or (plist-get attachment :description-label)
                                "Description"))
         (state (plist-get attachment :state))
         (action (plist-get attachment :action))
         (object (if (plist-member attachment :object)
                     (plist-get attachment :object)
                   attachment))
         (help-echo (plist-get attachment :help-echo)))
    (unless (stringp label)
      (error "Appkit compose attachment label must be a string: %S"
             attachment))
    (when (and action (not (functionp action)))
      (error "Appkit compose attachment action is not callable: %S"
             attachment))
    (insert "  ")
    (when preview
      (let ((preview-start (point)))
        (insert " ")
        (add-text-properties
         preview-start (point)
         (list 'display preview
               'rear-nonsticky '(display)))))
    (insert label)
    (when (and (stringp description)
               (not (string-empty-p description)))
      (insert (format "  %s: %s" description-label description)))
    (when (and (stringp state)
               (not (string-empty-p state)))
      (insert (format "  [%s]" state)))
    (insert "\n")
    (when action
      (appkit-ui-make-action-row
       start (point) object action :help-echo help-echo :mouse-face 'highlight))
    (add-text-properties start (point)
                         (appkit-compose--generated-properties))))

(defun appkit-compose--insert-attachments (section)
  "Insert generated attachment SECTION.

SECTION is a plist with `:title', `:items', and optional `:empty-label'."
  (unless (listp section)
    (error "Appkit compose attachment section must be a plist: %S" section))
  (let ((title (or (plist-get section :title) "Attachments"))
        (items (plist-get section :items))
        (empty-label (or (plist-get section :empty-label)
                         "  No attachments.")))
    (unless (stringp title)
      (error "Appkit compose attachment title must be a string: %S" section))
    (unless (listp items)
      (error "Appkit compose attachment items must be a list: %S" section))
    (appkit-compose--insert-generated (concat title "\n"))
    (if items
        (dolist (attachment items)
          (appkit-compose--insert-attachment attachment))
      (appkit-compose--insert-generated (concat empty-label "\n")))))

(defun appkit-compose-body-region-bounds ()
  "Return the editable compose body bounds, or nil."
  (when (and (markerp appkit-compose--body-start-marker)
             (markerp appkit-compose--body-end-marker))
    (cons (marker-position appkit-compose--body-start-marker)
          (marker-position appkit-compose--body-end-marker))))

(defun appkit-compose-body-start-position ()
  "Return the start position of the editable compose body, or nil."
  (car (appkit-compose-body-region-bounds)))

(defun appkit-compose-body-end-position ()
  "Return the end position of the editable compose body, or nil."
  (cdr (appkit-compose-body-region-bounds)))

(defun appkit-compose-body ()
  "Return the current editable compose body without text properties."
  (if-let* ((bounds (appkit-compose-body-region-bounds)))
      (buffer-substring-no-properties (car bounds) (cdr bounds))
    ""))

(defun appkit-compose-refresh ()
  "Refresh generated compose presentation while preserving the body.

The configured callbacks are called in the current buffer.  The editable body,
point offset, modified flag, and read-only generated regions remain separate."
  (appkit-compose--init-state)
  (let* ((body (appkit-compose-body))
         (modified (buffer-modified-p))
         (old-start (appkit-compose-body-start-position))
         (point-offset (if (and old-start (>= (point) old-start))
                           (- (point) old-start)
                         0))
         (context (appkit-compose--callback-value
                   appkit-compose-context-function))
         (fields (appkit-compose--callback-value
                  appkit-compose-status-fields-function))
         (attachments (appkit-compose--callback-value
                       appkit-compose-attachments-function))
         (footer (appkit-compose--callback-value
                  appkit-compose-footer-function))
         (has-chrome (or context fields attachments))
         (inhibit-read-only t))
    (when (and fields (not (listp fields)))
      (error "Appkit compose status fields must be a list: %S" fields))
    (appkit-compose--clear-marker appkit-compose--body-start-marker)
    (appkit-compose--clear-marker appkit-compose--body-end-marker)
    (erase-buffer)
    (when context
      (appkit-compose--insert-generated context))
    (when fields
      (when context
        (insert "\n"))
      (appkit-compose--insert-status-fields fields))
    (when attachments
      (when (or context fields)
        (insert "\n"))
      (appkit-compose--insert-attachments attachments))
    (when has-chrome
      (insert "\n"))
    (setq-local appkit-compose--body-start-marker (copy-marker (point)))
    (insert body)
    (let ((body-end (point)))
      (when footer
        (insert "\n\n")
        (appkit-compose--insert-generated footer))
      (setq-local appkit-compose--body-end-marker
                  (copy-marker body-end t)))
    (goto-char (+ (marker-position appkit-compose--body-start-marker)
                  (min point-offset (length body))))
    (set-buffer-modified-p modified)
    (appkit-compose-body-region-bounds)))

(cl-defun appkit-compose-setup
    (&key context-function status-fields-function attachments-function
          footer-function)
  "Configure generated compose callbacks and refresh the current buffer.

CONTEXT-FUNCTION returns a context string.  STATUS-FIELDS-FUNCTION returns a
list of field plists with `:label', `:value', and optional `:action'.
ATTACHMENTS-FUNCTION returns an attachment-section plist.  FOOTER-FUNCTION
returns the generated footer string.  Client callbacks own all protocol
semantics and may be nil when a section is not needed."
  (dolist (entry `((:context . ,context-function)
                   (:status-fields . ,status-fields-function)
                   (:attachments . ,attachments-function)
                   (:footer . ,footer-function)))
    (let ((function (cdr entry)))
      (when (and function (not (functionp function)))
        (error "Appkit compose %s callback is not callable: %S"
               (car entry) function))))
  (setq-local appkit-compose-context-function context-function
              appkit-compose-status-fields-function status-fields-function
              appkit-compose-attachments-function attachments-function
              appkit-compose-footer-function footer-function)
  (appkit-compose-refresh))

(define-derived-mode appkit-compose-mode text-mode "Appkit-Compose"
  "Major mode for a standalone protocol-neutral compose surface."
  (setq-local header-line-format nil)
  (setq-local require-final-newline nil)
  (appkit-compose--init-state))

(provide 'appkit-compose)

;;; appkit-compose.el ends here
