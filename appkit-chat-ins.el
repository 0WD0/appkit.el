;;; appkit-chat-ins.el --- Shared insert/render leaf helpers -*- lexical-binding: t; -*-

;; Author: appkit contributors

;;; Commentary:

;; Shared insertion helpers for chat-like renderers.  This module is the
;; owner for small render leaves and formatting primitives; room/root EWOC and
;; timeline orchestration should stay with their UI facades.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)
(require 'appkit-media-card)
(require 'appkit-ui)
(require 'appkit-view)

(defun appkit-chat-ins--current-line-prefix-width ()
  "Return the display prefix width already attached to the current line."
  (let ((prefix (or (get-text-property (line-beginning-position) 'line-prefix)
                    (get-text-property (line-beginning-position) 'wrap-prefix))))
    (if (stringp prefix) (string-width prefix) 0)))

(cl-defun appkit-chat-ins-insert-right-aligned-text
    (text target-width &key face (right-align-p t) left-prefix-width
          (minimum-gap 2) (overflow-newline-p t))
  "Insert TEXT at the right edge of TARGET-WIDTH and return its span.

FACE styles TEXT.  When RIGHT-ALIGN-P is nil, insert one ordinary separating
space instead.  LEFT-PREFIX-WIDTH reserves a display-only prefix which the
caller will apply after insertion.  MINIMUM-GAP is the required gap between
existing line content and TEXT.  If the line cannot fit and
OVERFLOW-NEWLINE-P is non-nil, place TEXT on a new right-aligned line."
  (let* ((raw (or text ""))
         (rendered (if face (propertize raw 'face face) raw))
         (target-width (max 0 (or target-width 0)))
         (prefix-width (+ (max 0 (or left-prefix-width 0))
                          (appkit-chat-ins--current-line-prefix-width)))
         (start (point)))
    (if right-align-p
        (let* ((tail-width (string-width raw))
               (target-column (max 0 (- target-width tail-width)))
               (current-column (+ prefix-width
                                  (appkit-view-current-column))))
          (when (and overflow-newline-p
                     (> current-column
                        (max 0 (- target-column
                                  (max 0 (or minimum-gap 0))))))
            (insert "\n")
            (setq start (point)))
          (appkit-view-move-to-column target-column))
      (insert " "))
    (insert rendered)
    (cons start (point))))

(defun appkit-chat-ins-insert-full-width-divider (label face target-width
                                                  &optional properties)
  "Insert a centered divider for LABEL spanning TARGET-WIDTH columns.

FACE is applied to the entire inserted span.  PROPERTIES is an optional plist
of additional text properties.  Return the inserted span as (START . END)."
  (let* ((open "( ")
         (close " )")
         (inner-width (+ (string-width open)
                         (string-width label)
                         (string-width close)))
         (fill-col (max 0 (or target-width 0)))
         (total-bar (max 4 (- fill-col inner-width)))
         (left-bars (/ total-bar 2))
         (right-bars (- total-bar left-bars))
         (start (point)))
    (insert (make-string left-bars ?─)
            open label close
            (make-string right-bars ?─)
            "\n")
    (add-face-text-property start (point) face t)
    (when properties
      (add-text-properties start (point) properties))
    (cons start (point))))

(defun appkit-chat-ins-insert-divider-row (text face target-width &optional properties)
  "Insert read-only divider row TEXT using FACE across TARGET-WIDTH.

PROPERTIES is appended before the standard read-only divider properties.
Return the inserted span as (START . END)."
  (appkit-chat-ins-insert-full-width-divider
   text face target-width
   (append properties
           '(read-only t
             front-sticky (read-only)
             rear-nonsticky (read-only)))))


(cl-defun appkit-chat-ins-insert-reaction-line
    (reactions &key prefix selected-face unselected-face line-face
               label-function selected-p-function action-function
               help-echo-function)
  "Insert one reaction chip line for REACTIONS.

PREFIX is applied with `appkit-ui-apply-line-prefix'.  SELECTED-FACE and
UNSELECTED-FACE style each reaction chip.  LINE-FACE applies to the whole
inserted span.

LABEL-FUNCTION must format one reaction.  SELECTED-P-FUNCTION identifies the
current account's reactions, ACTION-FUNCTION makes chips clickable and is
called with the selected reaction, and HELP-ECHO-FUNCTION supplies hover
text.  Return the inserted span as (START . END), or nil when REACTIONS is
empty."
  (when reactions
    (unless (functionp label-function)
      (error "Appkit reaction LABEL-FUNCTION must be a function"))
    (let ((line-start (point))
          (first t))
      (dolist (reaction reactions)
        (unless first
          (insert " "))
        (setq first nil)
        (let* ((item reaction)
               (chip (funcall label-function item))
               (selected-p (and selected-p-function
                                (funcall selected-p-function item)))
               (face (if selected-p selected-face unselected-face))
               (help-echo (and help-echo-function
                               (funcall help-echo-function item))))
          (if action-function
              (insert-text-button
               chip
               'follow-link t
               'face face
               'help-echo help-echo
               'action
               (lambda (_button)
                 (funcall action-function item)))
            (insert (propertize chip 'face face)))))
      (insert "\n")
      (appkit-ui-apply-line-prefix line-start (point) (or prefix "    "))
      (when line-face
        (add-face-text-property line-start (point) line-face nil))
      (cons line-start (point)))))



(defun appkit-chat-ins-media-prefix-state (prefix border-face)
  "Return effective attachment prefix state from PREFIX and BORDER-FACE."
  (cond
   ((appkit-ui-prefix-state-p prefix) prefix)
   ((stringp prefix) (appkit-ui-make-prefix-state prefix prefix))
   (t (appkit-ui-card-prefix-state :face border-face))))

(cl-defun appkit-chat-ins-insert-prefixed-line (text &key prefix face properties
                                                action help-echo)
  "Insert TEXT as one prefixed line and return its span.

PREFIX controls the display prefix.  FACE and PROPERTIES style the line.
ACTION makes its content interactive, with HELP-ECHO as the description."
  (let ((start (point)))
    (insert (or text "") "\n")
    (when (and (functionp action)
               (> (point) start))
      (appkit-media-add-action-properties
       start
       (max start (1- (point)))
       (lambda (&optional _event)
         (interactive)
         (funcall action))
       help-echo))
    (appkit-ui-apply-line-prefix start (point) (or prefix "    "))
    (when properties
      (add-text-properties start (point) properties))
    (when face
      (appkit-ui-append-face start (point) face))
    (cons start (point))))

(defun appkit-chat-ins-media-kind-tag (kind)
  "Return human-oriented header tag string for attachment KIND."
  (pcase kind
    ((or 'photo 'image) "[image]")
    ('video "[video]")
    ('audio "[audio]")
    ('sticker "[sticker]")
    (_ "[file]")))

(defun appkit-chat-ins-media-detail-text (details)
  "Return formatted header detail string for DETAILS list."
  (if details
      (format " (%s)" (string-join details ", "))
    ""))

(defun appkit-chat-ins-media-transfer-status-text (state)
  "Return compact transfer status text for normalized media STATE.

STATE uses the shared `:status', `:path', and `:error' plist shape already
returned by both client media adapters.  Ordinary remote media has
no status line; only active, local, or failed transfer state occupies timeline
space."
  (let ((status (plist-get state :status))
        (path (plist-get state :path))
        (error-text (plist-get state :error)))
    (pcase status
      ('downloading "downloading…")
      ('downloaded
       (if (and (stringp path) (not (string-empty-p path)))
           (format "local: %s" (file-name-nondirectory path))
         "downloaded"))
      ('error
       (if (and (stringp error-text) (not (string-empty-p error-text)))
           (format "download failed: %s"
                   (truncate-string-to-width error-text 68 nil nil t))
         "download failed"))
      (_ nil))))

(cl-defun appkit-chat-ins-insert-media-status-line (status &key prefix face)
  "Insert compact media STATUS using PREFIX and FACE."
  (when (and (stringp status) (not (string-empty-p status)))
    (appkit-chat-ins-insert-prefixed-line
     status :prefix (or prefix "    ") :face face)))

(cl-defun appkit-chat-ins-insert-media-card
    (&key kind title details meta status prefix border-face title-face meta-face
          properties context open-action open-help-echo body-inserter)
  "Insert one backend-neutral compact media card.

KIND, TITLE, DETAILS, META, and STATUS describe presentation only.  CONTEXT is
a `appkit-media-card-context-create' value stored as a text property across the
card, so message transients can target the exact attachment/segment at point.
PREFIX supplies a string or mutable prefix state; BORDER-FACE styles the
default card border when PREFIX is absent.  TITLE-FACE and META-FACE style the
corresponding rows.
OPEN-ACTION defaults to the context's open callback.  BODY-INSERTER, when
non-nil, receives the mutable prefix-state and inserts previews, captions, or
stateful controls owned by the client adapter.  PROPERTIES are applied across
the final card span."
  (let* ((card-start (point))
         (prefix-state (appkit-chat-ins-media-prefix-state prefix border-face))
         (details (delq nil (copy-sequence (or details '()))))
         (meta-text (cond
                     ((stringp meta) meta)
                     ((listp meta) (string-join (delq nil (copy-sequence meta)) "  "))
                     (t nil)))
         (action (or open-action (plist-get context :open-action))))
    (appkit-chat-ins-insert-prefixed-line
     (format "%s %s%s"
             (appkit-chat-ins-media-kind-tag kind)
             (or title "media")
             (appkit-chat-ins-media-detail-text details))
     :prefix prefix-state
     :face title-face
     :action action
     :help-echo (or open-help-echo "Open media"))
    (when (and (stringp meta-text) (not (string-empty-p meta-text)))
      (appkit-chat-ins-insert-prefixed-line
       meta-text :prefix prefix-state :face meta-face))
    (appkit-chat-ins-insert-media-status-line
     status :prefix prefix-state :face meta-face)
    (when (functionp body-inserter)
      (funcall body-inserter prefix-state))
    (when properties
      (add-text-properties card-start (point) properties))
    (when context
      (add-text-properties
       card-start (point)
       (list appkit-media-card-context-property context)))
    (cons card-start (point))))

(provide 'appkit-chat-ins)

;;; appkit-chat-ins.el ends here
