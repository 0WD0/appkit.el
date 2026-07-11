;;; appkit-position.el --- Semantic point and window preservation -*- lexical-binding: t; -*-

;;; Commentary:

;; Stable-key position snapshots shared by full, EWOC, and chat views.

;;; Code:

(require 'cl-lib)

(cl-defstruct (appkit-position-snapshot
               (:constructor appkit-position-snapshot--create))
  line
  column
  anchor-property
  anchor-value
  anchor-line-offset
  window-start-line)

(defun appkit-position-find-property-value (start end property value)
  "Return first position from START to END whose PROPERTY equals VALUE."
  (let ((position start)
        found)
    (while (and (< position end) (not found))
      (if (equal (get-text-property position property) value)
          (setq found position)
        (setq position
              (next-single-property-change position property nil end))))
    found))

(cl-defun appkit-position-capture (&key anchor-property preserve-window-start)
  "Capture semantic point context in the current buffer."
  (let* ((anchor-value (and anchor-property
                            (or (get-text-property (point) anchor-property)
                                (get-text-property (line-beginning-position)
                                                   anchor-property))))
         (anchor-target (and anchor-value
                             (appkit-position-find-property-value
                              (point-min) (point-max)
                              anchor-property anchor-value)))
         (anchor-line-offset
          (and anchor-target
               (max 0 (- (line-number-at-pos)
                         (save-excursion
                           (goto-char anchor-target)
                           (line-number-at-pos))))))
         (window (and preserve-window-start
                      (get-buffer-window (current-buffer))))
         (window-start-line
          (and window
               (save-excursion
                 (goto-char (window-start window))
                 (line-number-at-pos)))))
    (appkit-position-snapshot--create
     :line (line-number-at-pos)
     :column (current-column)
     :anchor-property anchor-property
     :anchor-value anchor-value
     :anchor-line-offset anchor-line-offset
     :window-start-line window-start-line)))

(defun appkit-position--anchor-value-at (position property)
  "Return PROPERTY at POSITION, probing the previous character if needed."
  (or (get-text-property position property)
      (and (> position (point-min))
           (get-text-property (1- position) property))))

(defun appkit-position--move-within-anchor (property value offset)
  "Move OFFSET lines while PROPERTY remains VALUE."
  (let ((remaining (max 0 (or offset 0))))
    (while (> remaining 0)
      (let ((next (save-excursion (forward-line 1) (point))))
        (if (or (= next (point))
                (not (equal (appkit-position--anchor-value-at next property)
                            value)))
            (setq remaining 0)
          (goto-char next)
          (setq remaining (1- remaining)))))))

(defun appkit-position-restore (snapshot &optional anchor-value-map)
  "Restore point and viewport from SNAPSHOT.

ANCHOR-VALUE-MAP maps explicit old keys to promoted keys."
  (let* ((property (appkit-position-snapshot-anchor-property snapshot))
         (captured (appkit-position-snapshot-anchor-value snapshot))
         (value (if-let* ((mapping (assoc captured anchor-value-map)))
                    (cdr mapping)
                  captured))
         (target (and property value
                      (appkit-position-find-property-value
                       (point-min) (point-max) property value))))
    (if target
        (progn
          (goto-char target)
          (appkit-position--move-within-anchor
           property value
           (appkit-position-snapshot-anchor-line-offset snapshot)))
      (goto-char (point-min))
      (forward-line
       (max 0 (1- (or (appkit-position-snapshot-line snapshot) 1)))))
    (move-to-column (max 0 (or (appkit-position-snapshot-column snapshot) 0)))
    (when-let* ((line (appkit-position-snapshot-window-start-line snapshot))
                (window (get-buffer-window (current-buffer))))
      (save-excursion
        (goto-char (point-min))
        (forward-line (max 0 (1- line)))
        (set-window-start window (point) 'noforce)))))

(cl-defun appkit-position-render-preserving
    (render-function &key anchor-property preserve-window-start after-restore)
  "Call RENDER-FUNCTION and restore semantic point and viewport context.

ANCHOR-PROPERTY and PRESERVE-WINDOW-START are passed to
`appkit-position-capture'.  Call AFTER-RESTORE after restoration when it is a
function."
  (let ((snapshot
         (appkit-position-capture
          :anchor-property anchor-property
          :preserve-window-start preserve-window-start)))
    (funcall render-function)
    (when snapshot
      (appkit-position-restore snapshot))
    (when (functionp after-restore)
      (funcall after-restore))))

(provide 'appkit-position)

;;; appkit-position.el ends here
