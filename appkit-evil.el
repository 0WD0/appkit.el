;;; appkit-evil.el --- Optional Evil bindings for Appkit -*- lexical-binding: t; -*-

;;; Commentary:

;; Appkit's ordinary mode maps remain the Emacs-state contract.  This optional
;; integration defines a separate Evil-state vocabulary instead of raising an
;; entire application map above Evil: doing the latter turns single application
;; keys such as `g' into terminal bindings and breaks native sequences such as
;; `gg'.  Clients can use `appkit-evil-define-keys' for their own state maps.

;;; Code:

(require 'cl-lib)
(require 'appkit-directory)

(declare-function evil-define-key* "evil-core"
                  (state keymap key def &rest bindings))
(declare-function evil-normalize-keymaps "evil-core" (&optional state))
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function evil-quit "evil-commands" (&optional force))
(defvar evil-local-mode)

(defgroup appkit-evil nil
  "Optional Evil integration for Appkit surfaces."
  :group 'appkit
  :prefix "appkit-evil-")

(defcustom appkit-evil-enable-integration t
  "If non-nil, install Appkit's Evil bindings automatically."
  :type 'boolean
  :group 'appkit-evil)

(defcustom appkit-evil-directory-initial-state 'normal
  "Initial Evil state for `appkit-directory-mode'.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'appkit-evil)

(defvar appkit-evil--deferred-bindings nil
  "Bindings waiting for their keymap variables to be defined.")

(defconst appkit-evil--readonly-states '(normal motion)
  "Evil states used by read-only Appkit application bindings.")

(defun appkit-evil--keymap-value (symbol)
  "Return SYMBOL's keymap value, or nil when it is not available."
  (and (boundp symbol)
       (keymapp (symbol-value symbol))
       (symbol-value symbol)))

(defun appkit-evil--install-binding (entry)
  "Install deferred binding ENTRY and return non-nil when successful."
  (pcase-let ((`(,states ,keymap-symbol ,bindings) entry))
    (when-let* (((featurep 'evil))
                ((fboundp 'evil-define-key*))
                (keymap (appkit-evil--keymap-value keymap-symbol)))
      (apply #'evil-define-key* states keymap bindings)
      t)))

(defun appkit-evil--after-load (&rest _args)
  "Install bindings whose keymaps became available after a library load."
  (when (and (featurep 'evil)
             appkit-evil-enable-integration
             appkit-evil-directory-initial-state)
    (evil-set-initial-state
     'appkit-directory-mode appkit-evil-directory-initial-state))
  (setq appkit-evil--deferred-bindings
        (cl-delete-if #'appkit-evil--install-binding
                      appkit-evil--deferred-bindings))
  (unless appkit-evil--deferred-bindings
    (remove-hook 'after-load-functions #'appkit-evil--after-load)))


(defun appkit-evil-define-keys (states keymap-symbol &rest bindings)
  "Define Evil BINDINGS for STATES in KEYMAP-SYMBOL.

STATES is an Evil state or a list of states.  KEYMAP-SYMBOL names a keymap
variable and may be defined later.  Installation waits until both Evil and the
keymap are available."
  (declare (indent 2))
  (unless (symbolp keymap-symbol)
    (error "Appkit Evil keymap name is not a symbol: %S" keymap-symbol))
  (let ((entry (list states keymap-symbol bindings)))
    (if (appkit-evil--install-binding entry)
        (setq appkit-evil--deferred-bindings
              (delete entry appkit-evil--deferred-bindings))
      (cl-pushnew entry appkit-evil--deferred-bindings :test #'equal)
      (add-hook 'after-load-functions #'appkit-evil--after-load t)))
  keymap-symbol)

(defun appkit-evil-normalize-keymaps ()
  "Refresh Evil's active keymaps in the current buffer when Evil is active."
  (when (and (featurep 'evil)
             (boundp 'evil-local-mode)
             evil-local-mode)
    (evil-normalize-keymaps)))

(defun appkit-evil-define-readonly-keys (keymap-symbol)
  "Install standard read-only modal bindings in KEYMAP-SYMBOL.

`q' and `ZZ' close the window, and `ZQ' uses `evil-quit'.  Editing commands
are deliberately left to Evil and the buffer's own read-only enforcement.
Call this before adding surface-specific bindings."
  (appkit-evil-define-keys appkit-evil--readonly-states keymap-symbol
    (kbd "q") #'quit-window
    (kbd "ZZ") #'quit-window
    (kbd "ZQ") #'evil-quit))

(defun appkit-evil--define-directory-keys ()
  "Install modal bindings for `appkit-directory-mode-map'."
  (appkit-evil-define-readonly-keys 'appkit-directory-mode-map)
  (appkit-evil-define-keys appkit-evil--readonly-states
      'appkit-directory-mode-map
    (kbd "RET") #'appkit-directory-activate
    (kbd "<return>") #'appkit-directory-activate
    (kbd "TAB") #'appkit-directory-tab-dwim
    (kbd "<backtab>") #'appkit-directory-previous-item))

;;;###autoload
(defun appkit-evil-setup ()
  "Install Appkit's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when appkit-evil-enable-integration
    (when (and (featurep 'evil) appkit-evil-directory-initial-state)
      (evil-set-initial-state
       'appkit-directory-mode appkit-evil-directory-initial-state))
    (appkit-evil--define-directory-keys)))

(appkit-evil-setup)

(provide 'appkit-evil)

;;; appkit-evil.el ends here
