;;; appkit-evil-test.el --- Tests for Appkit Evil bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'appkit)
(require 'evil)

(ert-deftest appkit-evil-directory-keeps-native-prefixes-and-actions ()
  (with-temp-buffer
    (appkit-directory-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "RET"))
                #'appkit-directory-activate))
    (should (eq (key-binding (kbd "TAB"))
                #'appkit-directory-tab-dwim))
    (should (eq (key-binding (kbd "<backtab>"))
                #'appkit-directory-previous-item))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g j")) #'evil-next-visual-line))
    (should (eq (key-binding (kbd "g k")) #'evil-previous-visual-line))
    (should (eq (key-binding (kbd "g u")) #'evil-downcase))
    (evil-motion-state)
    (should (eq (key-binding (kbd "RET"))
                #'appkit-directory-activate))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))))

(ert-deftest appkit-evil-directory-leaves-emacs-state-map-unchanged ()
  (with-temp-buffer
    (appkit-directory-mode)
    (evil-emacs-state)
    (should (eq (key-binding (kbd "RET"))
                #'appkit-directory-activate))
    (should (eq (key-binding (kbd "n"))
                #'appkit-directory-next-item))
    (should (eq (key-binding (kbd "p"))
                #'appkit-directory-previous-item))))

(ert-deftest appkit-evil-defers-state-bindings-until-keymap-exists ()
  (let ((symbol 'appkit-evil-test-deferred-mode-map))
    (when (boundp symbol)
      (makunbound symbol))
    (appkit-evil-define-keys 'normal symbol
      (kbd "RET") #'ignore)
    (should (assoc symbol
                   (mapcar (lambda (entry)
                             (cons (nth 1 entry) entry))
                           appkit-evil--deferred-bindings)))
    (set symbol (make-sparse-keymap))
    (appkit-evil--after-load "appkit-evil-test-deferred")
    (with-temp-buffer
      (use-local-map (symbol-value symbol))
      (evil-normal-state)
      (should (eq (key-binding (kbd "RET")) #'ignore))
      (should (eq (key-binding (kbd "g g"))
                  #'evil-goto-first-line)))))

(defvar appkit-evil-test-dynamic-mode-map (make-sparse-keymap))

(define-minor-mode appkit-evil-test-dynamic-mode
  "Test-only dynamic application mode."
  :keymap appkit-evil-test-dynamic-mode-map)

(appkit-evil-define-keys 'normal 'appkit-evil-test-dynamic-mode-map
  (kbd "RET") #'ignore)
(add-hook 'appkit-evil-test-dynamic-mode-hook
          #'appkit-evil-normalize-keymaps)

(ert-deftest appkit-evil-dynamic-minor-mode-refreshes-state-map ()
  (with-temp-buffer
    (evil-normal-state)
    (should-not (eq (key-binding (kbd "RET")) #'ignore))
    (appkit-evil-test-dynamic-mode 1)
    (should (eq (key-binding (kbd "RET")) #'ignore))
    (appkit-evil-test-dynamic-mode -1)
    (should-not (eq (key-binding (kbd "RET")) #'ignore))))


(provide 'appkit-evil-test)

;;; appkit-evil-test.el ends here
