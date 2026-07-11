;;; appkit.el --- Runtime primitives for stateful buffer applications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Maintainer: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: lisp, extensions
;; URL: https://github.com/0WD0/appkit.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (plz "0.8"))

;;; Commentary:

;; appkit is a runtime layer for stateful Emacs buffer applications.  It owns
;; app/view lifecycle, mutation boundaries, invalidation scheduling, keyed
;; history projection, telega-style same-buffer chat input mechanics, media,
;; and reusable presentation geometry.  It deliberately does not own
;; application business objects, protocol adapters, or client branding.

;;; Code:

(require 'appkit-core)
(require 'appkit-transaction)
(require 'appkit-position)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'appkit-media)
(require 'appkit-ui)
(require 'appkit-view)
(require 'appkit-mode-line)
(require 'appkit-chat-avatar)
(require 'appkit-chat-ins)

(provide 'appkit)

;;; appkit.el ends here
