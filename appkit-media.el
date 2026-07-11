;;; appkit-media.el --- Shared media runtime -*- lexical-binding: t; -*-

;;; Commentary:

;; Aggregates protocol-neutral media card, image, resource, and video
;; primitives.  Applications remain responsible for adapting backend objects
;; and for owning transfer/cache state visible in their user interfaces.

;;; Code:

(require 'appkit-media-card)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-media-video)

(provide 'appkit-media)

;;; appkit-media.el ends here
