;;; appkit-markup-codecs.el --- Built-in safe chat markup codecs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0

;;; Commentary:

;; Local, hook-free parsers and canonical printers for plain text, a safe Org
;; chat subset, and chat Markdown.  These are deliberately smaller grammars than
;; full Org or CommonMark and claim only the semantics implemented here.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-element)
(require 'subr-x)
(require 'appkit-markup)
(require 'appkit-markup-codec)

(defun appkit-markup-codecs--add-style (children style)
  "Return inline CHILDREN with STYLE added to every style-bearing node."
  (mapcar
   (lambda (node)
     (cond
      ((appkit-markup-text-p node)
       (appkit-markup-text
        (appkit-markup-text-text node)
        (append (appkit-markup-text-styles node) (list style))))
      ((appkit-markup-link-p node)
       (appkit-markup-link
        (appkit-markup-link-url node)
        (appkit-markup-codecs--add-style
         (appkit-markup-link-children node) style)))
      ((appkit-markup-object-p node)
       (appkit-markup-object
        (appkit-markup-object-value node)
        (appkit-markup-object-fallback node)
        (append (appkit-markup-object-styles node) (list style))))
      (t node)))
   children))

(defun appkit-markup-codecs--escaped-p (text position)
  "Return non-nil when TEXT POSITION follows an odd backslash run."
  (let ((cursor (1- position))
        (count 0))
    (while (and (>= cursor 0) (= (aref text cursor) ?\\))
      (cl-incf count)
      (cl-decf cursor))
    (= (% count 2) 1)))

(defun appkit-markup-codecs--closing (text delimiter start)
  "Return the next unescaped DELIMITER in TEXT after START."
  (let ((position (+ start (length delimiter)))
        found)
    (while (and (not found)
                (setq position (string-match (regexp-quote delimiter)
                                             text position)))
      (if (appkit-markup-codecs--escaped-p text position)
          (setq position (+ position (length delimiter)))
        (setq found position)))
    found))

(defun appkit-markup-codecs--style-rules (_kind)
  "Return ordered inline delimiter rules for Markdown."
  '(("**" bold) ("~~" strike) ("`" code) ("*" italic)
    ("_" italic)))

(defun appkit-markup-codecs--unescape (text)
  "Remove one Markdown backslash escape layer from TEXT."
  (let ((position 0)
        (finish (length text))
        pieces)
    (while (< position finish)
      (if (and (= (aref text position) ?\\)
               (< (1+ position) finish))
          (progn
            (push (substring text (1+ position) (+ position 2)) pieces)
            (setq position (+ position 2)))
        (push (substring text position (1+ position)) pieces)
        (cl-incf position)))
    (apply #'concat (nreverse pieces))))

(defun appkit-markup-codecs--link-at (_kind text position)
  "Return `(END URL LABEL)' for a Markdown link at TEXT POSITION."
  (let ((label-end (appkit-markup-codecs--closing text "]" position)))
    (when (and label-end
               (< (1+ label-end) (length text))
               (= (aref text (1+ label-end)) ?\())
      (let ((cursor (+ label-end 2))
            (depth 0)
            destination-end)
        (while (and (< cursor (length text)) (not destination-end))
          (pcase (aref text cursor)
            (?\\ (setq cursor (+ cursor 2)))
            (?\( (cl-incf depth) (cl-incf cursor))
            (?\) (if (= depth 0)
                     (setq destination-end cursor)
                   (cl-decf depth)
                   (cl-incf cursor)))
            (_ (cl-incf cursor))))
        (when destination-end
          (list (1+ destination-end)
                (appkit-markup-codecs--unescape
                 (substring text (+ label-end 2) destination-end))
                (substring text (1+ position) label-end)))))))

(defun appkit-markup-codecs--parse-inlines (text kind)
  "Parse one property-free inline TEXT line using codec KIND."
  (let ((position 0)
        (finish (length text))
        (plain-start 0)
        result)
    (cl-labels
        ((emit-plain
          (end)
          (when (< plain-start end)
            (push (appkit-markup-text (substring text plain-start end)) result)))
         (emit-many
          (nodes)
          (dolist (node nodes) (push node result))))
      (while (< position finish)
        (let ((link (and (= (aref text position) ?\[)
                         (appkit-markup-codecs--link-at kind text position)))
              matched)
          (cond
           ((and (= (aref text position) ?\\) (< (1+ position) finish))
            (emit-plain position)
            (push (appkit-markup-text
                   (substring text (1+ position) (+ position 2)))
                  result)
            (setq position (+ position 2)
                  plain-start position))
           (link
            (emit-plain position)
            (pcase-let ((`(,end ,url ,label) link))
              (let ((label-nodes
                     (appkit-markup-codecs--parse-inlines label kind)))
                (if (cl-every #'appkit-markup-text-p label-nodes)
                    (push (appkit-markup-link url label-nodes) result)
                  (push (appkit-markup-link
                         url (list (appkit-markup-text label))) result)))
              (setq position end plain-start end)))
           (t
            (dolist (rule (appkit-markup-codecs--style-rules kind))
              (when (and (not matched)
                         (let ((delimiter (car rule)))
                           (and (<= (+ position (length delimiter)) finish)
                                (string-equal
                                 delimiter
                                 (substring text position
                                            (+ position (length delimiter)))))))
                (let* ((delimiter (car rule))
                       (style (cadr rule))
                       (close (appkit-markup-codecs--closing
                               text delimiter position)))
                  (when (and close (> close (+ position (length delimiter))))
                    (emit-plain position)
                    (let* ((content-start (+ position (length delimiter)))
                           (content (substring text content-start close))
                           (nodes
                            (if (eq style 'code)
                                (list (appkit-markup-text content '(code)))
                              (appkit-markup-codecs--add-style
                               (appkit-markup-codecs--parse-inlines content kind)
                               style))))
                      (emit-many nodes)
                      (setq position (+ close (length delimiter))
                            plain-start position
                            matched t))))))
            (unless matched
              (cl-incf position))))))
      (emit-plain finish))
    (nreverse result)))

(defun appkit-markup-codecs--blank-line-p (line)
  "Return non-nil when LINE contains only horizontal whitespace."
  (string-match-p "\\`[ \t]*\\'" line))

(defun appkit-markup-codecs--fence-open (_kind line)
  "Return `(DELIMITER LANGUAGE)' when LINE opens a Markdown code block."
  (when (string-match
         "\\`\\(```+\\)[ \t]*\\([^ `\t]*\\)[ \t]*\\'" line)
    (list (match-string 1 line)
          (let ((language (match-string 2 line)))
            (unless (string-empty-p language) language)))))

(defun appkit-markup-codecs--fence-close-p (_kind line delimiter)
  "Return non-nil when LINE closes Markdown DELIMITER."
  (string-match-p
   (concat "\\`" (regexp-quote delimiter) "[ \t]*\\'") line))

(defun appkit-markup-codecs--heading (_kind line)
  "Return `(LEVEL TEXT)' when LINE is a Markdown heading."
  (when (string-match "\\`\\(#\\{1,6\\}\\)[ \t]+\\(.+\\)\\'" line)
    (list (length (match-string 1 line)) (match-string 2 line))))

(defun appkit-markup-codecs--list-line (line)
  "Return `(INDENT STYLE START BODY)' when LINE is a list item."
  (when (string-match
         "\\`\\([ ]*\\)\\([-+*]\\|\\([0-9]+\\)[.)]\\)[ \t]+\\(.*\\)\\'"
         line)
    (list (length (match-string 1 line))
          (if (match-string 3 line) 'ordered 'unordered)
          (and (match-string 3 line) (string-to-number (match-string 3 line)))
          (match-string 4 line))))

(defun appkit-markup-codecs--block-start-p (kind line)
  "Return non-nil when LINE begins a non-paragraph Markdown block."
  (or (appkit-markup-codecs--blank-line-p line)
      (appkit-markup-codecs--fence-open kind line)
      (appkit-markup-codecs--heading kind line)
      (appkit-markup-codecs--list-line line)
      (string-match-p "\\`>[ \t]?" line)))

(defun appkit-markup-codecs--strip-indent (line amount)
  "Strip at most AMOUNT leading spaces from LINE."
  (let ((count 0))
    (while (and (< count (length line)) (< count amount)
                (= (aref line count) ?\s))
      (cl-incf count))
    (substring line count)))

(defun appkit-markup-codecs--paragraph (lines kind)
  "Return one paragraph parsing LINES with KIND."
  (let (children first)
    (dolist (line lines)
      (when first (push (appkit-markup-line-break) children))
      (setq first t)
      (dolist (node (appkit-markup-codecs--parse-inlines line kind))
        (push node children)))
    (appkit-markup-paragraph (nreverse children))))

(defun appkit-markup-codecs--blank-continues-list-p (lines index indent)
  "Return non-nil when blank LINES at INDEX lead to content below INDENT."
  (let ((count (length lines)))
    (while (and (< index count)
                (appkit-markup-codecs--blank-line-p (nth index lines)))
      (cl-incf index))
    (when (< index count)
      (let ((line (nth index lines)))
        (string-match "\\` *" line)
        (> (length (match-string 0 line)) indent)))))

(defun appkit-markup-codecs--parse-blocks (lines kind)
  "Parse property-free LINES into block values for KIND."
  (let ((index 0)
        (count (length lines))
        blocks)
    (while (< index count)
      (let ((line (nth index lines)))
        (cond
         ((appkit-markup-codecs--blank-line-p line)
          (cl-incf index))
         ((appkit-markup-codecs--fence-open kind line)
          (pcase-let* ((`(,delimiter ,language)
                        (appkit-markup-codecs--fence-open kind line))
                       (cursor (1+ index))
                       (body nil))
            (while (and (< cursor count)
                        (not (appkit-markup-codecs--fence-close-p
                              kind (nth cursor lines) delimiter)))
              (push (nth cursor lines) body)
              (cl-incf cursor))
            (if (< cursor count)
                (progn
                  (push (appkit-markup-preformatted
                         (mapconcat #'identity (nreverse body) "\n") language)
                        blocks)
                  (setq index (1+ cursor)))
              ;; Malformed fences are literal source, never partial markup.
              (push (appkit-markup-codecs--paragraph
                     (seq-subseq lines index count) kind)
                    blocks)
              (setq index count))))
         ((string-match-p "\\`>[ \t]?" line)
          (let ((cursor index) body)
            (while (and (< cursor count)
                        (string-match "\\`>[ \t]?\\(.*\\)\\'"
                                      (nth cursor lines)))
              (push (match-string 1 (nth cursor lines)) body)
              (cl-incf cursor))
            (push (appkit-markup-quote
                   (appkit-markup-codecs--parse-blocks (nreverse body) kind))
                  blocks)
            (setq index cursor)))
         ((appkit-markup-codecs--heading kind line)
          (pcase-let ((`(,level ,text)
                       (appkit-markup-codecs--heading kind line)))
            (push (appkit-markup-heading
                   level (appkit-markup-codecs--parse-inlines text kind))
                  blocks)
            (cl-incf index)))
         ((appkit-markup-codecs--list-line line)
          (pcase-let* ((`(,indent ,style ,start ,_body)
                        (appkit-markup-codecs--list-line line))
                       (cursor index)
                       (items nil))
            (while (and (< cursor count)
                        (let ((parsed
                               (appkit-markup-codecs--list-line
                                (nth cursor lines))))
                          (and parsed (= (nth 0 parsed) indent)
                               (eq (nth 1 parsed) style))))
              (let* ((parsed (appkit-markup-codecs--list-line
                              (nth cursor lines)))
                     (body (list (nth 3 parsed)))
                     (next (1+ cursor)))
                (while (and
                        (< next count)
                        (let* ((next-line (nth next lines))
                               (candidate
                                (appkit-markup-codecs--list-line next-line))
                               (leading
                                (progn
                                  (string-match "\\` *" next-line)
                                  (length (match-string 0 next-line)))))
                          (or (and (appkit-markup-codecs--blank-line-p next-line)
                                   (appkit-markup-codecs--blank-continues-list-p
                                    lines next indent))
                              (and candidate (> (nth 0 candidate) indent))
                              (> leading indent))))
                  (push (appkit-markup-codecs--strip-indent
                         (nth next lines) (+ indent 2))
                        body)
                  (cl-incf next))
                (push (appkit-markup-list-item
                       (appkit-markup-codecs--parse-blocks
                        (nreverse body) kind))
                      items)
                (setq cursor next)))
            (push (appkit-markup-list
                   style (nreverse items)
                   :start (and (eq style 'ordered) start))
                  blocks)
            (setq index cursor)))
         (t
          (let ((cursor index) paragraph)
            (while (and
                    (< cursor count)
                    (or (= cursor index)
                        (not (appkit-markup-codecs--block-start-p
                              kind (nth cursor lines)))
                        (cl-every
                         #'appkit-markup-codecs--blank-line-p
                         (nthcdr cursor lines))))
              (push (nth cursor lines) paragraph)
              (cl-incf cursor))
            (push (appkit-markup-codecs--paragraph
                   (nreverse paragraph) kind)
                  blocks)
            (setq index cursor))))))
    (nreverse blocks)))
(defun appkit-markup-codecs--source-lines (source)
  "Split normalized SOURCE while preserving every trailing empty line."
  (split-string source "\n" nil))


(defun appkit-markup-codecs--parse (source kind)
  "Parse SOURCE using built-in codec KIND."
  (let* ((plain (substring-no-properties source))
         (plain (replace-regexp-in-string "\r\n?" "\n" plain t t))
         (lines (appkit-markup-codecs--source-lines plain)))
    (appkit-markup-parse-result
     (appkit-markup-document
      (if (string-empty-p plain)
          nil
        (appkit-markup-codecs--parse-blocks lines kind))))))

(defun appkit-markup-codecs--plain-parse (source _context)
  "Parse literal SOURCE without interpreting formatting syntax."
  (let* ((plain (substring-no-properties source))
         (plain (replace-regexp-in-string "\r\n?" "\n" plain t t))
         children)
    (cl-loop for part in (appkit-markup-codecs--source-lines plain)
             for first = t then nil
             unless first do (push (appkit-markup-line-break) children)
             unless (string-empty-p part)
             do (push (appkit-markup-text part) children))
    (appkit-markup-parse-result
     (appkit-markup-document
      (if children
          (list (appkit-markup-paragraph (nreverse children)))
        nil)))))

(defun appkit-markup-codecs--escape-matches (text regexp)
  "Backslash every REGEXP match in TEXT."
  (replace-regexp-in-string
   regexp (lambda (match) (concat "\\" match)) text t t))

(defun appkit-markup-codecs--escape (text kind)
  "Escape literal TEXT for built-in codec KIND."
  (appkit-markup-codecs--escape-matches
   text (if (eq kind 'org) "[][\\*_/+=~]" "[][\\`*_~]")))

(defun appkit-markup-codecs--markdown-escape-block-start (text)
  "Escape a Markdown block marker at the start of literal TEXT."
  (let ((position
         (cond
          ((string-match-p
            "\\`\\(?:#\\{1,6\\}[ \t]+\\|>[ \t]?\\|```\\)" text)
           0)
          ((string-match
            "\\`\\( *\\)\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+" text)
           (length (match-string 1 text))))))
    (if position
        (concat (substring text 0 position) "\\"
                (substring text position))
      text)))

(defun appkit-markup-codecs--markdown-escape-url (url)
  "Escape Markdown destination delimiters in URL."
  (let ((position 0)
        (start 0)
        (finish (length url))
        pieces)
    (while (< position finish)
      (let ((character (aref url position)))
        (when (memq character '(?\\ ?\( ?\)))
          (when (< start position)
            (push (substring url start position) pieces))
          (push (string ?\\ character) pieces)
          (setq start (1+ position))))
      (cl-incf position))
    (if (null pieces)
        url
      (when (< start finish)
        (push (substring url start) pieces))
      (apply #'concat (nreverse pieces)))))

(defun appkit-markup-codecs--style-delimiter (kind style)
  "Return delimiter for KIND STYLE, or nil when unsupported."
  (alist-get style
             (if (eq kind 'org)
                 '((bold . "*") (italic . "/") (underline . "_")
                   (strike . "+") (code . "~"))
               '((bold . "**") (italic . "*") (strike . "~~")
                 (code . "`")))))

(defun appkit-markup-codecs--print-inlines (children kind path losses)
  "Print inline CHILDREN for KIND at PATH, mutating LOSSES list cell."
  (let ((line-start-p t)
        result)
    (dolist (node children)
      (let
          ((encoded
            (cond
             ((appkit-markup-text-p node)
              (let* ((styles (appkit-markup-text-styles node))
                     (text (appkit-markup-text-text node))
                     (encoded (appkit-markup-codecs--escape text kind)))
                (when (and line-start-p (eq kind 'markdown) (null styles))
                  (setq encoded
                        (appkit-markup-codecs--markdown-escape-block-start
                         encoded)))
                (dolist (style (reverse styles))
                  (if-let* ((delimiter
                             (appkit-markup-codecs--style-delimiter
                              kind style)))
                      (if (and (eq style 'code)
                               (string-match-p
                                (regexp-quote delimiter) text))
                          (push (appkit-markup-loss style path) (car losses))
                        (setq encoded
                              (concat delimiter
                                      (if (eq style 'code) text encoded)
                                      delimiter)))
                    (push (appkit-markup-loss style path) (car losses))))
                encoded))
             ((appkit-markup-link-p node)
              (let ((label
                     (appkit-markup-codecs--print-inlines
                      (appkit-markup-link-children node) kind
                      (append path '(label)) losses))
                    (url (appkit-markup-link-url node)))
                (if (eq kind 'org)
                    (format "[[%s][%s]]"
                            (appkit-markup-codecs--escape-matches url "[]\\]")
                            label)
                  (format "[%s](%s)" label
                          (appkit-markup-codecs--markdown-escape-url url)))))
             ((appkit-markup-object-p node)
              (push (appkit-markup-loss 'object path) (car losses))
              (appkit-markup-codecs--print-inlines
               (appkit-markup-object-fallback node) kind
               (append path '(fallback)) losses))
             ((appkit-markup-line-break-p node) "\n")
             (t ""))))
        (push encoded result)
        (setq line-start-p
              (if (string-empty-p encoded)
                  line-start-p
                (= (aref encoded (1- (length encoded))) ?\n)))))
    (apply #'concat (nreverse result))))

(defun appkit-markup-codecs--prefix-lines (text first rest)
  "Prefix TEXT with FIRST and subsequent lines with REST."
  (let ((lines (split-string text "\n" nil)) first-line)
    (mapconcat
     (lambda (line)
       (prog1 (concat (if first-line rest first) line)
         (setq first-line t)))
     lines "\n")))

(defun appkit-markup-codecs--print-blocks (blocks kind path losses)
  "Print BLOCKS for KIND at PATH, mutating LOSSES list cell."
  (let (result)
    (cl-loop for block in blocks
             for index from 0
             for node-path = (append path (list index))
             do
             (push
              (cond
               ((appkit-markup-paragraph-p block)
                (appkit-markup-codecs--print-inlines
                 (appkit-markup-paragraph-children block)
                 kind (append node-path '(children)) losses))
               ((appkit-markup-heading-p block)
                (concat
                 (make-string (appkit-markup-heading-level block)
                              (if (eq kind 'org) ?* ?#))
                 " "
                 (appkit-markup-codecs--print-inlines
                  (appkit-markup-heading-children block)
                  kind (append node-path '(children)) losses)))
               ((appkit-markup-quote-p block)
                (let ((body
                       (appkit-markup-codecs--print-blocks
                        (appkit-markup-quote-blocks block) kind
                        (append node-path '(blocks)) losses)))
                  (if (eq kind 'org)
                      (concat "#+begin_quote\n" body "\n#+end_quote")
                    (appkit-markup-codecs--prefix-lines body "> " "> "))))
               ((appkit-markup-list-p block)
                (let ((number (or (appkit-markup-list-start block) 1))
                      items)
                  (cl-loop
                   for item in (appkit-markup-list-items block)
                   for item-index from 0
                   do
                   (let* ((marker
                           (if (eq (appkit-markup-list-style block) 'ordered)
                               (prog1 (format "%d. " number) (cl-incf number))
                             "- "))
                          (body
                           (appkit-markup-codecs--print-blocks
                            (appkit-markup-list-item-blocks item) kind
                            (append node-path (list 'items item-index 'blocks))
                            losses)))
                     (push (appkit-markup-codecs--prefix-lines
                            body marker (make-string (length marker) ?\s))
                           items)))
                  (mapconcat #'identity (nreverse items) "\n")))
               ((appkit-markup-preformatted-p block)
                (let ((text (appkit-markup-preformatted-text block))
                      (language (appkit-markup-preformatted-language block)))
                  (if (eq kind 'org)
                      (if (string-match-p
                           "^[ \t]*#[+]end_src[ \t]*$" (downcase text))
                          (progn
                            (push (appkit-markup-loss 'preformatted node-path)
                                  (car losses))
                            text)
                        (concat "#+begin_src"
                                (if language (concat " " language) "")
                                "\n" text "\n#+end_src"))
                    (let* ((runs
                            (mapcar #'length
                                    (split-string text "[^`]+" t)))
                           (width (max 3 (1+ (if runs (apply #'max runs) 0))))
                           (fence (make-string width ?`)))
                      (concat fence (or language "") "\n"
                              text "\n" fence)))))
               ((appkit-markup-object-block-p block)
                (push (appkit-markup-loss 'object-block node-path)
                      (car losses))
                (appkit-markup-codecs--print-blocks
                 (appkit-markup-object-block-fallback block) kind
                 (append node-path '(fallback)) losses))
               (t ""))
              result))
    (mapconcat #'identity (nreverse result) "\n\n")))

(defun appkit-markup-codecs--print (document kind)
  "Print DOCUMENT canonically with built-in codec KIND."
  (let ((losses (list nil)))
    (appkit-markup-print-result
     (appkit-markup-codecs--print-blocks
      (appkit-markup-document-blocks document) kind '(blocks) losses)
     (nreverse (car losses)))))

(defun appkit-markup-codecs--plain-losses (document)
  "Return losses incurred by plain export of DOCUMENT."
  (let (losses)
    (cl-labels
        ((inlines
          (nodes path)
          (cl-loop for node in nodes for index from 0
                   for here = (append path (list index)) do
                   (cond
                    ((appkit-markup-text-p node)
                     (dolist (style (appkit-markup-text-styles node))
                       (push (appkit-markup-loss style here) losses)))
                    ((appkit-markup-link-p node)
                     (push (appkit-markup-loss 'link here) losses)
                     (inlines (appkit-markup-link-children node)
                              (append here '(label))))
                    ((appkit-markup-object-p node)
                     (push (appkit-markup-loss 'object here) losses)
                     (inlines (appkit-markup-object-fallback node)
                              (append here '(fallback)))))))
         (blocks
          (nodes path)
          (when (cdr nodes)
            (push (appkit-markup-loss 'block-boundary path) losses))
          (cl-loop for node in nodes for index from 0
                   for here = (append path (list index)) do
                   (cond
                    ((appkit-markup-paragraph-p node)
                     (inlines (appkit-markup-paragraph-children node)
                              (append here '(children))))
                    ((appkit-markup-heading-p node)
                     (push (appkit-markup-loss 'heading here) losses)
                     (inlines (appkit-markup-heading-children node)
                              (append here '(children))))
                    ((appkit-markup-quote-p node)
                     (push (appkit-markup-loss 'quote here) losses)
                     (blocks (appkit-markup-quote-blocks node)
                             (append here '(blocks))))
                    ((appkit-markup-list-p node)

                     (push (appkit-markup-loss 'list here) losses))
                    ((appkit-markup-preformatted-p node)
                     (push (appkit-markup-loss 'preformatted here) losses))
                    ((appkit-markup-object-block-p node)
                     (push (appkit-markup-loss 'object-block here) losses))))))
      (blocks (appkit-markup-document-blocks document) '(blocks)))
    (nreverse losses)))

(defun appkit-markup-codecs--plain-print (document _context)
  "Print DOCUMENT as semantic plain text with explicit loss records."
  (appkit-markup-print-result
   (appkit-markup-plain-text document)
   (appkit-markup-codecs--plain-losses document)))

(defvar appkit-markup-codecs--org-diagnostics nil
  "Dynamically collected hook-free Org parse diagnostics.")

(defun appkit-markup-codecs--org-position (node property)
  "Return zero-based source PROPERTY position for Org NODE."
  (max 0 (1- (or (org-element-property property node) 1))))

(defun appkit-markup-codecs--org-diagnostic (kind node)
  "Record unsupported Org NODE as diagnostic KIND."
  (push (appkit-markup-diagnostic
         kind 'warning
         (appkit-markup-codecs--org-position node :begin)
         (appkit-markup-codecs--org-position node :end))
        appkit-markup-codecs--org-diagnostics))

(defun appkit-markup-codecs--org-string-inlines (text)
  "Convert Org plain TEXT to semantic inline nodes."
  (let ((position 0)
        (finish (length text))
        result)
    (while (< position finish)
      (let ((newline (string-match "\n" text position)))
        (if newline
            (progn
              (when (< position newline)
                (push (appkit-markup-text (substring text position newline))
                      result))
              (push (appkit-markup-line-break) result)
              (setq position (1+ newline)))
          (push (appkit-markup-text (substring text position)) result)
          (setq position finish))))
    (nreverse result)))

(defun appkit-markup-codecs--org-inlines (contents)
  "Convert Org inline CONTENTS to semantic inline nodes."
  (let (result)
    (dolist (node contents (nreverse result))
      (cond
       ((stringp node)
        (dolist (inline (appkit-markup-codecs--org-string-inlines
                         (substring-no-properties node)))
          (push inline result)))
       ((memq (org-element-type node)
              '(bold italic underline strike-through))
        (let ((style (pcase (org-element-type node)
                       ('strike-through 'strike)
                       (other other))))
          (dolist
              (inline
               (appkit-markup-codecs--add-style
                (appkit-markup-codecs--org-inlines
                 (org-element-contents node))
                style))
            (push inline result))))
       ((memq (org-element-type node) '(code verbatim))
        (push (appkit-markup-text
               (or (org-element-property :value node) "") '(code))
              result))
       ((eq (org-element-type node) 'link)
        (let* ((url (or (org-element-property :raw-link node)
                        (org-element-property :path node)
                        ""))
               (description
                (appkit-markup-codecs--org-inlines
                 (org-element-contents node)))
               (description
                (if (and description
                         (cl-every #'appkit-markup-text-p description))
                    description
                  (list (appkit-markup-text url)))))
          (push (appkit-markup-link url description) result)))
       ((eq (org-element-type node) 'line-break)
        (push (appkit-markup-line-break) result))
       (t
        (appkit-markup-codecs--org-diagnostic
         'unsupported-org-inline node)
        (let ((visible (substring-no-properties
                        (org-element-interpret-data node))))
          (unless (string-empty-p visible)
            (push (appkit-markup-text
                   (replace-regexp-in-string "[\r\n]+" " " visible))
                  result)))))
      (when (and (not (stringp node))
                 (> (or (org-element-property :post-blank node) 0) 0))
        (push (appkit-markup-text
               (make-string (org-element-property :post-blank node) ?\s))
              result)))))
(defun appkit-markup-codecs--org-paragraph (node)
  "Convert Org paragraph NODE."
  (let ((children
         (appkit-markup-codecs--org-inlines
          (org-element-contents node))))
    ;; Org includes the source line terminator in paragraph strings.  The block
    ;; boundary owns one terminator; explicit internal newlines remain.
    (when (and children (appkit-markup-line-break-p (car (last children))))
      (setq children (butlast children)))
    (appkit-markup-paragraph children)))

(defun appkit-markup-codecs--org-literal-block (node)
  "Return literal paragraph fallback for unsupported Org NODE."
  (appkit-markup-codecs--org-diagnostic 'unsupported-org-block node)
  (let* ((begin (or (org-element-property :begin node) 1))
         (end (or (org-element-property :end node) begin))
         (text (buffer-substring-no-properties begin (min end (point-max))))
         (text (string-remove-suffix "\n" text)))
    (appkit-markup-paragraph
     (appkit-markup-codecs--org-string-inlines text))))

(defun appkit-markup-codecs--org-blocks (contents)
  "Convert Org block CONTENTS to semantic blocks."
  (let (blocks)
    (dolist (node contents (nreverse blocks))
      (pcase (org-element-type node)
        ('section
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        ('headline
         (push (appkit-markup-heading
                (min 6 (org-element-property :level node))
                (appkit-markup-codecs--org-inlines
                 (org-element-property :title node)))
               blocks)
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        ('paragraph
         (push (appkit-markup-codecs--org-paragraph node) blocks))
        ('quote-block
         (push (appkit-markup-quote
                (appkit-markup-codecs--org-blocks
                 (org-element-contents node)))
               blocks))
        ('plain-list
         (let* ((style
                 (if (eq (org-element-property :type node) 'ordered)
                     'ordered
                   'unordered))
                (items
                 (mapcar
                  (lambda (item)
                    (appkit-markup-list-item
                     (appkit-markup-codecs--org-blocks
                      (org-element-contents item))))
                  (seq-filter
                   (lambda (child)
                     (and (not (stringp child))
                          (eq (org-element-type child) 'item)))
                   (org-element-contents node))))
                (bullet
                 (and items
                      (org-element-property
                       :bullet
                       (seq-find
                        (lambda (child)
                          (and (not (stringp child))
                               (eq (org-element-type child) 'item)))
                        (org-element-contents node)))))
                (start
                 (and (eq style 'ordered)
                      (stringp bullet)
                      (string-match "[0-9]+" bullet)
                      (string-to-number (match-string 0 bullet)))))
           (push (appkit-markup-list style items :start start) blocks)))
        ('src-block
         (push (appkit-markup-preformatted
                (string-remove-suffix
                 "\n" (or (org-element-property :value node) ""))
                (org-element-property :language node))
               blocks))
        ((or 'org-data 'item)
         (dolist (block
                  (appkit-markup-codecs--org-blocks
                   (org-element-contents node)))
           (push block blocks)))
        (_
         (push (appkit-markup-codecs--org-literal-block node) blocks))))))

(defun appkit-markup-codecs--org-element-parse (source)
  "Parse SOURCE through hook-free built-in Org Element semantics."
  (let (appkit-markup-codecs--org-diagnostics)
    (with-temp-buffer
      (insert (substring-no-properties source))
      (let ((org-element-use-cache nil)
            (org-inhibit-startup t)
            (org-use-property-inheritance nil))
        (let ((document
               (appkit-markup-document
                (appkit-markup-codecs--org-blocks
                 (org-element-contents (org-element-parse-buffer))))))
          (appkit-markup-parse-result
           document
           :diagnostics
           (nreverse appkit-markup-codecs--org-diagnostics)))))))
(defun appkit-markup-codecs--org-parse (source _context)
  "Parse Org SOURCE with hook-free built-in Org Element semantics."
  (appkit-markup-codecs--org-element-parse source))

(defun appkit-markup-codecs--markdown-parse (source _context)
  "Parse chat Markdown SOURCE."
  (appkit-markup-codecs--parse source 'markdown))

(defun appkit-markup-codecs--org-print (document _context)
  "Print DOCUMENT as canonical safe Org chat source."
  (appkit-markup-codecs--print document 'org))

(defun appkit-markup-codecs--markdown-print (document _context)
  "Print DOCUMENT as canonical chat Markdown source."
  (appkit-markup-codecs--print document 'markdown))

(defun appkit-markup-codecs--wrap-edit
    (source start end opening closing)
  "Wrap SOURCE START..END in OPENING and CLOSING."
  (let ((edited (concat (substring source 0 start)
                        opening
                        (substring source start end)
                        closing
                        (substring source end))))
    (if (= start end)
        (appkit-markup-edit-result
         edited (+ start (length opening)) (+ start (length opening)))
      (appkit-markup-edit-result
       edited (+ start (length opening)) (+ end (length opening))))))

(defun appkit-markup-codecs--line-edit
    (source start end first-prefix rest-prefix)
  "Prefix every source line touched by START..END."
  (let* ((line-start
          (if-let* ((newline
                     (cl-position ?\n source :end start :from-end t)))
              (1+ newline)
            0))
         (line-end (or (cl-position ?\n source :start end)
                       (length source)))
         (lines (split-string (substring source line-start line-end) "\n" nil))
         (first t)
         (index 0)
         (replacement
          (mapconcat
           (lambda (line)
             (let* ((prefix (if first first-prefix rest-prefix))
                    (prefix (if (functionp prefix)
                                (funcall prefix index)
                              prefix)))
               (prog1 (concat prefix line)
                 (setq first nil)
                 (cl-incf index))))
           lines "\n"))
         (edited (concat (substring source 0 line-start)
                         replacement
                         (substring source line-end))))
    (appkit-markup-edit-result
     edited line-start (+ line-start (length replacement)))))

(defun appkit-markup-codecs--edit
    (kind source operation start end data _context)
  "Apply KIND source OPERATION to SOURCE START..END."
  (pcase operation
    ((or 'bold 'italic 'underline 'strike 'code)
     (if-let* ((delimiter
                (appkit-markup-codecs--style-delimiter kind operation)))
         (appkit-markup-codecs--wrap-edit
          source start end delimiter delimiter)
       (signal 'appkit-markup-codec-error '(unsupported-source-edit))))
    ('link
     (unless (and (stringp data)
                  (not (string-empty-p data))
                  (not (string-match-p "[\r\n]" data)))
       (signal 'appkit-markup-codec-error '(invalid-link-edit)))
     (if (eq kind 'org)
         (appkit-markup-codecs--wrap-edit
          source start end (concat "[[" data "][") "]]")
       (appkit-markup-codecs--wrap-edit
        source start end "[" (concat "](" data ")"))))
    ('quote
     (if (eq kind 'org)
         (appkit-markup-codecs--wrap-edit
          source start end "#+begin_quote\n" "\n#+end_quote")
       (appkit-markup-codecs--line-edit source start end "> " "> ")))
    ('unordered-list
     (appkit-markup-codecs--line-edit source start end "- " "- "))
    ('ordered-list
     (appkit-markup-codecs--line-edit
      source start end
      (lambda (index) (format "%d. " (1+ index)))
      (lambda (index) (format "%d. " (1+ index)))))
    ('heading
     (unless (and (integerp data) (<= 1 data 6))
       (signal 'appkit-markup-codec-error '(invalid-heading-edit)))
     (appkit-markup-codecs--line-edit
      source start end
      (concat (make-string data (if (eq kind 'org) ?* ?#)) " ")
      ""))
    ('preformatted
     (let ((language
            (cond ((null data) nil)
                  ((and (stringp data)
                        (not (string-empty-p data))
                        (not (string-match-p "[\r\n \t]" data)))
                   data)
                  (t (signal 'appkit-markup-codec-error
                             '(invalid-code-language))))))
       (if (eq kind 'org)
           (appkit-markup-codecs--wrap-edit
            source start end
            (concat "#+begin_src"
                    (if language (concat " " language) "")
                    "\n")
            "\n#+end_src")
         (appkit-markup-codecs--wrap-edit
          source start end
          (concat "```" (or language "") "\n")
          "\n```"))))
    (_ (signal 'appkit-markup-codec-error '(unsupported-source-edit)))))

(defun appkit-markup-codecs--org-edit
    (source operation start end data context)
  "Apply safe Org source edit OPERATION."
  (appkit-markup-codecs--edit
   'org source operation start end data context))

(defun appkit-markup-codecs--markdown-edit
    (source operation start end data context)
  "Apply Markdown source edit OPERATION."
  (appkit-markup-codecs--edit
   'markdown source operation start end data context))

(appkit-markup-register-codec
 'plain
 :label "Plain text"
 :parse #'appkit-markup-codecs--plain-parse
 :print #'appkit-markup-codecs--plain-print)

(appkit-markup-register-codec
 'org
 :label "Org"
 :parse #'appkit-markup-codecs--org-parse
 :print #'appkit-markup-codecs--org-print
 :edit #'appkit-markup-codecs--org-edit
 :capabilities '(heading bold italic underline strike code link quote list
                  preformatted))

(appkit-markup-register-codec
 'markdown
 :label "Markdown"
 :parse #'appkit-markup-codecs--markdown-parse
 :print #'appkit-markup-codecs--markdown-print
 :edit #'appkit-markup-codecs--markdown-edit
 :capabilities '(heading bold italic strike code link quote list preformatted))

(provide 'appkit-markup-codecs)

;;; appkit-markup-codecs.el ends here
